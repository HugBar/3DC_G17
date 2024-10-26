using System;
using System.Threading.Tasks;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Identity;
using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using Microsoft.Extensions.Configuration;
using Microsoft.IdentityModel.Tokens;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Extensions.Logging;


namespace DDDSample1.Domain.Auth
{
    public class AuthService : IAuthService
    {
        private readonly UserManager<ApplicationUser> _userManager;
        private readonly SignInManager<ApplicationUser> _signInManager;
        private readonly IConfiguration _configuration;
        private readonly IEmailService _emailService;
        private readonly ILogger<AuthService> _logger;

        public AuthService(
            UserManager<ApplicationUser> userManager,
            SignInManager<ApplicationUser> signInManager,
            IConfiguration configuration,
            IEmailService emailService,
            ILogger<AuthService> logger)
        {
            _userManager = userManager;
            _signInManager = signInManager;
            _configuration = configuration;
            _emailService = emailService;
            _logger = logger;
        }

        public async Task<string> LoginAsync(LoginDto model)
        {
            
            var user = await _userManager.FindByEmailAsync(model.Email);

            if (user == null)
            {
                throw new InvalidOperationException("User not found.");
            }

            var result = await _signInManager.PasswordSignInAsync(user.UserName, model.Password, false, true);  

            if (result.Succeeded)
            {
                var user1 = await _userManager.FindByEmailAsync(model.Email);
                var token = await GenerateJwtToken(user);
                return token;
            }
            if (result.IsLockedOut)
            {
                await _emailService.SendAdminLockoutNotification(user.Email);
                throw new InvalidOperationException("User account locked out.");
            }

            throw new InvalidOperationException("Invalid login attempt.");
        }

        public async Task<bool> LogoutAsync()
        {
            await _signInManager.SignOutAsync();
            return true;
        }

        

        public async Task<bool> ChangePasswordAsync(string userId, string currentPassword, string newPassword)
        {
            var user = await _userManager.FindByIdAsync(userId);
            if (user == null)
            {
                throw new InvalidOperationException("User not found.");
            }

            var result = await _userManager.ChangePasswordAsync(user, currentPassword, newPassword);
            return result.Succeeded;
        }
        public async Task<string> GenerateJwtToken(ApplicationUser user)
        {
            var userRoles = await _userManager.GetRolesAsync(user);

            var claims = new List<Claim>
        {
            new Claim(JwtRegisteredClaimNames.Sub, user.UserName),
            new Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()),
            new Claim(JwtRegisteredClaimNames.Email, user.Email),
            new Claim(ClaimTypes.NameIdentifier, user.Id)
        };

            // Add the roles as claims
            claims.AddRange(userRoles.Select(role => new Claim(ClaimTypes.Role, role)));

            var key = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(_configuration["Jwt:Key"]));
            var creds = new SigningCredentials(key, SecurityAlgorithms.HmacSha256);

            var token = new JwtSecurityToken(
                issuer: _configuration["Jwt:Issuer"],
                audience: _configuration["Jwt:Audience"],
                claims: claims,
                expires: DateTime.Now.AddHours(3),
                signingCredentials: creds
            );

            return new JwtSecurityTokenHandler().WriteToken(token);
        }
         public async Task<bool> ResetPasswordAsync(string email)
        {
            var user = await _userManager.FindByEmailAsync(email);
            
            // Verifica se o usuário existe e se está nas roles apropriadas (Backoffice User)
            if (user == null || !(await _userManager.IsInRoleAsync(user, "Admin") 
                                || await _userManager.IsInRoleAsync(user, "Doctor")
                                || await _userManager.IsInRoleAsync(user, "Nurse")
                                || await _userManager.IsInRoleAsync(user, "Technician")
                                || await _userManager.IsInRoleAsync(user, "Staff")))
                                
            {
                throw new InvalidOperationException("User not found or not authorized.");
            }
                        // Check if account is locked
            if (await _userManager.IsLockedOutAsync(user))
            {
                throw new InvalidOperationException("Account is locked. Please contact administrator.");
            }
            
            // Check for existing reset tokens and invalidate them
            await _userManager.RemoveAuthenticationTokenAsync(user, "Default", "ResetPassword");

            // Gera o token de redefinição de senha
            var token = await _userManager.GeneratePasswordResetTokenAsync(user);

            // Cria um link de redefinição de senha com o token e e-mail
            var resetLink = $"https://localhost:5001/api/auth/reset-password?token={Uri.EscapeDataString(token)}&email={Uri.EscapeDataString(email)}";

             try
            {
            string subject = "Password Reset Request";
                string body = $@"Dear {user.UserName},

                Please reset your password by clicking the link below:
                {resetLink}

                This link will expire in 24 hours.

                If you didn't request this password reset, please ignore this email or contact support if you have concerns.

                Best regards,
                Your Application Team";

            await _emailService.SendEmailAsync(user.Email, subject, body);

            return true;

            }
            catch (Exception ex)
            {
                _logger.LogError($"Failed to send password reset email: {ex.Message}");
                throw new InvalidOperationException("Failed to send password reset email.");
            }
        }
        public async Task<bool> ConfirmResetPasswordAsync(string email, string token, string newPassword)
        {
            var user = await _userManager.FindByEmailAsync(email);
            if (user == null)
            {
                throw new InvalidOperationException("User not found.");
            }
            var isSamePassword = await _userManager.CheckPasswordAsync(user, newPassword);
            if (isSamePassword)
            {
                throw new InvalidOperationException("New password cannot be the same as the previous password.");
            }
                    // Validate password requirements
            var validators = _userManager.PasswordValidators;
            foreach (var validator in validators)
            {
                var validationResult = await validator.ValidateAsync(_userManager, user, newPassword);
                if (!validationResult.Succeeded)
                {
                    var errors = string.Join(", ", validationResult.Errors.Select(e => e.Description));
                    throw new InvalidOperationException($"Password validation failed: {errors}");
                }
            }

            // Additional custom password validations
            if (string.IsNullOrWhiteSpace(newPassword))
            {
                throw new InvalidOperationException("Password cannot be empty or whitespace.");
            }
            
            if (newPassword.Length < 8)
            {
                throw new InvalidOperationException("Password must be at least 8 characters long.");
            }

            if (!newPassword.Any(char.IsUpper))
            {
                throw new InvalidOperationException("Password must contain at least one uppercase letter.");
            }

            if (!newPassword.Any(char.IsLower))
            {
                throw new InvalidOperationException("Password must contain at least one lowercase letter.");
            }

            if (!newPassword.Any(char.IsDigit))
            {
                throw new InvalidOperationException("Password must contain at least one number.");
            }

            if (!newPassword.Any(ch => !char.IsLetterOrDigit(ch)))
            {
                throw new InvalidOperationException("Password must contain at least one special character.");
            }

            // Decodificar o token recebido do Postman
            var decodedToken = Uri.UnescapeDataString(token);

            // Tentar redefinir a senha com o token decodificado
            var resetResult = await _userManager.ResetPasswordAsync(user, decodedToken, newPassword);
            if (!resetResult.Succeeded)
            {
                var errors = string.Join(", ", resetResult.Errors.Select(e => e.Description));
                throw new Exception($"Password reset failed. Errors: {errors}");
            }

            return true;
        }




    }
}
