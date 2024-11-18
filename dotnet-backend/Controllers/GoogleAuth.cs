using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using System.Linq;
using System.Security.Claims;
using DDDSample1.Domain.UserData;
using Microsoft.Extensions.Logging;
using Microsoft.AspNetCore.Authentication.Google;
using System;
using System.IdentityModel.Tokens.Jwt;
using Microsoft.IdentityModel.Tokens;
using System.Text;
using System.Collections.Generic;
using Microsoft.Extensions.Configuration;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class GoogleAuthController : Controller
    {
        private readonly UserManager<ApplicationUser> _userManager;
        private readonly SignInManager<ApplicationUser> _signInManager;
        private readonly ILogger<GoogleAuthController> _logger;
        private readonly IConfiguration _configuration;

        public GoogleAuthController(
            UserManager<ApplicationUser> userManager, 
            SignInManager<ApplicationUser> signInManager, 
            ILogger<GoogleAuthController> logger,
            IConfiguration configuration)
        {
            _userManager = userManager;
            _signInManager = signInManager;
            _logger = logger;
            _configuration = configuration;
        }

        [HttpGet("google-login")]
        public IActionResult GoogleLogin()
        {
            _logger.LogInformation("Initiating Google login for patient...");
            var properties = new AuthenticationProperties { 
                RedirectUri = Url.Action("GoogleResponse"),
                Items = { { "role", "Patient" } }
            };
            return Challenge(properties, GoogleDefaults.AuthenticationScheme);
        }

        [HttpGet("google-response")]
        public async Task<IActionResult> GoogleResponse()
        {
            try
            {
                var authenticateResult = await HttpContext.AuthenticateAsync(IdentityConstants.ExternalScheme);
                if (!authenticateResult.Succeeded)
                {
                    return BadRequest("Google authentication failed");
                }

                var emailClaim = authenticateResult.Principal.FindFirst(ClaimTypes.Email)?.Value;
                var nameClaim = authenticateResult.Principal.FindFirst(ClaimTypes.Name)?.Value;

                var user = await _userManager.FindByEmailAsync(emailClaim);
                if (user == null)
                {
                    user = new ApplicationUser { 
                        UserName = emailClaim, 
                        Email = emailClaim,
                        EmailConfirmed = true 
                    };
                    var result = await _userManager.CreateAsync(user);
                    if (!result.Succeeded)
                    {
                        return BadRequest(result.Errors);
                    }
                }

                if (!await _userManager.IsInRoleAsync(user, "Patient"))
                {
                    var roleResult = await _userManager.AddToRoleAsync(user, "Patient");
                    if (!roleResult.Succeeded)
                    {
                        return BadRequest("Failed to assign Patient role");
                    }
                }

                // Generate JWT token for the patient
                var token = GenerateJwtToken(user);

                // Return token in a way that can be captured by the frontend
                return Redirect($"http://localhost:3000/auth/callback?token={token}");
            }
            catch (Exception ex)
            {
                _logger.LogError($"Error during Google authentication: {ex.Message}");
                return BadRequest($"Authentication failed: {ex.Message}");
            }
        }

        private string GenerateJwtToken(ApplicationUser user)
        {
            var claims = new List<Claim>
            {
                new Claim(JwtRegisteredClaimNames.Sub, user.Id),
                new Claim(JwtRegisteredClaimNames.Email, user.Email),
                new Claim(ClaimTypes.Role, "Patient"),
                new Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString())
            };

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
    }
}
