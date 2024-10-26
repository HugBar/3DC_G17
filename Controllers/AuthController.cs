using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Auth;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using System.Security.Claims;
using Microsoft.AspNetCore.Identity;
using DDDSample1.Domain.UserData;


namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class AuthController : ControllerBase
    {
        private readonly IAuthService _authService;
        private readonly ILogger<AuthController> _logger;
        private readonly UserManager<ApplicationUser> _userManager;

        public AuthController(IAuthService authService, ILogger<AuthController> logger, UserManager<ApplicationUser> userManager)
        {
            _authService = authService;
            _logger = logger;
            _userManager = userManager;
        }

        [HttpPost("login")]
        public async Task<IActionResult> Login([FromBody] LoginDto model)
        {
            try
            {
                var token = await _authService.LoginAsync(model);
                return Ok(new { Token = token });
            }
            catch (InvalidOperationException ex)
            {
                return Unauthorized(ex.Message);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred during login");
                return StatusCode(500, "An error occurred while processing your request.");
            }
        }

        [Authorize]
        [HttpPost("logout")]
        public async Task<IActionResult> Logout()
        {
            await _authService.LogoutAsync();
            return Ok();
        }

        [Authorize]
        [HttpPost("change-password")]
        public async Task<IActionResult> ChangePassword([FromBody] ChangePasswordDto model)
        {
            try
            {
                var userId = User.FindFirst(ClaimTypes.NameIdentifier)?.Value;
                var result = await _authService.ChangePasswordAsync(userId, model.CurrentPassword, model.NewPassword);
                if (result)
                {
                    return Ok("Password changed successfully.");
                }
                return BadRequest("Failed to change password.");
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred during password change");
                return StatusCode(500, "An error occurred while processing your request.");
            }
        }

        [HttpPost("reset-password")]
        public async Task<IActionResult> ResetPassword([FromBody] ResetPasswordDto model)
        {
            try
            {
                // Input validation
                if (!ModelState.IsValid)
                {
                    return BadRequest(ModelState);
                }

                // Email format validation
                if (string.IsNullOrEmpty(model.Email))
                {
                    return BadRequest("Email is required.");
                }

                var emailRegex = new System.Text.RegularExpressions.Regex(@"^[^@\s]+@[^@\s]+\.[^@\s]+$");
                if (!emailRegex.IsMatch(model.Email))
                {
                    return BadRequest("Invalid email format.");
                }

                var result = await _authService.ResetPasswordAsync(model.Email);
                if (result)
                {
                    _logger.LogInformation($"Password reset email sent successfully to {model.Email}");
                    return Ok("Password reset email sent.");
                }
                _logger.LogWarning($"Failed to send password reset email to {model.Email}");
                return BadRequest("Failed to send password reset email.");
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred during password reset");
                return StatusCode(500, "An error occurred while processing your request.");
            }
        }

        [HttpPost("confirm-reset-password")]
        public async Task<IActionResult> ConfirmResetPassword([FromBody] ConfirmResetPasswordDto model)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var user = await _userManager.FindByEmailAsync(model.Email);
            if (user == null)
                return BadRequest("User with provided email not found.");

            try
            {
                if (string.IsNullOrEmpty(model.NewPassword))
                {
                    return BadRequest("New password is required.");
                }

                if (string.IsNullOrEmpty(model.Token))
                {
                    return BadRequest("Reset token is required.");
                }

                var result = await _authService.ConfirmResetPasswordAsync(model.Email, model.Token, model.NewPassword);
                
                if (result)
                {
                    _logger.LogInformation($"Password reset successful for user {model.Email}");
                    return Ok("Password has been reset successfully.");
                }
                else
                {
                    return BadRequest("Failed to reset password. Invalid token or token expired.");
                }
            }
                catch (InvalidOperationException ex)
            {
                _logger.LogWarning($"Password reset validation failed for user {model.Email}: {ex.Message}");
                return BadRequest(ex.Message);
            }

            catch (Exception ex)
            {
                _logger.LogError($"Password reset failed for user {model.Email}: {ex.Message}");
                return StatusCode(500, $"An error occurred: {ex.Message}");
            }
        }

    }
}
