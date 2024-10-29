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

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class GoogleAuthController : Controller
    {
        private readonly UserManager<ApplicationUser> _userManager;
        private readonly SignInManager<ApplicationUser> _signInManager;
        private readonly ILogger<GoogleAuthController> _logger;

        public GoogleAuthController(UserManager<ApplicationUser> userManager, SignInManager<ApplicationUser> signInManager, ILogger<GoogleAuthController> logger)
        {
            _userManager = userManager;
            _signInManager = signInManager;
            _logger = logger;
        }

        [HttpGet("google-login")]
        public IActionResult GoogleLogin()
        {
            _logger.LogInformation("Initiating Google login...");
            var properties = new AuthenticationProperties { RedirectUri = Url.Action("GoogleResponse") };
            return Challenge(properties, GoogleDefaults.AuthenticationScheme);
        }

        [HttpGet("google-response")]
        public async Task<IActionResult> GoogleResponse()
        {
            try
            {
                _logger.LogInformation("Handling Google response...");

                // Authenticate using the external scheme
                var authenticateResult = await HttpContext.AuthenticateAsync(IdentityConstants.ExternalScheme);

                if (!authenticateResult.Succeeded)
                {
                    var failureMessage = authenticateResult.Failure?.Message ?? "Unknown reason";
                    _logger.LogError($"Google authentication failed. Reason: {failureMessage}");
                    return BadRequest($"Google authentication failed. Reason: {failureMessage}");
                }

                var emailClaim = authenticateResult.Principal.FindFirst(ClaimTypes.Email)?.Value;
                var nameClaim = authenticateResult.Principal.FindFirst(ClaimTypes.Name)?.Value;

                _logger.LogInformation($"User authenticated. Email: {emailClaim}, Name: {nameClaim}");

                // Find or create the user in your Identity system
                var user = await _userManager.FindByEmailAsync(emailClaim);
                if (user == null)
                {
                    user = new ApplicationUser { UserName = emailClaim, Email = emailClaim };
                    var result = await _userManager.CreateAsync(user);
                    if (!result.Succeeded)
                    {
                        _logger.LogError($"Failed to create user. Errors: {string.Join(", ", result.Errors.Select(e => e.Description))}");
                        return BadRequest($"Failed to create user. Errors: {string.Join(", ", result.Errors.Select(e => e.Description))}");
                    }
                }

                // Check if the user is already in the "Patient" role
                if (!await _userManager.IsInRoleAsync(user, "Admin"))
                {
                    var roleResult = await _userManager.AddToRoleAsync(user, "Admin");
                    if (!roleResult.Succeeded)
                    {
                        _logger.LogError($"Failed to add user to role 'Patient'. Errors: {string.Join(", ", roleResult.Errors.Select(e => e.Description))}");
                        return BadRequest($"Failed to add user to role 'Patient'. Errors: {string.Join(", ", roleResult.Errors.Select(e => e.Description))}");
                    }

                    _logger.LogInformation($"User {user.Email} was successfully assigned the role 'Patient'.");
                }

                // Sign in the user
                await _signInManager.SignInAsync(user, isPersistent: false);

                return Ok("Google authentication successful.");
            }
            catch (Exception ex)
            {
                _logger.LogError($"Error during Google response: {ex.Message}");
                return BadRequest($"Authentication failed: {ex.Message}");
            }
        }





    }
}
