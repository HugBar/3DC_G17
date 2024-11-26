using System;
using System.Threading.Tasks;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;


namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class UserController : ControllerBase
    {
        private readonly IUserService _userService;
        private readonly ILogger<UserController> _logger;

        public UserController(IUserService userService, ILogger<UserController> logger)
        {
            _userService = userService;
            _logger = logger;
        }

        [HttpPost("register")]
        public async Task<IActionResult> Register([FromBody] CreateUserDto model)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            try
            {
                var result = await _userService.RegisterUserAsync(model);
                return CreatedAtAction(nameof(GetUser), new { id = result.Id }, result);
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred during user registration");
                return StatusCode(500, "An error occurred while processing your request.");
            }
        }

        [HttpGet("{id}")]
        public async Task<IActionResult> GetUser(string id)
        {
            try
            {
                var user = await _userService.GetUserByIdAsync(id);
                if (user == null)
                {
                    return NotFound();
                }
                return Ok(user);
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred while fetching user");
                return StatusCode(500, "An error occurred while processing your request.");
            }
        }

        [HttpPut("{id}")]
        public async Task<IActionResult> UpdateUser(string id, [FromBody] UpdateUserDto model)
        {
            try
            {
                var user = await _userService.UpdateUserAsync(id, model);
                if (user == null)
                {
                    return NotFound("User not found.");
                }
                return Ok(user);  // Return 200 OK if the user is successfully updated
            }
            catch (InvalidOperationException ex)
            {

                return BadRequest(ex.Message);  // Return 400 Bad Request for other validation errors
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred while updating user");
                return StatusCode(500, "An error occurred while processing your request.");
            }
        }


        [HttpDelete("{id}")]
        public async Task<IActionResult> DeleteUser(string id)
        {
            try
            {
                var user = await _userService.GetUserByIdAsync(id);  // Assuming you have a method to get the user
                if (user == null)
                {
                    return NotFound("User not found.");
                }

                var result = await _userService.DeleteUserAsync(id);
                if (result)
                {
                    return Ok();
                }
                return Conflict("Failed to delete user due to a conflict.");  // 409 Conflict for failed deletions
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(ex.Message);  // 400 Bad Request for validation errors or known issues
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred while deleting user");
                return StatusCode(500, "An error occurred while processing your request.");  // 500 Internal Server Error
            }
        }

    }
}
