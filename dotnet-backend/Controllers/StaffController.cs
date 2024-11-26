using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Authorization;
using DDDSample1.Infrastructure.Staffs;
using DDDSample1.Domain.Shared;
using System;
using Microsoft.AspNetCore.JsonPatch;
using Microsoft.Extensions.Logging;
using System.Collections.Generic;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using System.Linq;


namespace DDDSample1.Controllers
{

    [Authorize(AuthenticationSchemes = JwtBearerDefaults.AuthenticationScheme)]
    [Route("api/[controller]")]
    [ApiController]
    public class StaffController : ControllerBase
    {
        private readonly StaffService _staffService;
        private readonly ILogger<StaffController> _logger;

        public StaffController(StaffService staffService, ILogger<StaffController> logger)
        {
            _staffService = staffService;
            _logger = logger;
        }



        [HttpPost("create-staff-profile")]
        [Authorize(Policy = "AdminPolicy")]
        public async Task<IActionResult> CreateStaffProfile(CreateStaffDto model)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            try
            {
                var newStaff = await _staffService.AddAsync(model);
                return Ok(newStaff);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { Message = ex.Message });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred while processing the request.");
                return StatusCode(500, new { Message = "An error occurred while processing your request." });
            }
        }



        [HttpPatch("edit-staff-profile/{id}")]
        public async Task<IActionResult> UpdateStaffProfile(string id, [FromBody] JsonPatchDocument<UpdateStaffDto> patchDoc)
        {

            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }
            if (patchDoc == null)
            {
                return BadRequest("Patch document is null");
            }

            var staff = await _staffService.GetByIdAsync(id);
            if (staff == null)
            {
                return NotFound("Staff not found");
            }

            var staffToPatch = new UpdateStaffDto
            {
                Email = staff.Email,
                PhoneNumber = staff.PhoneNumber,
            };

            patchDoc.ApplyTo(staffToPatch, ModelState);

            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            try
            {
                var updatedStaff = await _staffService.UpdateStaffAsync(id, staffToPatch);
                return Ok(updatedStaff);
            }
            catch (BusinessRuleValidationException ex) when (ex.Message.Contains("Email is already in use"))
            {
                return Conflict(new { Message = ex.Message });
            }
            catch (NotFoundException ex)
            {
                return NotFound(new { Message = ex.Message });
            }

        }



        [Authorize(Roles = "Admin")]
        [HttpPatch("{id}/deactivate")]
        public async Task<ActionResult> Deactivate(string id)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }
            try
            {
                var result = await _staffService.DeactivateAsync(id);
                if (!result)
                {
                    return NotFound();
                }

                return Ok("Staff member successfully deactivated.");
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                return StatusCode(500, $"Internal server error: {ex.Message}");
            }
        }

        [HttpGet("filter")]
        [Authorize(Roles = "Admin")]
        public async Task<ActionResult<PagedResult<StaffDto>>> GetStaffs([FromQuery] StaffFilterDto filter, int pageNumber=1, int pageSize=2)
        {
            try
            {   
                Console.WriteLine("Filter:-----------------------------------------------------------------------------");
                var staffs = await _staffService.getStaffFilteredAsync(filter, pageNumber, pageSize);
                if (staffs.Items == null || !staffs.Items.Any())
                {
                    return NotFound();
                }


                return Ok(staffs);
            }
            catch (BusinessRuleValidationException ex)
            {
                _logger.LogWarning(ex, "Business rule violation in GetStaffs");
                return BadRequest(new { Message = ex.Message });
            }
            catch (NotFoundException ex)
            {
                _logger.LogWarning(ex, "Resource not found in SearchOperationRequests");
                return NotFound(new { Message = ex.Message });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An unexpected error occurred in SearchOperationRequests");
                return StatusCode(500, new { Message = "An unexpected error occurred while processing your request. Please try again later." });
            }

        }

        [HttpGet("get-staff-profile/{id}")]
        public async Task<ActionResult<StaffDto>> GetStaffById(string id)
        {
            var staff = await _staffService.GetByIdAsync(id);
            if (staff == null)
            {
                return NotFound();
            }

            return Ok(staff);
        }

        [HttpDelete("{id}")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> DeleteStaff(string id)
        {
            var result = await _staffService.DeleteAsync(id);
            if (!result)
            {
                return NotFound();
            }
            return Ok();
        }

        [HttpGet("deactivated-staffs")]
        [Authorize(Roles = "Admin")]
        public async Task<ActionResult<IEnumerable<StaffDto>>> GetDeactivatedStaff()
        {
            var staffs = await _staffService.GetDeactivatedStaffAsync();
            if (!staffs.Any())
            {
                return NotFound();
            }
            return Ok(staffs);
        }


    }
}