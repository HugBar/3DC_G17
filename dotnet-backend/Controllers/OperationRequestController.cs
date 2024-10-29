using System.Threading.Tasks;
using DDDSample1.Domain.OperationRequestData;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Shared;
using Microsoft.Extensions.Logging;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using System;
using System.Security.Claims;
using Microsoft.AspNetCore.Authentication;
using System.Collections.Generic;
using System.Linq;
using Microsoft.AspNetCore.JsonPatch;

namespace DDDSample1.Controllers
{
    [Authorize(AuthenticationSchemes = JwtBearerDefaults.AuthenticationScheme)]
    [Route("api/[controller]")]
    [ApiController]
    public class OperationRequestController : ControllerBase
    {
        private readonly OperationRequestService _service;
        private readonly ILogger<OperationRequestController> _logger;

        public OperationRequestController(OperationRequestService service, ILogger<OperationRequestController> logger)
        {
            _service = service;
            _logger = logger;
        }

        [HttpPost("create-operation-request")]
        [Authorize(Policy = "DoctorPolicy")]
        public async Task<IActionResult> CreateOperationRequest(CreateOperationRequestDto dto)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            try
            {
                var newRequest = await _service.CreateOperationRequestAsync(dto);
                return Ok(newRequest);
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


        [HttpDelete("delete-operation-request/{id}")]
        [Authorize(Policy = "DoctorPolicy")]
        public async Task<IActionResult> DeleteOperationRequest(string id)
        {
            try
            {
                var doctorEmail = User.FindFirstValue(ClaimTypes.Email);
                if (string.IsNullOrEmpty(doctorEmail))
                {
                    return Unauthorized("User email not found in the token.");
                }

                var result = await _service.DeleteOperationRequestAsync(id, doctorEmail);
                if (result)
                {
                    return Ok("Operation request deleted successfully.");
                }
                return BadRequest("Unable to delete the operation request.");
            }
            catch (NotFoundException ex)
            {
                return NotFound(ex.Message);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(ex.Message);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred while deleting the operation request.");
                return StatusCode(500, "An error occurred while processing your request.");
            }
        }

        [HttpGet("filter")]
        [Authorize(Policy = "DoctorPolicy")]
        public async Task<ActionResult<IEnumerable<OperationRequestDto>>> SearchOperationRequests([FromQuery] SearchOperationRequestDto searchDto)
        {
            try
            {
                if (!ModelState.IsValid)
                {
                    return BadRequest(ModelState);
                }

                var requests = await _service.SearchOperationRequestsAsync(searchDto);

                if (requests == null || !requests.Any())
                {
                    return NotFound("No operation requests found matching the search criteria.");
                }

                return Ok(requests);
            }
            catch (ArgumentException ex)
            {
                _logger.LogWarning(ex, "Invalid argument in SearchOperationRequests");
                return BadRequest(new { Message = ex.Message });
            }
            catch (BusinessRuleValidationException ex)
            {
                _logger.LogWarning(ex, "Business rule violation in SearchOperationRequests");
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

        [HttpPatch("{id}")]
        [Authorize(Roles = "Doctor")]
        public async Task<ActionResult<OperationRequestDto>> UpdateOperationRequest(string id, [FromBody] JsonPatchDocument<UpdateOperationRequestDto> patchDoc)
        {
            if (patchDoc == null)
                return BadRequest("Patch document is null");

            var existingOperationRequest = await _service.GetByIdAsync(id);
            if (existingOperationRequest == null)
                return NotFound();

            var operationRequestToPatch = new UpdateOperationRequestDto
            {
                OperationTypeId = existingOperationRequest.OperationTypeId,
                Deadline = existingOperationRequest.Deadline,
                Priority = existingOperationRequest.Priority
            };

            patchDoc.ApplyTo(operationRequestToPatch, ModelState);

            if (!ModelState.IsValid)
                return BadRequest(ModelState);

            try
            {
                var updatedRequest = await _service.UpdateAsync(id, operationRequestToPatch);
                return Ok(updatedRequest);
            }
            catch (NotFoundException ex)
            {
                return NotFound(ex.Message);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

    }
}