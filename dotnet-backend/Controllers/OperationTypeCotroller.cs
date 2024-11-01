using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using DDDSample1.Domain.OperationTypeData;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using DDDSample1.Domain.Shared;
using System;
using System.Linq;
using Microsoft.AspNetCore.JsonPatch;


namespace DDDSample1.Controllers
{
    [Authorize(AuthenticationSchemes = JwtBearerDefaults.AuthenticationScheme)]
    [Route("api/[controller]")]
    [ApiController]
    public class OperationTypeController : ControllerBase
    {
        private readonly OperationTypeService _service;

        public OperationTypeController(OperationTypeService service)
        {
            _service = service;
        }

        [HttpPost("create-operation-type")]
        [Authorize(Roles = "Admin")]
        public async Task<ActionResult<OperationTypeDto>> CreateOperationType(CreateOperationTypeDto dto)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(GetModelErrors());
            }
            try
            {
                var result = await _service.AddAsync(dto);
                return Ok(result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (Exception ex)
            {
                return StatusCode(500, new { message = ex.Message });
            }
            

        }

        [HttpPatch("edit-operation-type/{id}")]
        [Authorize(Roles = "Admin")]

        public async Task<IActionResult> UpdateOperationType([FromRoute] Guid  id, [FromBody] JsonPatchDocument<UpdateOperationTypeDto> patchDoc)
        {   
            if (patchDoc == null)
            {
                return BadRequest(new { message = "Invalid patch document." });
            }

            if (!ModelState.IsValid)
            {
                return BadRequest(GetModelErrors());
            }
            try
            {
                var operationTypeId = new OperationTypeId(id);
                var result = await _service.UpdateOperationType(operationTypeId, patchDoc);
                return Ok(result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (Exception ex)
            {
                return StatusCode(500, new { message = ex.Message });
            }
        }

        [HttpPatch("deactivate-operation-type/{id}")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> DeactivateOperationType([FromRoute] Guid id)
        {
            try
            {
                var result = await _service.DeactivateOperationType(id);
                return Ok(result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (Exception ex)
            {
                return StatusCode(500, new { message = ex.Message });
            }
        }

        [HttpGet("search-operation-type")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> SearchOperationType([FromQuery] OperationTypeFilterDto filterDto)
        {
            try
            {
                var result = await _service.SearchOperationType(filterDto);
                return Ok(result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (Exception)
            {
                return StatusCode(500, new { message = "An error occurred while searching operation types." });
            }
        }

        private object GetModelErrors()
        {
            return new
            {
                Message = "Invalid input",
                Errors = ModelState
                    .Where(x => x.Value.Errors.Count > 0)
                    .Select(x => new { Property = x.Key, Errors = x.Value.Errors.Select(e => e.ErrorMessage) })
                    .ToArray()
            };
        }

    }
}