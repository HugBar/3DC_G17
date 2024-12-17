using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.PatientData;
using System.Collections.Generic;
using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using System.Security.Claims;
using Microsoft.AspNetCore.JsonPatch;
using DDDSample1.Domain.Shared;



namespace DDDSample1.Controllers
{

    [Authorize(AuthenticationSchemes = JwtBearerDefaults.AuthenticationScheme)]
    [ApiController]
    [Route("api/[controller]")]
    public class PatientController : ControllerBase
    {
        private readonly PatientService _service;

        public PatientController(PatientService service)
        {
            _service = service;
        }

        [HttpPost]

        public async Task<IActionResult> RegisterPatient(RegisterPatientDto dto)

        {

            var result = await _service.AddAsync(dto);

            return Ok(result);

        }

        [HttpGet("filter")]
        [Authorize(Roles = "Doctor,Admin")]
        public async Task<ActionResult<PagedResult<PatientDto>>> GetPatients(
            [FromQuery] PatientFilterDTO filter,
            [FromQuery] int pageNumber = 1,
            [FromQuery] int pageSize = 5)
        {
            var result = await _service.GetFilteredPatient(filter, pageNumber, pageSize);

            if (result.Items == null || !result.Items.Any())
            {
                return NotFound("No patients found matching the criteria.");
            }

            return Ok(result);
        }

        [HttpGet("get-patient-profile/{id}")]
        public async Task<IActionResult> GetPatientById([FromRoute] string id)
        {
            var patient = await _service.GetByIdAsync(id);
            if (patient == null)
            {
                return NotFound("Patient not found.");
            }
            return Ok(patient);
        }
        [HttpPatch("edit-patient-profile")]
        [Authorize(Roles = "Patient")]
        public async Task<IActionResult> UpdatePatientProfile(string email, [FromBody] JsonPatchDocument<UpdatePatientDto> patchDoc)
        {
            if (!User.IsInRole("Patient"))
            {
                return Unauthorized("User is not authorized to perform this action.");
            }
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }
            if (patchDoc == null)
            {
                return BadRequest("Patch document is null");
            }

            var userEmail = User.FindFirstValue(ClaimTypes.Email);
            if (string.IsNullOrEmpty(userEmail))
            {
                return Unauthorized("User email not found in the token.");
            }

            var patient = await _service.GetByEmailAsync(userEmail);
            if (patient == null)
            {
                return NotFound("Patient not found");
            }
            Console.WriteLine("Patient found: " + patient.PhoneNumber);

            var patientToPatch = new UpdatePatientDto
            {
                Email = patient.Email,
                PhoneNumber = patient.PhoneNumber,

            };

            patchDoc.ApplyTo(patientToPatch, ModelState);

            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            try
            {
                var updatedPatient = await _service.UpdatePatientProfileAsync(userEmail, patientToPatch);
                return Ok(updatedPatient);
            }
            catch (BusinessRuleValidationException ex) when (ex.Message.Contains("Email is already in use"))
            {
                return Conflict(new { Message = ex.Message });
            }
            catch (Exception ex) when (ex.Message == "Patient not found.")
            {
                return NotFound("Patient not found.");
            }
        }

        [HttpDelete("delete-patient/{id}")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> DeletePatient([FromRoute] string id, [FromBody] DeletePatientDto deleteDto)
        {
            if (!User.IsInRole("Admin"))
            {
                return Unauthorized();
            }

            if (!deleteDto.ConfirmDeletion)
            {
                return BadRequest("Deletion not confirmed.");
            }

            try
            {
                var result = await _service.DeletePatientAsync(id);
                if (!result)
                {
                    return NotFound("Patient not found.");
                }
                return Ok("Patient successfully deleted.");
            }
            catch (Exception ex)
            {
                return BadRequest(ex.Message);
            }
        }

        [HttpPatch("admin/edit-patient-profile/{id}")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> AdminUpdatePatientProfile(string id, [FromBody] JsonPatchDocument<UpdatePatientDto> patchDoc)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }
            if (patchDoc == null)
            {
                return BadRequest("Patch document is null");
            }

            var patient = await _service.GetByIdAsync(id);
            if (patient == null)
            {
                return NotFound("Patient not found");
            }

            var patientToPatch = new UpdatePatientDto
            {
                Email = patient.Email,
                PhoneNumber = patient.PhoneNumber,
                // Add other fields that can be updated
            };

            patchDoc.ApplyTo(patientToPatch, ModelState);

            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            try
            {
                var updatedPatient = await _service.AdminUpdatePatientProfileAsync(id, patientToPatch);
                return Ok(updatedPatient);
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
        [HttpPost("account-deletion-request")]
        [Authorize(Roles = "Patient")]
        public async Task<IActionResult> RequestAccountDeletion()
        {
            var userEmail = User.FindFirstValue(ClaimTypes.Email);
            if (string.IsNullOrEmpty(userEmail))
            {
                return Unauthorized("User email not found.");
            }

            try
            {
                // Enviar o email de confirmação para o paciente
                await _service.RequestAccountDeletionAsync(userEmail);
                return Ok("Confirmation email sent. Please check your email to confirm account deletion.");
            }
            catch (Exception ex)
            {
                return BadRequest(ex.Message);
            }
        }

        [HttpDelete("confirm-account-deletion")]
        [Authorize(Roles = "Patient")]
        public async Task<IActionResult> ConfirmAccountDeletion([FromBody] DeleteConfirmationDto dto)
        {
            if (string.IsNullOrEmpty(dto.Token))
            {
                return BadRequest("Token is required.");
            }

            var userEmail = User.FindFirstValue(ClaimTypes.Email);
            if (string.IsNullOrEmpty(userEmail))
            {
                return Unauthorized("User email not found.");
            }

            try
            {
                var emailFromToken = _service.ValidateTokenAndGetEmail(dto.Token);
                if (emailFromToken != userEmail)
                {
                    return Unauthorized("Invalid token or email mismatch.");
                }

                var result = await _service.ConfirmAccountDeletionAsync(emailFromToken);
                if (!result)
                {
                    return NotFound("Patient not found.");
                }

                return Ok("Account deletion confirmed and completed.");
            }
            catch (Exception ex)
            {
                return BadRequest(ex.Message);
            }
        }

        [HttpGet("verify/{medicalNumber}")]
        [AllowAnonymous]
        public async Task<IActionResult> VerifyPatientExists([FromRoute] string medicalNumber)
        {
            try
            {
                var exists = await _service.ExistsByMedicalNumberAsync(medicalNumber);
                if (!exists)
                {
                    return NotFound();
                }
                return Ok();
            }
            catch (Exception ex)
            {
                return BadRequest(ex.Message);
            }
        }




    }

}
