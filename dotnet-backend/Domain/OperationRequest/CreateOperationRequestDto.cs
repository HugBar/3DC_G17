using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace DDDSample1.Domain.OperationRequestData
{
    public class CreateOperationRequestDto : IValidatableObject
    {
        [Required(ErrorMessage = "Patient Medical Record Number is required.")]
        public string PatientMRN { get; set; }

        [Required(ErrorMessage = "Doctor License Number is required.")]
        [RegularExpression(@"^LIC-[A-Z0-9]{8}$", ErrorMessage = "Invalid license number format. Should be LIC-XXXXXXXX")]
        public string DoctorLicenseNumber { get; set; }

        [Required(ErrorMessage = "Operation Type ID is required.")]
        public string OperationTypeId { get; set; }

        [Required(ErrorMessage = "Deadline is required.")]
        public DateTime Deadline { get; set; }

        [Required(ErrorMessage = "Priority is required.")]
        public string Priority { get; set; }

        public IEnumerable<ValidationResult> Validate(ValidationContext validationContext)
        {
            var validPriorities = new List<string> { "elective", "urgent", "emergency" };
            var errors = new List<string>();

            if (Deadline <= DateTime.Now)
            {
                errors.Add("The deadline must be a future date.");
            }

            if (!validPriorities.Contains(Priority.ToLower()))
            {
                errors.Add("Invalid priority. Valid values are: elective, urgent, emergency.");
            }

            if (errors.Count > 0)
            {
                yield return new ValidationResult(string.Join(" ", errors), new[] { nameof(Priority), nameof(Deadline) });
            }
        }
    }
}
