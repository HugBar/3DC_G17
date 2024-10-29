using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;

namespace DDDSample1.Domain.StaffData
{
    public class CreateStaffDto : IValidatableObject
    {
        [Required(ErrorMessage = "First name is required.")]
        [StringLength(50, MinimumLength = 2, ErrorMessage = "First name must be between 2 and 50 characters.")]
        public string FirstName { get; set; }

        [Required(ErrorMessage = "Last name is required.")]
        [StringLength(50, MinimumLength = 2, ErrorMessage = "Last name must be between 2 and 50 characters.")]
        public string LastName { get; set; }

        [Required(ErrorMessage = "Email is required.")]
        [EmailAddress(ErrorMessage = "Invalid email address.")]
        public string Email { get; set; }

        [Required(ErrorMessage = "Phone number is required.")]
        [RegularExpression(@"^(\+\d{1,3}[- ]?)?\d{9}$", ErrorMessage = "Invalid phone number. Use format: +XXX XXXXXXXXX or XXXXXXXXX")]
        public string PhoneNumber { get; set; }

        [Required(ErrorMessage = "Specialization is required.")]
        [StringLength(100, MinimumLength = 2, ErrorMessage = "Specialization must be between 2 and 100 characters.")]
        public string Specialization { get; set; }

        [Required(ErrorMessage = "At least one availability slot is required.")]
        public List<AvailabilitySlot> AvailabilitySlots { get; set; } = new List<AvailabilitySlot>();

        public IEnumerable<ValidationResult> Validate(ValidationContext validationContext)
        {
            var errors = new List<string>();

            if (AvailabilitySlots.Count == 0)
            {
                errors.Add("At least one availability slot is required.");
            }
            else
            {
                for (int i = 0; i < AvailabilitySlots.Count; i++)
                {
                    var slot = AvailabilitySlots[i];

                    if (slot.StartTime >= slot.EndTime)
                    {
                        errors.Add($"Slot {i + 1}: Start time must be before end time.");
                    }

                    if (slot.StartTime < DateTime.Now)
                    {
                        errors.Add($"Slot {i + 1}: Availability slots cannot be in the past.");
                    }

                    for (int j = i + 1; j < AvailabilitySlots.Count; j++)
                    {
                        var otherSlot = AvailabilitySlots[j];
                        if ((slot.StartTime >= otherSlot.StartTime && slot.StartTime < otherSlot.EndTime) ||
                            (slot.EndTime > otherSlot.StartTime && slot.EndTime <= otherSlot.EndTime) ||
                            (slot.StartTime <= otherSlot.StartTime && slot.EndTime >= otherSlot.EndTime))
                        {
                            errors.Add($"Slot {i + 1} overlaps with Slot {j + 1}.");
                            break;
                        }
                    }
                }
            }

            if (errors.Any())
            {
                yield return new ValidationResult(string.Join(" ", errors), new[] { nameof(AvailabilitySlots) });
            }
        }


    }

}
