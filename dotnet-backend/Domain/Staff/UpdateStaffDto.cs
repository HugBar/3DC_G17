// Domain/Staff/UpdateStaffDto.cs

using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using DDDSample1.Domain.StaffData;

#nullable enable

public class UpdateStaffDto : IValidatableObject
{
    [EmailAddress(ErrorMessage = "Invalid email format.")]
    [RegularExpression(@"^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$", ErrorMessage = "Please enter a valid email address.")]
    public string? Email { get; set; }

    [Phone(ErrorMessage = "Invalid phone number format.")]
    [RegularExpression(@"^\d{9}$", ErrorMessage = "Phone number must be exactly 9 digits.")]
    public string? PhoneNumber { get; set; }


    [Required(ErrorMessage = "Specialization is required.")]
    [StringLength(100, ErrorMessage = "Specialization cannot be longer than 100 characters.")]
    public string? Specialization { get; set; }

    public List<AvailabilitySlot>? AvailabilitySlots { get; set; }

    public IEnumerable<ValidationResult> Validate(ValidationContext validationContext)
    {
        var errors = new List<ValidationResult>();

        if (AvailabilitySlots != null)
        {
            if (AvailabilitySlots.Count == 0)
            {
                errors.Add(new ValidationResult("At least one availability slot is required.", new[] { nameof(AvailabilitySlots) }));
            }
            else
            {
                for (int i = 0; i < AvailabilitySlots.Count; i++)
                {
                    var slot = AvailabilitySlots[i];

                    if (slot.StartTime >= slot.EndTime)
                    {
                        errors.Add(new ValidationResult($"Slot {i + 1}: Start time must be before end time.", new[] { nameof(AvailabilitySlots) }));
                    }

                    if (slot.StartTime < DateTime.Now)
                    {
                        errors.Add(new ValidationResult($"Slot {i + 1}: Availability slots cannot be in the past.", new[] { nameof(AvailabilitySlots) }));
                    }

                    for (int j = i + 1; j < AvailabilitySlots.Count; j++)
                    {
                        var otherSlot = AvailabilitySlots[j];
                        if ((slot.StartTime >= otherSlot.StartTime && slot.StartTime < otherSlot.EndTime) ||
                            (slot.EndTime > otherSlot.StartTime && slot.EndTime <= otherSlot.EndTime) ||
                            (slot.StartTime <= otherSlot.StartTime && slot.EndTime >= otherSlot.EndTime))
                        {
                            errors.Add(new ValidationResult($"Slot {i + 1} overlaps with Slot {j + 1}.", new[] { nameof(AvailabilitySlots) }));
                            break;
                        }
                    }
                }
            }
        }

        return errors;
    }
}
