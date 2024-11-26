using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

public class UpdatePatientDto : IValidatableObject
{
    public string UserId { get; set; }

    [StringLength(100, ErrorMessage = "First name cannot be longer than 100 characters.")]
    public string FirstName { get; set; }

    [StringLength(100, ErrorMessage = "Last name cannot be longer than 100 characters.")]
    public string LastName { get; set; }

    [EmailAddress(ErrorMessage = "Invalid email format.")]
    [RegularExpression(@"^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$", ErrorMessage = "Please enter a valid email address.")]
    public string Email { get; set; }

    [Phone(ErrorMessage = "Invalid phone number format.")]
    [RegularExpression(@"^\d{9}$", ErrorMessage = "Phone number must be exactly 9 digits.")]
    public string PhoneNumber { get; set; }

    [DataType(DataType.Date)]
    [DisplayFormat(DataFormatString = "{0:yyyy-MM-dd}", ApplyFormatInEditMode = true)]
    public string DateOfBirth { get; set; }

    [StringLength(50, ErrorMessage = "Gender cannot be longer than 50 characters.")]
    public string Gender { get; set; }

    [StringLength(500, ErrorMessage = "Contact info cannot be longer than 500 characters.")]
    public string ContactInfo { get; set; }

    [StringLength(500, ErrorMessage = "Emergency contact info cannot be longer than 500 characters.")]
    public string EmergencyContact { get; set; }

    [StringLength(1000, ErrorMessage = "Medical history cannot be longer than 1000 characters.")]
    public string MedicalHistory { get; set; }

    [RegularExpression(@"^MED-[A-Z0-9]{8}$", ErrorMessage = "Invalid medical number format. It should be 'MED-' followed by 8 alphanumeric characters.")]
    public string MedicalNr { get; set; }

    public IEnumerable<ValidationResult> Validate(ValidationContext validationContext)
    {
        yield break;
    }
}
