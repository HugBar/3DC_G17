using System.ComponentModel.DataAnnotations;

namespace DDDSample1.Domain.PatientData
{
    public class DeleteConfirmationDto
{
    [Required]
    [StringLength(6, MinimumLength = 6)]
    public string Code { get; set; }
}
}
