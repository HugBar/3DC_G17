using DDDSample1.Domain.PatientData;
using DDDSample1.Domain.StaffData;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

public class PatientEntityTypeConfiguration : IEntityTypeConfiguration<Patient>
{
    public void Configure(EntityTypeBuilder<Patient> builder)
    {
        // Table mapping
        builder.ToTable("Patient");

        // Key configuration for Staff
        builder.HasKey(s => s.UserId);

        // Properties configuration for Staff
        builder.Property(s => s.MedicalNr).IsRequired().HasMaxLength(15);
        builder.Property(s => s.UserId).IsRequired();
        builder.Property(s => s.FirstName).IsRequired().HasMaxLength(100);
        builder.Property(s => s.LastName).IsRequired().HasMaxLength(100);
        builder.Property(s => s.Email).IsRequired().HasMaxLength(255);
        builder.Property(s => s.DateofBirth).IsRequired().HasMaxLength(15);
        builder.Property(s => s.Gender).IsRequired().HasMaxLength(15);
        builder.Property(s => s.ContactInfo).IsRequired().HasMaxLength(15);
        builder.Property(s => s.MedicalHistory).IsRequired().HasMaxLength(15);
        builder.Property(s => s.EmergencyContact).IsRequired().HasMaxLength(15);
        builder.Property(s => s.PhoneNumber).IsRequired().HasMaxLength(15);     
        builder.Property(s => s.AppointmentHistory).IsRequired().HasMaxLength(15);
        builder.Property(s => s.MedicalHistory).IsRequired().HasMaxLength(15);
        builder.Property(p => p.IsAnonymized).IsRequired().HasDefaultValue(false);
        builder.Property(p => p.AnonymizedAt).IsRequired(false);
        

    }
}
