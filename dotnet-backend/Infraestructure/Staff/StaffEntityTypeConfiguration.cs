using DDDSample1.Domain.StaffData;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

public class StaffEntityTypeConfiguration : IEntityTypeConfiguration<Staff>
{
    public void Configure(EntityTypeBuilder<Staff> builder)
    {

        builder.ToTable("Staffs");

        builder.HasKey(s => s.Id);

        builder.Property(s => s.UserId).IsRequired();
        builder.Property(s => s.FirstName).IsRequired().HasMaxLength(100);
        builder.Property(s => s.LastName).IsRequired().HasMaxLength(100);
        builder.Property(s => s.Email).IsRequired().HasMaxLength(255);
        builder.Property(s => s.PhoneNumber).IsRequired().HasMaxLength(15);
        builder.Property(s => s.Specialization).IsRequired();
        builder.Property(s => s.LicenseNumber).IsRequired();
        builder.Property(s => s.Active).IsRequired();

        builder.OwnsMany(s => s.AvailabilitySlots, a =>
        {
            a.WithOwner().HasForeignKey("StaffId");
            a.Property<int>("Id");
            a.HasKey("Id");
            a.ToTable("AvailabilitySlots");
        });

        builder.Navigation(s => s.AvailabilitySlots).AutoInclude();
    }
}
