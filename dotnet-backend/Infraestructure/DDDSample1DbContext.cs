using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.PatientData;
using DDDSample1.Domain.OperationRequestData;
using DDDSample1.Domain.StaffData;
using Microsoft.AspNetCore.Identity.EntityFrameworkCore;
using Microsoft.AspNetCore.Identity;
using DDDSample1.Domain.UserData;
using DDDSample1.Infrastructure.OperationRequestData;
using DDDSample1.Infrastructure.OperationTypeData;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Domain.SurgeryRoomData;
using DDDSample1.Infrastructure.SurgeryRoomData;


namespace DDDSample1.Infrastructure
{
    public class DDDSample1DbContext : IdentityDbContext<ApplicationUser, IdentityRole, string>

    {

        public DbSet<Patient> Patients { get; set; }

        public DbSet<Staff> Staffs { get; set; }

        public DbSet<OperationRequest> OperationRequests { get; set; }

        public DbSet<OperationType> OperationTypes { get; set; }

        public DbSet<SurgeryRoom> SurgeryRooms { get; set; }



        public DDDSample1DbContext(DbContextOptions<DDDSample1DbContext> options) : base(options)
        {
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            modelBuilder.Ignore<AvailabilitySlot>();

            modelBuilder.ApplyConfiguration(new OperationRequestEntityTypeConfiguration());


            modelBuilder.ApplyConfiguration(new StaffEntityTypeConfiguration());

            modelBuilder.ApplyConfiguration(new OperationTypeEntityTypeConfiguration());

            modelBuilder.ApplyConfiguration(new SurgeryRoomEntityTypeConfiguration());

            modelBuilder.Entity<Staff>()
                .OwnsMany(s => s.AvailabilitySlots, a =>
                {
                    a.WithOwner().HasForeignKey("StaffId");  // FK to Staff
                    a.Property<int>("Id");  // Internal Id for each slot (optional)
                    a.HasKey("Id");  // Optional: Make it a key if you want unique slots
                });


            modelBuilder.Entity<Patient>(entity =>
                {
                    entity.HasKey(p => p.UserId);
                    entity.Property(p => p.FirstName).IsRequired();
                    // Outros mapeamentos...
                });
            
        }




    }
}
