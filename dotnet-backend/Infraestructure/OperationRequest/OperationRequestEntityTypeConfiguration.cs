using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRequestData;

namespace DDDSample1.Infrastructure.OperationRequestData
{
       public class OperationRequestEntityTypeConfiguration : IEntityTypeConfiguration<OperationRequest>
       {
              public void Configure(EntityTypeBuilder<OperationRequest> builder)
              {
                     builder.ToTable("OperationRequests");

                     builder.HasKey(o => o.Id);

                     builder.Property(o => o.PatientId)
                            .IsRequired()
                            .HasMaxLength(50);

                     builder.Property(o => o.DoctorId)
                            .IsRequired()
                            .HasMaxLength(50);

                     builder.Property(o => o.OperationTypeId)
                            .IsRequired()
                            .HasMaxLength(100);

                     builder.Property(o => o.Deadline)
                            .IsRequired();

                     builder.Property(o => o.Priority)
                            .IsRequired()
                            .HasMaxLength(10);

                     builder.Property(o => o.IsScheduled)
                            .IsRequired();

                     builder.HasIndex(o => o.PatientId).HasDatabaseName("IX_OperationRequest_PatientId");
                     builder.HasIndex(o => o.DoctorId).HasDatabaseName("IX_OperationRequest_DoctorId");
              }
       }
}
