using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.SurgeryRoomData;
using System.Text.Json;
using System.Collections.Generic;
using System.Linq;
using System;

namespace DDDSample1.Infrastructure.SurgeryRoomData
{
    public class SurgeryRoomEntityTypeConfiguration : IEntityTypeConfiguration<SurgeryRoom>
    {
        public void Configure(EntityTypeBuilder<SurgeryRoom> builder)
        {
            builder.ToTable("SurgeryRooms");

            builder.HasKey(r => r.Id);

            builder.Property(r => r.Id)
                .IsRequired()
                .HasMaxLength(20)
                .ValueGeneratedNever();
            
            builder.HasIndex(r => r.Id)
        .IsUnique();

            builder.Property(r => r.Type)
                .IsRequired()
                .HasConversion<string>();

            builder.Property(r => r.Capacity)
                .IsRequired();

            builder.Property(r => r.Status)
                .IsRequired()
                .HasConversion<string>();

            builder.Property(r => r.AssignedEquipment)
                .HasConversion(
                    v => JsonSerializer.Serialize(v, (JsonSerializerOptions)null),
                    v => JsonSerializer.Deserialize<List<string>>(v, (JsonSerializerOptions)null)
                )
                .Metadata.SetValueComparer(
                    new Microsoft.EntityFrameworkCore.ChangeTracking.ValueComparer<List<string>>(
                        (c1, c2) => c1 != null && c2 != null && c1.SequenceEqual(c2),
                        c => c.Aggregate(0, (a, v) => HashCode.Combine(a, v.GetHashCode())),
                        c => c.ToList()
                    )
                );

            builder.OwnsMany(r => r.MaintenanceSlots, ms =>
            {
                ms.WithOwner().HasForeignKey("SurgeryRoomId");
                ms.Property<int>("Id");
                ms.HasKey("Id");
                ms.Property(m => m.StartTime).IsRequired();
                ms.Property(m => m.EndTime).IsRequired();
                ms.ToTable("MaintenanceSlots");
            });
        }
    }
}