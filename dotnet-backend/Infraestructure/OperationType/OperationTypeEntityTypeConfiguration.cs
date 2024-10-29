using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationTypeData;
using System.Collections.Generic;
using System.Linq;
using System;

namespace DDDSample1.Infrastructure.OperationTypeData
{
    internal class OperationTypeEntityTypeConfiguration : IEntityTypeConfiguration<OperationType>
    {
        public void Configure(EntityTypeBuilder<OperationType> builder)
        {
    builder.ToTable("OperationTypes");

    builder.HasKey(o => o.Id);

    // Name é obrigatório
    builder.Property(o => o.Name)
        .IsRequired();

    // Configurando a conversão de RequiredStaffBySpecialization para JSON e vice-versa
    builder.Property(o => o.RequiredStaffBySpecialization)
        .IsRequired()  // Garante que não seja null
        .HasConversion(
            // Serializa o dicionário para JSON
            v => System.Text.Json.JsonSerializer.Serialize(v, (System.Text.Json.JsonSerializerOptions)null),
            // Desserializa JSON para o dicionário
            v => System.Text.Json.JsonSerializer.Deserialize<Dictionary<string, int>>(v, (System.Text.Json.JsonSerializerOptions)null)
        )
        // Adiciona o ValueComparer para comparar o dicionário corretamente
        .Metadata.SetValueComparer(new Microsoft.EntityFrameworkCore.ChangeTracking.ValueComparer<Dictionary<string, int>>(
            (d1, d2) => d1.SequenceEqual(d2),  // Compara os dicionários
            d => d.Aggregate(0, (a, v) => HashCode.Combine(a, v.GetHashCode())),  // Gera um hash para o dicionário
            d => d.ToDictionary(kv => kv.Key, kv => kv.Value)  // Clona o dicionário
        ));

        
        

        builder.OwnsOne(o => o.Duration, p =>
            {
                p.Property(p => p.AnesthesiaPreparation).IsRequired();
                p.Property(p => p.Surgery).IsRequired();
                p.Property(p => p.Cleaning).IsRequired();
            });

}
    }
}