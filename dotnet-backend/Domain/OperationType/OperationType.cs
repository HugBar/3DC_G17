using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationTypeData

{
    public class OperationType : Entity<OperationTypeId>, IAggregateRoot
    {
        public string Name { get; private set; }

        public int Version { get; private set; }
        public DateTime ValidFrom { get; private set; }
        public DateTime? ValidTo { get; private set; }
        public Dictionary<String, int> RequiredStaffBySpecialization { get; private set; }
        public OperationPhases Duration { get; private set; }

        public bool IsActive { get; private set; }
        public string OperationTypeCode { get; private set; } // This will be shared across versions


        public OperationType()
        {
        }
        public OperationType(string name, Dictionary<String, int> requiredStaffBySpecialization, OperationPhases duration)
        {

            Id = new OperationTypeId(Guid.NewGuid());
            OperationTypeCode = Guid.NewGuid().ToString();
            SetName(name);
            SetRequiredStaffBySpecialization(requiredStaffBySpecialization);
            SetEstimatedDuration(duration);
            Version = 1;
            ValidFrom = DateTime.UtcNow;
            IsActive = true;
        }
        public OperationType CreateNewVersion(Dictionary<String, int> requiredStaffBySpecialization,
            OperationPhases duration)
        {
            ValidTo = DateTime.UtcNow;
            IsActive = false;

            return new OperationType
            {
                Id = new OperationTypeId(Guid.NewGuid()),
                Name = this.Name,
                OperationTypeCode = this.OperationTypeCode, // Mantém o mesmo código da versão anterior
                RequiredStaffBySpecialization = requiredStaffBySpecialization,
                Duration = duration,
                Version = this.Version + 1,
                ValidFrom = DateTime.UtcNow,
                IsActive = true
            };
        }

        public static OperationType CreateNewVersionSpecialization(
        OperationType previousVersion,
        Dictionary<string, int> newRequiredStaffBySpecialization)
        {
            var newVersion = new OperationType
            {
                Id = new OperationTypeId(Guid.NewGuid()),
                Name = previousVersion.Name,
                RequiredStaffBySpecialization = newRequiredStaffBySpecialization,
                Duration = new OperationPhases(
                    previousVersion.Duration.AnesthesiaPreparation,
                    previousVersion.Duration.Surgery,
                    previousVersion.Duration.Cleaning
                ),
                IsActive = true,
                Version = previousVersion.Version + 1,
                OperationTypeCode = previousVersion.OperationTypeCode,
                ValidFrom = DateTime.UtcNow,
                ValidTo = null
            };

            return newVersion;
        }
        public void SetName(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Operation name cannot be empty.");
            Name = name;
        }

        public void SetRequiredStaffBySpecialization(Dictionary<String, int> requiredStaffBySpecialization)
        {
            if (requiredStaffBySpecialization == null)
                throw new BusinessRuleValidationException("Required staff by specialization cannot be empty.");
            RequiredStaffBySpecialization = requiredStaffBySpecialization;
        }

        public void SetEstimatedDuration(OperationPhases duration)
        {
            if (duration == null)
                throw new BusinessRuleValidationException("Operation phases cannot be null.");

            Duration = duration;
        }

        public void SetIsActive(bool isActive)
        {
            IsActive = isActive;
        }

        public void Deactivate()
        {
            IsActive = false;
        }

        internal object WithOwner()
        {
            throw new NotImplementedException();
        }
    }
}