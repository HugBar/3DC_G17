using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationTypeData

{
    public class OperationType : Entity<OperationTypeId>, IAggregateRoot
    {
        public string Name { get; private set; }

        public Dictionary<String, int> RequiredStaffBySpecialization { get; private set; }
        public OperationPhases Duration { get; private set; }

        public OperationType()
        {
        }
        public OperationType(string name, Dictionary<String, int> requiredStaffBySpecialization, OperationPhases duration)
        {

            Id = new OperationTypeId(Guid.NewGuid());
            SetName(name);
            SetRequiredStaffBySpecialization(requiredStaffBySpecialization);
            SetEstimatedDuration(duration);
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

        internal object WithOwner()
        {
            throw new NotImplementedException();
        }
    }
}