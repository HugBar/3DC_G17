using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationTypeData
{
    public class OperationTypeDto
    {
        public OperationTypeId Id { get; set; }
        public string Name { get; set; }
        public string OperationTypeCode { get; set; }
        public int Version { get; set; }
        public DateTime ValidFrom { get; set; }
        public DateTime? ValidTo { get; set; }
        public bool IsActive { get; set; }
        public Dictionary<String, int> RequiredStaffBySpecialization { get; set; }
        public OperationPhases Duration { get; set; }

        public OperationTypeDto(OperationType operationType)
        {
            Id = operationType.Id;
            Name = operationType.Name;
            OperationTypeCode = operationType.OperationTypeCode;
            Version = operationType.Version;
            ValidFrom = operationType.ValidFrom;
            ValidTo = operationType.ValidTo;
            IsActive = operationType.IsActive;
            RequiredStaffBySpecialization = operationType.RequiredStaffBySpecialization;
            Duration = operationType.Duration;
        }
    }
}
