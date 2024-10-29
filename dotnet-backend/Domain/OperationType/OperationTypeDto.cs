using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationTypeData
{
    public class OperationTypeDto
    {
        public OperationTypeId Id { get; set; }
        public string Name { get; set; }
        public Dictionary<String, int> RequiredStaffBySpecialization { get; set; }
        public OperationPhases Duration { get; set; }

        public OperationTypeDto(OperationTypeId id, string name, Dictionary<String, int> requiredStaffBySpecialization, OperationPhases estimatedDuration)
        {
            Id = id;
            Name = name;
            RequiredStaffBySpecialization = requiredStaffBySpecialization;
            Duration = estimatedDuration;
        }
    }
}
