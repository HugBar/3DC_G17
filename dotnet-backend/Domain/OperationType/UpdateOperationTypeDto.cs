using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace DDDSample1.Domain.OperationTypeData
{
    public class UpdateOperationTypeDto
    {
        [Required]
        public string Name { get; set; }

        [Required]
        public Dictionary<String, int> RequiredStaffBySpecialization { get; set; }

        [Required]
        public TimeSpan AnesthesiaPreparation { get; set; }

        [Required]
        public TimeSpan Surgery { get; set; }

        [Required]
        public TimeSpan Cleaning { get; set; }
    }
}

