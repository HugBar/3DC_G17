using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using DDDSample1.Domain.StaffData;


namespace DDDSample1.Domain.StaffData
{

    public class AvailabilitySlotDto
    {
        public DateTime StartTime { get; set; }
        public DateTime EndTime { get; set; }
    }
}