using System;

namespace DDDSample1.Domain.SurgeryRoom
{
    public class MaintenanceSlot
    {
        public DateTime StartTime { get; private set; }
        public DateTime EndTime { get; private set; }

        public MaintenanceSlot(DateTime startTime, DateTime endTime)
        {
            StartTime = startTime;
            EndTime = endTime;
        }
    }
}