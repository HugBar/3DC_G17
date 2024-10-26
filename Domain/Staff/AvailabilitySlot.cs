using System;

namespace DDDSample1.Domain.StaffData
{
    public class AvailabilitySlot
    {
        public DateTime StartTime { get; set; }
        public DateTime EndTime { get; set; }

        public AvailabilitySlot() { }

        public AvailabilitySlot(DateTime startTime, DateTime endTime)
        {
            if (startTime >= endTime)
            {
                throw new ArgumentException("Start time must be before end time.");
            }

            StartTime = startTime;
            EndTime = endTime;
        }

        public void UpdateSlot(DateTime newStartTime, DateTime newEndTime)
        {
            if (newStartTime >= newEndTime)
            {
                throw new ArgumentException("Start time must be before end time.");
            }

            StartTime = newStartTime;
            EndTime = newEndTime;
        }
    }
}
