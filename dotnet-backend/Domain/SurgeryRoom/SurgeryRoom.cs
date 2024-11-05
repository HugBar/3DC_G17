using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.SurgeryRoom;

namespace DDDSample1.Domain.SurgeryRoomData
{
    public class SurgeryRoom 
    {
        public string Id { get; private set; }
        public RoomType Type { get; private set; }
        public int Capacity { get; private set; }
        public List<string> AssignedEquipment { get; private set; }
        public RoomStatus Status { get; private set; }
        public List<MaintenanceSlot> MaintenanceSlots { get; private set; }

        private SurgeryRoom()
        {
            AssignedEquipment = new List<string>();
            MaintenanceSlots = new List<MaintenanceSlot>();
        }

        public SurgeryRoom(string roomNumber, RoomType type, int capacity)
        {
            if (string.IsNullOrWhiteSpace(roomNumber))
                throw new BusinessRuleValidationException("Room number cannot be empty");
            if (capacity <= 0)
                throw new BusinessRuleValidationException("Capacity must be greater than zero");

            Id = roomNumber;
            Type = type;
            Capacity = capacity;
            Status = RoomStatus.Available;
            AssignedEquipment = new List<string>();
            MaintenanceSlots = new List<MaintenanceSlot>();
        }

        public void ScheduleMaintenance(DateTime startTime, DateTime endTime)
        {
            if (startTime >= endTime)
                throw new BusinessRuleValidationException("End time must be after start time");

            MaintenanceSlots.Add(new MaintenanceSlot(startTime, endTime));
            Status = RoomStatus.UnderMaintenance;
        }

        public void SetStatus(RoomStatus newStatus)
        {
            Status = newStatus;
        }
    }
}