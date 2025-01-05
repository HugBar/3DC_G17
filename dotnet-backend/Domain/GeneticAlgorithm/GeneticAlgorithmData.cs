using System.Collections.Generic;

public class GeneticAlgorithmData
{
    public List<StaffAvailability> StaffAvailability { get; set; }
    public List<SurgeryRequirement> SurgeryRequirements { get; set; }
    public List<StaffInfo> Staff { get; set; }
    public List<SurgeryInfo> Surgeries { get; set; }
    public List<SurgeryId> SurgeryIds { get; set; }
    public List<OperationRoomSchedule> OperationRoomSchedules { get; set; }
}

public class StaffAvailability
{
    public string StaffId { get; set; }
    public int Day { get; set; }
    public List<(int Start, int End, string Operation)> Slots { get; set; }
}

public class SurgeryRequirement
{
    public string SurgeryType { get; set; }
    public List<List<(string Role, string Specialization, int Count)>> PhaseRequirements { get; set; }
}

public class StaffInfo
{
    public string StaffId { get; set; }
    public string Role { get; set; }
    public string Speciality { get; set; }
    public List<string> Operations { get; set; }
}

public class SurgeryInfo
{
    public string SurgeryType { get; set; }
    public int PreparationTime { get; set; }
    public int SurgeryTime { get; set; }
    public int CleaningTime { get; set; }
}

public class SurgeryId
{
    public string OperationRequestId { get; set; }
    public string SurgeryType { get; set; }
}

public class OperationRoomSchedule
{
    public string Room { get; set; }
    public int Day { get; set; }
    public List<(int Start, int End, string OperationId)> Slots { get; set; }
}

public class StaffTimetable
{
    public string StaffId { get; set; }
    public string Day { get; set; }
    public (int Start, int End) TimeSlot { get; set; }
}