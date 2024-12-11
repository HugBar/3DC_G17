// IRoomApiService.cs
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.SurgeryRoom;


namespace DDDSample1.Domain.SurgeryRoomData
{
    public interface IRoomApiService 
    {
        Task<SurgeryRoom> GetRoomDataAsync(string roomId);
        Task<bool> UpdateRoomStatusAsync(string roomId, RoomStatus status);
        Task<bool> ScheduleMaintenanceAsync(string roomId, DateTime startTime, DateTime endTime);
    }
}