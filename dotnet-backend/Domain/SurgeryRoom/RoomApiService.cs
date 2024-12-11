using System;
using System.Threading.Tasks;
using System.Net.Http;
using System.Text.Json;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.SurgeryRoom;


namespace DDDSample1.Domain.SurgeryRoomData
{
    public class RoomApiService : IRoomApiService
    {
        private readonly ISurgeryRoomRepository _repository;

        public RoomApiService(ISurgeryRoomRepository repository)
        {
            _repository = repository ?? throw new ArgumentNullException(nameof(repository));
        }

        public async Task<SurgeryRoom> GetRoomDataAsync(string roomId)
        {
            if (string.IsNullOrWhiteSpace(roomId))
                throw new BusinessRuleValidationException("Room ID cannot be empty");

            var room = await _repository.GetByIdAsync(roomId);
            
            if (room == null)
                throw new BusinessRuleValidationException($"Room {roomId} not found");

            return room;
        }

        public async Task<bool> UpdateRoomStatusAsync(string roomId, RoomStatus status)
        {
            var room = await GetRoomDataAsync(roomId);
            room.SetStatus(status);
            
            await _repository.UpdateAsync(room);
            return true;
        }

        public async Task<bool> ScheduleMaintenanceAsync(string roomId, DateTime startTime, DateTime endTime)
        {
            var room = await GetRoomDataAsync(roomId);
            room.ScheduleMaintenance(startTime, endTime);
            
            await _repository.UpdateAsync(room);
            return true;
        }
    }
}