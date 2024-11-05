using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.SurgeryRoom;

namespace DDDSample1.Domain.SurgeryRoomData
{
    public interface ISurgeryRoomRepository
    {
        Task<SurgeryRoom> AddAsync(SurgeryRoom surgeryRoom);
        Task<SurgeryRoom> GetByIdAsync(string roomNumber);
        Task<List<SurgeryRoom>> GetAllAsync();
        Task<List<SurgeryRoom>> GetByTypeAsync(RoomType type);
        Task<List<SurgeryRoom>> GetByStatusAsync(RoomStatus status);
        Task<SurgeryRoom> UpdateAsync(SurgeryRoom surgeryRoom);
        Task<bool> RemoveAsync(string id);
    }
} 