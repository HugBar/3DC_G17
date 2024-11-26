using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.SurgeryRoomData;
using System.Linq;
using DDDSample1.Domain.SurgeryRoom;

namespace DDDSample1.Infrastructure.SurgeryRoomData
{
    public class SurgeryRoomRepository : ISurgeryRoomRepository
    {
        private readonly DDDSample1DbContext _context;

        public SurgeryRoomRepository(DDDSample1DbContext context)
        {
            _context = context;
        }

        public async Task<SurgeryRoom> AddAsync(SurgeryRoom surgeryRoom)
        {
            var result = await _context.SurgeryRooms.AddAsync(surgeryRoom);
            await _context.SaveChangesAsync();
            return result.Entity;
        }

        public async Task<SurgeryRoom> GetByIdAsync(string roomNumber)
        {
            return await _context.SurgeryRooms
                .FirstOrDefaultAsync(r => r.Id == roomNumber);
        }

        public async Task<List<SurgeryRoom>> GetAllAsync()
        {
            return await _context.SurgeryRooms.ToListAsync();
        }

        public async Task<List<SurgeryRoom>> GetByTypeAsync(RoomType type)
        {
            return await _context.SurgeryRooms
                .Where(r => r.Type == type)
                .ToListAsync();
        }

        public async Task<List<SurgeryRoom>> GetByStatusAsync(RoomStatus status)
        {
            return await _context.SurgeryRooms
                .Where(r => r.Status == status)
                .ToListAsync();
        }

        public async Task<SurgeryRoom> UpdateAsync(SurgeryRoom surgeryRoom)
        {
            _context.SurgeryRooms.Update(surgeryRoom);
            await _context.SaveChangesAsync();
            return surgeryRoom;
        }

        public async Task<bool> RemoveAsync(string id)
        {
            var surgeryRoom = await GetByIdAsync(id);
            if (surgeryRoom == null) return false;

            _context.SurgeryRooms.Remove(surgeryRoom);
            await _context.SaveChangesAsync();
            return true;
        }

    }
}