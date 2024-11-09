using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.StaffData;
using System;
using Xunit.Sdk;

namespace DDDSample1.Infrastructure.Staffs
{
    public class StaffRepository : IStaffRepository
    {
        private readonly DDDSample1DbContext _context;

        public StaffRepository(DDDSample1DbContext context)
        {
            _context = context;
        }

        public async Task<Staff> GetByIdAsync(string id)
        {
            return await _context.Staffs.SingleOrDefaultAsync(s => s.Id == id);
        }

        public async Task<List<Staff>> GetByIdsAsync(List<string> ids)
        {
            return await _context.Staffs.Where(s => ids.Contains(s.Id)).ToListAsync();
        }

        public async Task<List<Staff>> GetAllAsync()
        {
            return await _context.Staffs.ToListAsync();
        }

        public async Task<Staff> AddAsync(Staff staff)
        {
            var result = await _context.Staffs.AddAsync(staff);
            await _context.SaveChangesAsync();
            return result.Entity;
        }

        public void Remove(Staff staff)
        {
            _context.Staffs.Remove(staff);
            _context.SaveChanges();
        }

        public async Task<Staff> UpdateAsync(Staff staff)
        {
            _context.Staffs.Update(staff);
            await _context.SaveChangesAsync();
            return staff;
        }

        public async Task<Staff> FindByUserIdAsync(string userId)
        {
            return await _context.Staffs.FirstOrDefaultAsync(s => s.UserId == userId);
        }

        public async Task<bool> IsEmailUniqueAsync(string email)
        {
            return !await _context.Staffs.AnyAsync(s => s.Email == email);
        }

        public async Task<bool> IsPhoneNumberUniqueAsync(string phoneNumber)
        {
            return !await _context.Staffs.AnyAsync(s => s.PhoneNumber == phoneNumber);
        }

        public async Task<Staff> GetByUserIdAsync(string userId)
        {
            return await _context.Staffs.FirstOrDefaultAsync(s => s.UserId == userId);
        }

        // Adding the missing GetByEmailAsync method
        public async Task<Staff> GetByEmailAsync(string email)
        {
            return await _context.Staffs
                .Include(s => s.AvailabilitySlots)
                .FirstOrDefaultAsync(s => s.Email == email);
        }

        public async Task<List<Staff>> GetActiveStaffAsync()
        {
            return await _context.Staffs
                                 .Where(s => s.Active)
                                 .ToListAsync();
        }

         public async Task<(List<Staff> Items, int TotalCount)> GetFilteredStaffAsync(
        StaffFilterDto filters,
        int pageNumber,
        int pageSize)
        {
        var query = _context.Staffs
            .AsQueryable();

        // Aplicar filtros
        if (!string.IsNullOrWhiteSpace(filters.FirstName))
            query = query.Where(s => s.FirstName.Contains(filters.FirstName));
        
        if (!string.IsNullOrWhiteSpace(filters.LastName))
            query = query.Where(s => s.LastName.Contains(filters.LastName));
        
        if (!string.IsNullOrWhiteSpace(filters.Email))
            query = query.Where(s => s.Email.Contains(filters.Email));
        
        if (!string.IsNullOrWhiteSpace(filters.Specialization))
            query = query.Where(s => s.Specialization.Contains(filters.Specialization));

        // Obter contagem total antes da paginação
        var totalCount = await query.CountAsync();

        // Aplicar paginação
        var items = await query
            .OrderBy(s => s.FirstName) // Ordenação padrão
            .Skip((pageNumber - 1) * pageSize)
            .Take(pageSize)
            .ToListAsync();

        return (items, totalCount);
    }



        public async Task<bool> ExistsAsync(string staffId)
        {
            return await _context.Staffs.AnyAsync(s => s.Id == staffId);
        }

        public async Task<List<Staff>> GetDeactivatedStaffAsync()
        {
            return await _context.Staffs.Where(s => !s.Active).ToListAsync();
        }

    }
}
