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

        public async Task<List<Staff>> GetFilteredStaffAsync(StaffFilterDto filter)
        {
            var staffs = _context.Staffs.AsQueryable();

            if (!string.IsNullOrEmpty(filter.FirstName))
            {
                staffs = staffs.Where(s => s.FirstName.StartsWith(filter.FirstName));
            }

            if (!string.IsNullOrEmpty(filter.LastName))
            {
                staffs = staffs.Where(s => s.LastName.StartsWith(filter.LastName));
            }

            if (!string.IsNullOrEmpty(filter.Email))
            {
                staffs = staffs.Where(s => s.Email.StartsWith(filter.Email));
            }

            if (filter.PhoneNumber != null)
            {
                throw new ArgumentException("Filtering by PhoneNumber is not allowed.");
            }

            if (!string.IsNullOrEmpty(filter.Specialization))
            {
                staffs = staffs.Where(s => s.Specialization.StartsWith(filter.Specialization));
            }

            if (!string.IsNullOrEmpty(filter.LicenseNumber))
            {
                staffs = staffs.Where(s => s.LicenseNumber.StartsWith(filter.LicenseNumber));
            }

            if (filter.Active)
            {
                throw new ArgumentException("Filtering by Active is not allowed.");
            }

            if (filter.AvailabilitySlots != null)
            {
                throw new ArgumentException("Filtering by AvailabilitySlots is not allowed.");
            }

            return await staffs.Where(s => s.Active).ToListAsync();
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
