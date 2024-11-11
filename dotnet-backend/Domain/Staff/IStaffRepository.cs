using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDSample1.Domain.StaffData
{
    public interface IStaffRepository
    {
        Task<Staff> GetByIdAsync(string id);
        Task<Staff> GetByUserIdAsync(string userId);
        Task<Staff> GetByEmailAsync(string email);
        Task<List<Staff>> GetAllAsync();
        Task<Staff> AddAsync(Staff staff);
        void Remove(Staff staff);
        Task<bool> IsEmailUniqueAsync(string email);
        Task<bool> IsPhoneNumberUniqueAsync(string phoneNumber);
        Task<Staff> UpdateAsync(Staff staff);
        Task<List<Staff>> GetActiveStaffAsync();
        Task<bool> ExistsAsync(string staffId);
        Task<(List<Staff> Items, int TotalCount)> GetFilteredStaffAsync(
        StaffFilterDto filters,
        int pageNumber,
        int pageSize
    );

        Task<List<Staff>> GetDeactivatedStaffAsync();
    }
}
