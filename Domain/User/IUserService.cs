using System.Threading.Tasks;
using System.Collections.Generic;

namespace DDDSample1.Domain.User
{
    public interface IUserService
    {
        Task<UserDto> RegisterUserAsync(CreateUserDto model);
        Task<UserDto> GetUserByIdAsync(string id);
        Task<UserDto> UpdateUserAsync(string id, UpdateUserDto model);
        Task<bool> DeleteUserAsync(string id);
    }
}
