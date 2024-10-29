using System.Threading.Tasks;
using DDDSample1.Domain.UserData;

namespace DDDSample1.Domain.Auth
{
    public interface IAuthService
    {
        Task<string> LoginAsync(LoginDto model);
        Task<bool> LogoutAsync();
        Task<bool> ChangePasswordAsync(string userId, string currentPassword, string newPassword);
        Task<bool> ResetPasswordAsync(string email);
        Task<bool> ConfirmResetPasswordAsync(string email, string token, string newPassword);

    }
}
