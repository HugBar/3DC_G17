using System.Threading.Tasks;

public interface IEmailService
{
    Task SendEmailAsync(string to, string subject, string body);
    Task SendAdminLockoutNotification(string userEmail);

}
