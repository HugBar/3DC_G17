
using System;
using System.Threading.Tasks;
public interface ILoggingService
{
    void LogInformation(string message);
    void LogWarning(string message);
    void LogError(string message, Exception ex = null);
    Task LogChangeAsync(string action, string userId, string patientName, object changeData);
}
