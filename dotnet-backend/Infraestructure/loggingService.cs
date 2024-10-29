using DDDSample1.Domain.Shared;
using Microsoft.Extensions.Logging;
using System;
using System.Threading.Tasks;

namespace DDDSample1.Infrastructure
{
    public class LoggingService : ILoggingService
    {
        private readonly ILogger<LoggingService> _logger;

        public LoggingService(ILogger<LoggingService> logger)
        {
            _logger = logger;
        }

        public void LogInformation(string message)
        {
            _logger.LogInformation(message);
        }

        public void LogWarning(string message)
        {
            _logger.LogWarning(message);
        }

        public void LogError(string message, Exception ex = null)
        {
            _logger.LogError(ex, message);
        }
        public async Task LogChangeAsync(string action, string userId, string patientName, object changeData)
        {
            // Aqui, você formata a mensagem de log da maneira mais detalhada
            var logMessage = $"Action: {action}, Patient Name: {patientName}, Patient ID: {userId}, Changes: {changeData}";

            // Registro assíncrono da mensagem de log
            await Task.Run(() => _logger.LogInformation(logMessage));
        }

    }
}
