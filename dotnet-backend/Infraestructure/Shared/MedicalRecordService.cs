using System;
using System.Net.Http;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;

public class MedicalRecordService : IMedicalRecordService
{
    private readonly HttpClient _httpClient;
    private readonly string _baseUrl;

    public MedicalRecordService(HttpClient httpClient, IConfiguration configuration)
    {
        _httpClient = httpClient;
        _baseUrl = configuration["MedicalRecordsApi:BaseUrl"];
    }

    public async Task CreateBlankMedicalRecord(string patientId)
    {
        try
        {
            var response = await _httpClient.PostAsync(
                $"{_baseUrl}/medical-records/create/{patientId}",
                new StringContent("{}", System.Text.Encoding.UTF8, "application/json")
            );

            response.EnsureSuccessStatusCode();
        }
        catch (Exception ex)
        {
            // Log the error but don't fail the patient creation
            // Consider implementing a retry mechanism or queue
            Console.WriteLine($"Failed to create medical record: {ex.Message}");
        }
    }
}