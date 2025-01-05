using System;
using System.Net.Http;
using System.Threading.Tasks;
using System.Text;
using Newtonsoft.Json;
using Microsoft.Extensions.Configuration;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Domain.OperationRequestData;
using System.Collections.Generic;
using System.Linq;
using Microsoft.AspNetCore.Identity;
using Microsoft.Extensions.DependencyInjection;
using DDDSample1.Domain.UserData;

public class GeneticAlgorithmService
{
    private readonly HttpClient _httpClient;
    private readonly string _prologServerUrl;
    private readonly IStaffRepository _staffRepo;
    private readonly IOperationTypeRepository _operationTypeRepo;
    private readonly IOperationRequestRepository _operationRequestRepo;
    private readonly IServiceProvider _serviceProvider;

    public GeneticAlgorithmService(
        IConfiguration configuration,
        IStaffRepository staffRepo,
        IOperationTypeRepository operationTypeRepo,
        IOperationRequestRepository operationRequestRepo,
        IServiceProvider serviceProvider)
    {
        _httpClient = new HttpClient();
        _prologServerUrl = configuration["PrologServer:BaseUrl"] ?? "http://localhost:6605";
        _staffRepo = staffRepo;
        _operationTypeRepo = operationTypeRepo;
        _operationRequestRepo = operationRequestRepo;
        _serviceProvider = serviceProvider;
    }

    public async Task SynchronizeDataWithPrologServer()
    {
        var data = await GatherAllData();

        // Send staff availability
        foreach (var availability in data.StaffAvailability)
        {
            await SendToPrologServer("/agendaStaff", new
            {
                staffID = availability.StaffId,
                day = availability.Day,
                slots = availability.Slots
            });
        }

        // Send surgery requirements
        foreach (var requirement in data.SurgeryRequirements)
        {
            await SendToPrologServer("/surgery-requirements", new
            {
                surgeryType = requirement.SurgeryType,
                requirements = requirement.PhaseRequirements
            });
        }

        // Send staff information
        foreach (var staff in data.Staff)
        {
            await SendToPrologServer("/staff", new
            {
                staffID = staff.StaffId,
                role = staff.Role,
                specialization = staff.Speciality,
                oTs = staff.Operations
            });
        }

        // Send surgery information
        foreach (var surgery in data.Surgeries)
        {
            await SendToPrologServer("/surgery", new
            {
                surgeryID = surgery.SurgeryType,
                t1 = surgery.PreparationTime,
                t2 = surgery.SurgeryTime,
                t3 = surgery.CleaningTime
            });
        }

        // Send surgery IDs
        foreach (var surgeryId in data.SurgeryIds)
        {
            await SendToPrologServer("/surgeryId", new
            {
                oRID = surgeryId.OperationRequestId,
                surgeryID = surgeryId.SurgeryType
            });
        }

        // Send operation room schedules
        foreach (var schedule in data.OperationRoomSchedules)
        {
            await SendToPrologServer("/agendaOperationRoom", new
            {
                room = schedule.Room,
                day = schedule.Day,
                slots = schedule.Slots
            });
        }
    }

    private async Task<GeneticAlgorithmData> GatherAllData()
    {
        // Implement the logic to gather all required data from your repositories
        // This is a placeholder - you'll need to implement the actual data gathering
        return new GeneticAlgorithmData
        {
            StaffAvailability = await GetStaffAvailability(),
            SurgeryRequirements = await GetSurgeryRequirements(),
            Staff = await GetStaffInfo(),
            Surgeries = await GetSurgeryInfo(),
            SurgeryIds = await GetSurgeryIds(),
            OperationRoomSchedules = await GetOperationRoomSchedules()
        };
    }

    public async Task SendToPrologServer(string endpoint, object data)
    {
        var json = JsonConvert.SerializeObject(data);
        var content = new StringContent(json, Encoding.UTF8, "application/json");

        try
        {
            var response = await _httpClient.PostAsync($"{_prologServerUrl}{endpoint}", content);
            response.EnsureSuccessStatusCode();
        }
        catch (Exception ex)
        {
            throw new Exception($"Failed to send data to Prolog server: {ex.Message}");
        }
    }

    // Implement the following methods according to your data structure:
    private async Task<List<StaffAvailability>> GetStaffAvailability()
    {
        // Implementation needed
        throw new NotImplementedException();
    }

    private async Task<List<SurgeryRequirement>> GetSurgeryRequirements()
    {
        // Implementation needed
        throw new NotImplementedException();
    }

    public async Task<List<StaffInfo>> GetStaffInfo()
    {
        var staffList = await _staffRepo.GetAllAsync();
        var operationTypes = await _operationTypeRepo.GetAllAsync();
        var userManager = _serviceProvider.GetRequiredService<UserManager<ApplicationUser>>();

        var staffInfoList = new List<StaffInfo>();

        foreach (var staff in staffList)
        {
            // Get user role
            var user = await userManager.FindByIdAsync(staff.UserId);
            var roles = await userManager.GetRolesAsync(user);
            var role = roles.FirstOrDefault()?.ToLower() ?? "unknown";

            // Get compatible operations
            var compatibleOperations = operationTypes
                .Where(op => op.RequiredStaffBySpecialization.ContainsKey(staff.Specialization))
                .Select(op => op.Name.ToLower().Replace(" ", ""))
                .ToList();

            staffInfoList.Add(new StaffInfo
            {
                StaffId = staff.LicenseNumber,
                Role = role,
                Speciality = staff.Specialization.ToLower(),
                Operations = compatibleOperations
            });
        }

        return staffInfoList;
    }

    public async Task<List<SurgeryInfo>> GetSurgeryInfo()
    {
        var operationTypes = await _operationTypeRepo.GetAllAsync();
        return operationTypes.Select(op => new SurgeryInfo
        {
            SurgeryType = op.Name.ToLower().Replace(" ", ""),  // Convert "Heart Surgery" to "heartsurgery"
            PreparationTime = (int)op.Duration.AnesthesiaPreparation.TotalMinutes,
            SurgeryTime = (int)op.Duration.Surgery.TotalMinutes,
            CleaningTime = (int)op.Duration.Cleaning.TotalMinutes
        }).ToList();
    }

    public async Task<List<SurgeryId>> GetSurgeryIds()
    {
        var operationRequests = await _operationRequestRepo.GetAllAsync();
        var operationTypes = await _operationTypeRepo.GetAllAsync();

        var operationTypeMap = operationTypes.ToDictionary(
            ot => ot.Id.Value,
            ot => ot.Name.ToLower().Replace(" ", "")
        );

        return operationRequests.Select(request => new SurgeryId
        {
            OperationRequestId = request.Id,
            SurgeryType = operationTypeMap[request.OperationTypeId]
        }).ToList();
    }

    private async Task<List<OperationRoomSchedule>> GetOperationRoomSchedules()
    {
        // Implementation needed
        throw new NotImplementedException();
    }

    public async Task<string> TestConnection()
    {
        try
        {
            var response = await _httpClient.GetAsync($"{_prologServerUrl}/");
            return await response.Content.ReadAsStringAsync();
        }
        catch (Exception ex)
        {
            throw new Exception($"Failed to connect to Prolog server: {ex.Message}");
        }
    }

    public async Task<List<StaffTimetable>> GetStaffTimetables()
    {
        var staffList = await _staffRepo.GetAllAsync();
        var today = DateTime.Now.ToString("yyyyMMdd");  // Format: 20241028

        return staffList.Select(staff => new StaffTimetable
        {
            StaffId = staff.LicenseNumber,
            Day = today,
            TimeSlot = (480, 1200)  // Fixed time slot as per requirements
        }).ToList();
    }
}