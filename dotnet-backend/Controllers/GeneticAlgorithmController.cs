using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using System;

[Route("api/[controller]")]
[ApiController]
public class GeneticAlgorithmController : ControllerBase
{
    private readonly GeneticAlgorithmService _service;

    public GeneticAlgorithmController(GeneticAlgorithmService service)
    {
        _service = service;
    }

    [HttpPost("sync")]
    public async Task<IActionResult> SynchronizeData()
    {
        try
        {
            await _service.SynchronizeDataWithPrologServer();
            return Ok("Data successfully synchronized with Prolog server");
        }
        catch (Exception ex)
        {
            return StatusCode(500, $"An error occurred: {ex.Message}");
        }
    }

    [HttpGet("test")]
    public async Task<IActionResult> TestPrologConnection()
    {
        try
        {
            var response = await _service.TestConnection();
            return Ok($"Prolog server connection test: {response}");
        }
        catch (Exception ex)
        {
            return StatusCode(500, $"Connection failed: {ex.Message}");
        }
    }

    [HttpPost("test-sync")]
    public async Task<IActionResult> TestSynchronization()
    {
        try
        {
            // Test with a single staff member
            await _service.SendToPrologServer("/staff", new
            {
                staffID = "test001",
                role = "doctor",
                specialization = "orthopaedist",
                oTs = new[] { "so2", "so3", "so4" }
            });

            // Test with a single surgery requirement
            await _service.SendToPrologServer("/surgery", new
            {
                surgeryID = "so2",
                t1 = 45,
                t2 = 60,
                t3 = 45
            });

            // Add logging to verify the request
            Console.WriteLine("Test synchronization completed");
            return Ok(new { message = "Test synchronization completed successfully" });
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error during test sync: {ex.Message}");
            return StatusCode(500, new { error = ex.Message, details = ex.StackTrace });
        }
    }

    [HttpGet("test-surgery")]
    public async Task<IActionResult> TestSurgerySynchronization()
    {
        try
        {
            var surgeries = await _service.GetSurgeryInfo();

            foreach (var surgery in surgeries)
            {
                Console.WriteLine($"Sending surgery: {surgery.SurgeryType}");
                await _service.SendToPrologServer("/surgery", new
                {
                    surgeryID = surgery.SurgeryType,
                    t1 = surgery.PreparationTime,
                    t2 = surgery.SurgeryTime,
                    t3 = surgery.CleaningTime
                });
            }

            return Ok(new
            {
                message = "Surgery synchronization completed successfully",
                surgeries = surgeries
            });
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error during surgery sync: {ex.Message}");
            return StatusCode(500, new { error = ex.Message, details = ex.StackTrace });
        }
    }

    [HttpGet("test-surgery-ids")]
    public async Task<IActionResult> TestSurgeryIdsSynchronization()
    {
        try
        {
            var surgeryIds = await _service.GetSurgeryIds();

            foreach (var surgeryId in surgeryIds)
            {
                Console.WriteLine($"Sending surgery ID: {surgeryId.OperationRequestId} -> {surgeryId.SurgeryType}");
                await _service.SendToPrologServer("/surgeryId", new
                {
                    oRID = surgeryId.OperationRequestId,
                    surgeryID = surgeryId.SurgeryType
                });
            }

            return Ok(new
            {
                message = "Surgery IDs synchronization completed successfully",
                surgeryIds = surgeryIds
            });
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error during surgery IDs sync: {ex.Message}");
            return StatusCode(500, new { error = ex.Message, details = ex.StackTrace });
        }
    }

    [HttpGet("test-staff")]
    public async Task<IActionResult> TestStaffSynchronization()
    {
        try
        {
            var staffList = await _service.GetStaffInfo();

            foreach (var staff in staffList)
            {
                Console.WriteLine($"Sending staff: {staff.StaffId} ({staff.Role} - {staff.Speciality})");
                await _service.SendToPrologServer("/staff", new
                {
                    staffID = staff.StaffId,
                    role = staff.Role,
                    specialization = staff.Speciality,
                    oTs = staff.Operations
                });
            }

            return Ok(new
            {
                message = "Staff synchronization completed successfully",
                staff = staffList
            });
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error during staff sync: {ex.Message}");
            return StatusCode(500, new { error = ex.Message, details = ex.StackTrace });
        }
    }

    [HttpGet("test-timetable")]
    public async Task<IActionResult> TestTimetableSynchronization()
    {
        try
        {
            var timetables = await _service.GetStaffTimetables();

            foreach (var timetable in timetables)
            {
                Console.WriteLine($"Sending timetable for staff: {timetable.StaffId}");
                await _service.SendToPrologServer("/timetable", new
                {
                    staffID = timetable.StaffId,
                    day = timetable.Day,
                    start_time = timetable.TimeSlot.Start,
                    end_time = timetable.TimeSlot.End
                });
            }

            return Ok(new
            {
                message = "Timetable synchronization completed successfully",
                timetables = timetables
            });
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error during timetable sync: {ex.Message}");
            return StatusCode(500, new { error = ex.Message, details = ex.StackTrace });
        }
    }
}