using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using DDDSample1.Domain.SurgeryRoomData;
using System.Linq;

[ApiController]
[Route("api/surgery-room")]
public class SurgeryRoomController : ControllerBase
{
    private readonly ISurgeryRoomRepository _surgeryRoomRepository;

    public SurgeryRoomController(ISurgeryRoomRepository surgeryRoomRepository)
    {
        _surgeryRoomRepository = surgeryRoomRepository;
    }

    [HttpGet("{id}")]
    public async Task<ActionResult<object>> GetRoomStatus(string id)
    {
        var room = await _surgeryRoomRepository.GetByIdAsync(id);
        if (room == null)
        {
            return NotFound();
        }

        // Return complete room data
        var response = new
        {
            id = room.Id,
            type = room.Type.ToString(),
            capacity = room.Capacity,
            assignedEquipment = room.AssignedEquipment,
            status = room.Status.ToString(),
            maintenanceSlots = room.MaintenanceSlots.Select(slot => new
            {
                startTime = slot.StartTime,
                endTime = slot.EndTime
            })
        };

        return Ok(response);
    }
}