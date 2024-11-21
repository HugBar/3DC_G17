using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using DDDSample1.Domain.SurgeryRoomData;

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

        // Format the response to match the expected format by the frontend
        var response = new
        {
            id = room.Id,
            status = room.Status.ToString()
        };

        return Ok(response);
    }
}