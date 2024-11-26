/*using Xunit;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure.Staffs;
using DDDSample1.Infrastructure;
using System.Threading.Tasks;
using System.Linq;
// Replace with the actual namespace

public class StaffServiceIntegrationTests
{
    private readonly StaffService _staffService;
    private readonly IStaffRepository _staffRepo;
    private readonly IUnitOfWork _unitOfWork;

    public StaffServiceIntegrationTests()
    {
        // Setup in-memory database and other dependencies
        var options = new DbContextOptionsBuilder<DDDSample1DbContext>()
            .UseInMemoryDatabase(databaseName: "TestDatabase")
            .Options;

        var context = new DDDSample1DbContext(options);
        _staffRepo = new StaffRepository(context);
        _unitOfWork = new UnitOfWork(context);

        _staffService = new StaffService(_staffRepo, null, _unitOfWork, null, null);
    }

    [Fact]
    public async Task GetStaffFilteredAsync_ValidFilter_ReturnsMatchingStaffDtos()
    {
        // Arrange
        var filter = new StaffFilterDto
        {
            Specialization = "Doctor"
        };

        var staff1 = new Staff("staff-123", "John", "Doe", "john.doe@example.com", "123456789", "Doctor", "LIC-12345");
        var staff2 = new Staff("staff-456", "Jane", "Doe", "jane.doe@example.com", "987654321", "Doctor", "LIC-67890");

        await _staffRepo.AddAsync(staff1);
        await _staffRepo.AddAsync(staff2);
        await _unitOfWork.CommitAsync();

        // Act
        var result = await _staffService.getStaffFilteredAsync(filter);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(2, result.Count());
        Assert.All(result, s => Assert.Equal("Doctor", s.Specialization));
    }
}
*/


