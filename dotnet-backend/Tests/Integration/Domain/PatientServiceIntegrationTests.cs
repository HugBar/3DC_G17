/*using Xunit;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.PatientData;
using DDDSample1.Infrastructure.PatientData;
using DDDSample1.Infrastructure;
using System.Threading.Tasks;
using System.Linq;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

public class PatientServiceIntegrationTests
{
    private readonly PatientService _patientService;
    private readonly IPatientRepository _patientRepo;
    private readonly IUnitOfWork _unitOfWork;

    public PatientServiceIntegrationTests()
    {
        // Setup in-memory database and other dependencies
        var options = new DbContextOptionsBuilder<DDDSample1DbContext>()
            .UseInMemoryDatabase(databaseName: "TestDatabase")
            .Options;

        var context = new DDDSample1DbContext(options);
        _patientRepo = new PatientRepository(context);
        _unitOfWork = new UnitOfWork(context);

        _patientService = new PatientService(_patientRepo, null, _unitOfWork, null, null);
    }

    [Fact]
    public async Task GetFilteredPatient_ValidFilter_ReturnsMatchingPatientDtos()
    {
        // Arrange
        var filter = new PatientFilterDTO
        {
            FirstName = "John"
        };

        var patient1 = new Patient("MED-123", "user-123", "John", "Doe", "john.doe@example.com", "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);
        var patient2 = new Patient("MED-456", "user-456", "Jane", "Doe", "jane.doe@example.com", "02/02/1991", "Female", "Contact", "Emergency", "9876543210", null, null);

        await _patientRepo.AddAsync(patient1);
        await _patientRepo.AddAsync(patient2);
        await _unitOfWork.CommitAsync();

        // Act
        var result = await _patientService.GetFilteredPatient(filter, 1, 5);

        // Assert
        Assert.NotNull(result);
        Assert.Single(result);
        Assert.Equal("John", result.First().FirstName);
    }
}
*/

