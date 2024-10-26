using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.OperationRequestData;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.PatientData;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;
using System.Security.Claims;
using Microsoft.AspNetCore.Http;

namespace DDDSample1.Tests.Unit.Controllers
{
    public class OperationRequestControllerTests
    {
        private readonly Mock<IOperationRequestRepository> _mockOperationRequestRepo;
        private readonly Mock<IUnitOfWork> _mockUnitOfWork;
        private readonly Mock<IStaffRepository> _mockStaffRepo;
        private readonly Mock<IPatientRepository> _mockPatientRepo;
        private readonly Mock<ILoggingService> _mockLoggingService;
        private readonly Mock<IOperationTypeRepository> _mockOperationTypeRepo;
        private readonly Mock<ILogger<OperationRequestController>> _mockLogger;
        private readonly OperationRequestService _operationRequestService;
        private readonly OperationRequestController _controller;

        public OperationRequestControllerTests()
        {
            _mockOperationRequestRepo = new Mock<IOperationRequestRepository>();
            _mockUnitOfWork = new Mock<IUnitOfWork>();
            _mockStaffRepo = new Mock<IStaffRepository>();
            _mockPatientRepo = new Mock<IPatientRepository>();
            _mockLoggingService = new Mock<ILoggingService>();
            _mockOperationTypeRepo = new Mock<IOperationTypeRepository>();
            _mockLogger = new Mock<ILogger<OperationRequestController>>();

            _operationRequestService = new OperationRequestService(
                _mockOperationRequestRepo.Object,
                _mockUnitOfWork.Object,
                _mockStaffRepo.Object,
                _mockPatientRepo.Object,
                _mockLoggingService.Object,
                _mockOperationTypeRepo.Object
            );

            _controller = new OperationRequestController(_operationRequestService, _mockLogger.Object);
        }

        [Fact]
        public async Task CreateOperationRequest_ValidData_ReturnsOkResult()
        {
            var createDto = new CreateOperationRequestDto
            {
                PatientId = "patient-123",
                DoctorId = "doctor-123",
                OperationTypeId = "3fa85f64-5717-4562-b3fc-2c963f66afa6",
                Deadline = DateTime.Now.AddDays(7),
                Priority = "urgent"
            };

            var operationTypeId = new OperationTypeId(createDto.OperationTypeId);
            var operationType = new OperationType(
                "General Surgery",
                new Dictionary<string, int> { { "Surgery", 1 } },
                new OperationPhases(TimeSpan.FromMinutes(30), TimeSpan.FromHours(2), TimeSpan.FromMinutes(30))
            );

            var doctor = new Staff("doctor-123", "Dr.", "Smith", "doctor@example.com", "987654321", "Surgery", "LIC-12345");
            var patient = new Patient("MED-123456", "patient-123", "John", "Doe", "patient@example.com", "1990-01-01", "Male", "123 Main St", "Jane Doe", "123456789");

            _mockOperationTypeRepo.Setup(repo => repo.GetByIdAsync(It.Is<OperationTypeId>(id => id.AsString() == createDto.OperationTypeId)))
                .ReturnsAsync(operationType);
            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(createDto.DoctorId)).ReturnsAsync(doctor);
            _mockPatientRepo.Setup(repo => repo.ExistsAsync(createDto.PatientId)).ReturnsAsync(true);

            var createdOperationRequest = new OperationRequest(
                "request-123",
                createDto.PatientId,
                createDto.DoctorId,
                createDto.OperationTypeId,
                createDto.Deadline,
                createDto.Priority
            );

            _mockOperationRequestRepo.Setup(repo => repo.AddAsync(It.IsAny<OperationRequest>())).ReturnsAsync(createdOperationRequest);
            _mockUnitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            var result = await _controller.CreateOperationRequest(createDto);

            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<OperationRequestDto>(okResult.Value);

            Assert.False(string.IsNullOrEmpty(returnValue.Id));
            Assert.Equal(createdOperationRequest.PatientId, returnValue.PatientId);
            Assert.Equal(createdOperationRequest.DoctorId, returnValue.DoctorId);
            Assert.Equal(createdOperationRequest.OperationTypeId, returnValue.OperationTypeId);
            Assert.Equal(createdOperationRequest.Priority, returnValue.Priority);

            _mockOperationTypeRepo.Verify(repo => repo.GetByIdAsync(It.Is<OperationTypeId>(id => id.AsString() == createDto.OperationTypeId)), Times.Once);
            _mockStaffRepo.Verify(repo => repo.GetByIdAsync(createDto.DoctorId), Times.Once);
            _mockPatientRepo.Verify(repo => repo.ExistsAsync(createDto.PatientId), Times.Once);
            _mockOperationRequestRepo.Verify(repo => repo.AddAsync(It.IsAny<OperationRequest>()), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task DeleteOperationRequest_ValidId_ReturnsOkResult()
        {
            var id = "request-123";
            var doctorEmail = "doctor@example.com";

            var userClaims = new List<Claim> { new Claim(ClaimTypes.Email, doctorEmail) };
            var identity = new ClaimsIdentity(userClaims, "TestAuth");
            var userPrincipal = new ClaimsPrincipal(identity);
            _controller.ControllerContext = new ControllerContext { HttpContext = new DefaultHttpContext { User = userPrincipal } };

            var doctor = new Staff("doctor-123", "Dr.", "Smith", doctorEmail, "987654321", "Surgery", "LIC-12345");
            var operationRequest = new OperationRequest(id, "patient-123", "doctor-123", "3fa85f64-5717-4562-b3fc-2c963f66afa6", DateTime.Now.AddDays(7), "urgent");

            _mockStaffRepo.Setup(repo => repo.GetByEmailAsync(doctorEmail)).ReturnsAsync(doctor);
            _mockOperationRequestRepo.Setup(repo => repo.GetByIdAsync(id)).ReturnsAsync(operationRequest);
            _mockOperationRequestRepo.Setup(repo => repo.RemoveAsync(operationRequest)).Returns(Task.CompletedTask);
            _mockUnitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            var result = await _controller.DeleteOperationRequest(id);

            var okResult = Assert.IsType<OkObjectResult>(result);
            Assert.Equal("Operation request deleted successfully.", okResult.Value);

            _mockStaffRepo.Verify(repo => repo.GetByEmailAsync(doctorEmail), Times.Once);
            _mockOperationRequestRepo.Verify(repo => repo.GetByIdAsync(id), Times.Once);
            _mockOperationRequestRepo.Verify(repo => repo.RemoveAsync(operationRequest), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task SearchOperationRequests_ValidSearchCriteria_ReturnsOkResult()
        {
            var searchDto = new SearchOperationRequestDto
            {
                PatientId = "patient-123",
                OperationTypeId = "3fa85f64-5717-4562-b3fc-2c963f66afa6",
                Priority = "urgent"
            };

            var operationRequests = new List<OperationRequest>
            {
                new OperationRequest("request-123", "patient-123", "doctor-123", "3fa85f64-5717-4562-b3fc-2c963f66afa6", DateTime.Now.AddDays(7), "urgent"),
                new OperationRequest("request-456", "patient-123", "doctor-789", "3fa85f64-5717-4562-b3fc-2c963f66afa6", DateTime.Now.AddDays(5), "urgent")
            };

            _mockOperationRequestRepo.Setup(repo => repo.GetFilteredOperationRequestsAsync(It.IsAny<SearchOperationRequestDto>())).ReturnsAsync(operationRequests);

            var result = await _controller.SearchOperationRequests(searchDto);

            var actionResult = Assert.IsType<ActionResult<IEnumerable<OperationRequestDto>>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<List<OperationRequestDto>>(okResult.Value);

            Assert.Equal(2, returnValue.Count);
            Assert.All(returnValue, r => Assert.Equal("patient-123", r.PatientId));
            Assert.All(returnValue, r => Assert.Equal("urgent", r.Priority));

            _mockOperationRequestRepo.Verify(repo => repo.GetFilteredOperationRequestsAsync(It.IsAny<SearchOperationRequestDto>()), Times.Once);
        }
    }
}
