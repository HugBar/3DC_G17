/*
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRequestData;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Domain.PatientData;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffData;
using Moq;
using Xunit;

namespace DDDSample1.Tests.Unit.Services
{
    public class OperationRequestServiceTests
    {
        private readonly Mock<IOperationRequestRepository> _mockOperationRequestRepo;
        private readonly Mock<IUnitOfWork> _mockUnitOfWork;
        private readonly Mock<IStaffRepository> _mockStaffRepo;
        private readonly Mock<IPatientRepository> _mockPatientRepo;
        private readonly Mock<ILoggingService> _mockLoggingService;
        private readonly Mock<IOperationTypeRepository> _mockOperationTypeRepo;
        private readonly OperationRequestService _service;

        public OperationRequestServiceTests()
        {
            _mockOperationRequestRepo = new Mock<IOperationRequestRepository>();
            _mockUnitOfWork = new Mock<IUnitOfWork>();
            _mockStaffRepo = new Mock<IStaffRepository>();
            _mockPatientRepo = new Mock<IPatientRepository>();
            _mockLoggingService = new Mock<ILoggingService>();
            _mockOperationTypeRepo = new Mock<IOperationTypeRepository>();

            _service = new OperationRequestService(
                _mockOperationRequestRepo.Object,
                _mockUnitOfWork.Object,
                _mockStaffRepo.Object,
                _mockPatientRepo.Object,
                _mockLoggingService.Object,
                _mockOperationTypeRepo.Object
            );
        }

        [Fact]
        public async Task CreateOperationRequest_ValidData_ReturnsOperationRequestDto()
        {
            var createDto = new CreateOperationRequestDto
            {
                PatientId = "patient-123",
                DoctorId = "doctor-123",
                OperationTypeId = "3fa85f64-5717-4562-b3fc-2c963f66afa6",
                Deadline = DateTime.Now.AddDays(7),
                Priority = "urgent"
            };

            _mockPatientRepo.Setup(repo => repo.ExistsAsync(createDto.PatientId)).ReturnsAsync(true);

            var doctor = new Staff("doctor-123", "Dr.", "Smith", "doctor@example.com", "987654321", "Surgery", "LIC-12345");
            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(createDto.DoctorId)).ReturnsAsync(doctor);

            var operationType = new OperationType(
                "General Surgery",
                new Dictionary<string, int> { { "Surgery", 1 } },
                new OperationPhases(TimeSpan.FromMinutes(30), TimeSpan.FromHours(2), TimeSpan.FromMinutes(30))
            );
            _mockOperationTypeRepo.Setup(repo => repo.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationType);

            var operationRequest = new OperationRequest("OP-F657DB07", createDto.PatientId, createDto.DoctorId, createDto.OperationTypeId, createDto.Deadline, createDto.Priority);
            _mockOperationRequestRepo.Setup(repo => repo.AddAsync(It.IsAny<OperationRequest>())).ReturnsAsync(operationRequest);
            _mockUnitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            var result = await _service.CreateOperationRequestAsync(createDto);

            Assert.NotNull(result);
            Assert.StartsWith("OP-", result.Id);
            Assert.Equal(createDto.PatientId, result.PatientId);
            Assert.Equal(createDto.DoctorId, result.DoctorId);
            Assert.Equal(createDto.OperationTypeId, result.OperationTypeId);
            Assert.Equal(createDto.Priority, result.Priority);

            _mockPatientRepo.Verify(repo => repo.ExistsAsync(createDto.PatientId), Times.Once);
            _mockStaffRepo.Verify(repo => repo.GetByIdAsync(createDto.DoctorId), Times.Once);
            _mockOperationRequestRepo.Verify(repo => repo.AddAsync(It.IsAny<OperationRequest>()), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task DeleteOperationRequest_ValidId_ReturnsTrue()
        {
            var id = "request-123";
            var doctorEmail = "doctor@example.com";

            var doctor = new Staff("doctor-123", "Dr.", "Smith", doctorEmail, "987654321", "Surgery", "LIC-12345");
            _mockStaffRepo.Setup(repo => repo.GetByEmailAsync(doctorEmail)).ReturnsAsync(doctor);

            var operationRequest = new OperationRequest(id, "patient-123", "doctor-123", "3fa85f64-5717-4562-b3fc-2c963f66afa6", DateTime.Now.AddDays(7), "urgent");
            _mockOperationRequestRepo.Setup(repo => repo.GetByIdAsync(id)).ReturnsAsync(operationRequest);

            _mockOperationRequestRepo.Setup(repo => repo.RemoveAsync(operationRequest)).Returns(Task.CompletedTask);
            _mockUnitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            var result = await _service.DeleteOperationRequestAsync(id, doctorEmail);

            Assert.True(result);

            _mockOperationRequestRepo.Verify(repo => repo.GetByIdAsync(id), Times.Once);
            _mockOperationRequestRepo.Verify(repo => repo.RemoveAsync(operationRequest), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task SearchOperationRequests_ValidSearchCriteria_ReturnsMatchingRequests()
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

            _mockOperationRequestRepo.Setup(repo => repo.GetFilteredOperationRequestsAsync(It.Is<SearchOperationRequestDto>(dto =>
                dto.PatientId == searchDto.PatientId &&
                dto.OperationTypeId == searchDto.OperationTypeId &&
                dto.Priority == searchDto.Priority
            ))).ReturnsAsync(operationRequests);

            var result = await _service.SearchOperationRequestsAsync(searchDto);

            Assert.NotNull(result);
            Assert.Equal(2, result.Count());
            Assert.All(result, r => Assert.Equal("patient-123", r.PatientId));
            Assert.All(result, r => Assert.Equal("urgent", r.Priority));
            Assert.All(result, r => Assert.Equal("3fa85f64-5717-4562-b3fc-2c963f66afa6", r.OperationTypeId));

            _mockOperationRequestRepo.Verify(repo => repo.GetFilteredOperationRequestsAsync(It.IsAny<SearchOperationRequestDto>()), Times.Once);
        }
    }
}
*/