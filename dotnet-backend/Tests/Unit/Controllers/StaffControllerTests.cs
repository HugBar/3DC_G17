using System;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;
using Microsoft.AspNetCore.JsonPatch;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Identity;
using System.Collections.Generic;
using System.Linq;

namespace DDDSample1.Tests.Unit.Controllers
{
    public class StaffControllerTests
    {
        private readonly Mock<IStaffRepository> _mockStaffRepo;
        private readonly Mock<UserManager<ApplicationUser>> _mockUserManager;
        private readonly Mock<IUnitOfWork> _mockUnitOfWork;
        private readonly Mock<ILoggingService> _mockLoggingService;
        private readonly Mock<IEmailService> _mockEmailService;
        private readonly Mock<ILogger<StaffController>> _mockLogger;
        private readonly StaffService _staffService;
        private readonly StaffController _controller;

        public StaffControllerTests()
        {
            _mockStaffRepo = new Mock<IStaffRepository>();
            _mockUserManager = new Mock<UserManager<ApplicationUser>>(
                Mock.Of<IUserStore<ApplicationUser>>(), null, null, null, null, null, null, null, null
            );
            _mockUnitOfWork = new Mock<IUnitOfWork>();
            _mockLoggingService = new Mock<ILoggingService>();
            _mockEmailService = new Mock<IEmailService>();
            _mockLogger = new Mock<ILogger<StaffController>>();

            _staffService = new StaffService(
                _mockStaffRepo.Object,
                _mockUserManager.Object,
                _mockUnitOfWork.Object,
                _mockLoggingService.Object,
                _mockEmailService.Object
            );

            _controller = new StaffController(_staffService, _mockLogger.Object);
        }

        [Fact]
        public async Task CreateStaffProfile_ValidData_ReturnsOkResult()
        {
            var createStaffDto = new CreateStaffDto
            {
                FirstName = "John",
                LastName = "Doe",
                Email = "john.doe@example.com",
                PhoneNumber = "123456789",
                Specialization = "Doctor"
            };

            var staff = new Staff("staff-123", "John", "Doe", "john.doe@example.com", "123456789", "Doctor", "LIC-12345");

            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);
            _mockStaffRepo.Setup(repo => repo.IsPhoneNumberUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);

            var user = new ApplicationUser { Id = "user-123", Email = createStaffDto.Email };
            _mockUserManager.Setup(um => um.FindByEmailAsync(It.IsAny<string>())).ReturnsAsync(user);

            _mockStaffRepo.Setup(repo => repo.AddAsync(It.IsAny<Staff>())).ReturnsAsync(staff);
            _mockUnitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            var result = await _controller.CreateStaffProfile(createStaffDto);

            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<StaffDto>(okResult.Value);
            Assert.Equal("john.doe@example.com", returnValue.Email);

            _mockStaffRepo.Verify(repo => repo.AddAsync(It.IsAny<Staff>()), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task CreateStaffProfile_InvalidModelState_ReturnsBadRequest()
        {
            _controller.ModelState.AddModelError("FirstName", "Required");

            var createStaffDto = new CreateStaffDto
            {
                LastName = "Doe",
                Email = "john.doe@example.com",
                PhoneNumber = "123456789",
                Specialization = "Doctor"
            };

            var result = await _controller.CreateStaffProfile(createStaffDto);

            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task CreateStaffProfile_EmailAlreadyExists_ReturnsConflict()
        {
            var createStaffDto = new CreateStaffDto
            {
                FirstName = "John",
                LastName = "Doe",
                Email = "existing.email@example.com",
                PhoneNumber = "123456789",
                Specialization = "Doctor"
            };

            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync(It.IsAny<string>())).ReturnsAsync(false);

            var result = await _controller.CreateStaffProfile(createStaffDto);

            var conflictResult = Assert.IsType<ConflictObjectResult>(result);
            Assert.Equal("Email already exists.", (conflictResult.Value as dynamic).Message);
        }

        [Fact]
        public async Task UpdateStaffProfile_ValidData_ReturnsOkResult()
        {
            var staffId = "staff-123";
            var patchDoc = new JsonPatchDocument<UpdateStaffDto>();

            var initialStaff = new Staff(staffId, "John", "Doe", "test@example.com", "123456789", "Doctor", "LIC-12345");
            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(staffId)).ReturnsAsync(initialStaff);

            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync("new.email@example.com")).ReturnsAsync(true);
            _mockStaffRepo.Setup(repo => repo.IsPhoneNumberUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);

            var currentUser = new ApplicationUser { Id = staffId, Email = "test@example.com" };
            _mockUserManager.Setup(um => um.FindByEmailAsync("test@example.com")).ReturnsAsync(currentUser);

            _mockUserManager.Setup(um => um.UpdateAsync(It.IsAny<ApplicationUser>())).ReturnsAsync(IdentityResult.Success);

            _mockStaffRepo.Setup(repo => repo.UpdateAsync(It.IsAny<Staff>())).ReturnsAsync((Staff staff) => staff);
            _mockUnitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            patchDoc.Replace(x => x.Email, "new.email@example.com");
            patchDoc.Replace(x => x.PhoneNumber, "987654321");

            var result = await _controller.UpdateStaffProfile(staffId, patchDoc);

            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<StaffDto>(okResult.Value);
            Assert.Equal("new.email@example.com", returnValue.Email);
            Assert.Equal("987654321", returnValue.PhoneNumber);

            _mockStaffRepo.Verify(repo => repo.GetByIdAsync(staffId), Times.Exactly(2));
            _mockStaffRepo.Verify(repo => repo.UpdateAsync(It.IsAny<Staff>()), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task UpdateStaffProfile_StaffNotFound_ReturnsNotFound()
        {
            var staffId = "non-existent-id";
            var patchDoc = new JsonPatchDocument<UpdateStaffDto>();

            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(staffId)).ReturnsAsync((Staff)null);

            var result = await _controller.UpdateStaffProfile(staffId, patchDoc);

            Assert.IsType<NotFoundObjectResult>(result);
        }

        [Fact]
        public async Task UpdateStaffProfile_NullPatchDocument_ReturnsBadRequest()
        {
            var staffId = "staff-123";
            JsonPatchDocument<UpdateStaffDto> patchDoc = null;

            var result = await _controller.UpdateStaffProfile(staffId, patchDoc);

            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task UpdateStaffProfile_EmailAlreadyExists_ReturnsConflict()
        {
            var staffId = "staff-123";
            var patchDoc = new JsonPatchDocument<UpdateStaffDto>();

            var initialStaff = new Staff(staffId, "John", "Doe", "test@example.com", "123456789", "Doctor", "LIC-12345");
            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(staffId)).ReturnsAsync(initialStaff);
            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync("new.email@example.com")).ReturnsAsync(false);

            patchDoc.Replace(x => x.Email, "new.email@example.com");

            var result = await _controller.UpdateStaffProfile(staffId, patchDoc);

            var conflictResult = Assert.IsType<ConflictObjectResult>(result);
            Assert.Equal("Email is already in use.", (conflictResult.Value as dynamic).Message);
        }

        [Fact]
        public async Task UpdateStaffProfile_InvalidModelState_ReturnsBadRequest()
        {
            var staffId = "staff-123";
            var patchDoc = new JsonPatchDocument<UpdateStaffDto>();
            _controller.ModelState.AddModelError("Email", "Invalid format");

            var initialStaff = new Staff(staffId, "John", "Doe", "test@example.com", "123456789", "Doctor", "LIC-12345");
            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(staffId)).ReturnsAsync(initialStaff);

            var result = await _controller.UpdateStaffProfile(staffId, patchDoc);

            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task GetStaffs_ReturnsOkResult_WithListOfStaffs()
        {
            // Arrange
            var filter = new StaffFilterDto
            {
                Specialization = "Doctor"
            };
            var staffs = new List<Staff>
            {
                new Staff("staff-123", "John", "Doe", "john.doe@example.com", "123456789", "Medic", "LIC-12345"),
                new Staff("staff-456", "Jane", "Doe", "jane.doe@example.com", "987654321", "Doctor", "LIC-67890")
            };
            _mockStaffRepo.Setup(r => r.GetFilteredStaffAsync(filter)).ReturnsAsync(staffs.Where(s => s.Specialization == filter.Specialization).ToList());

            // Act
            var result = await _controller.GetStaffs(filter);

            // Assert
            var actionResult = Assert.IsType<ActionResult<IEnumerable<StaffDto>>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnStaffs = Assert.IsType<List<StaffDto>>(okResult.Value);
            Assert.Single(returnStaffs);
            Assert.All(returnStaffs, s => Assert.Equal("Doctor", s.Specialization));

            _mockStaffRepo.Verify(r => r.GetFilteredStaffAsync(filter), Times.Once);
        }

        [Fact]
        public async Task GetStaffs_ReturnsNotFound_WhenNoStaffsFound()
        {
            var filter = new StaffFilterDto { Specialization = "Doctor" };
            _mockStaffRepo.Setup(r => r.GetFilteredStaffAsync(filter)).ReturnsAsync(new List<Staff>());

            var result = await _controller.GetStaffs(filter);
            var actionResult = Assert.IsType<ActionResult<IEnumerable<StaffDto>>>(result);
            Assert.IsType<NotFoundResult>(result.Result);
        }


    }
}
