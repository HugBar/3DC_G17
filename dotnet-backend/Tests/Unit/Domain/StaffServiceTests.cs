using System;
using System.Threading.Tasks;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Identity;
using Moq;
using Xunit;
using System.Collections.Generic;
using System.Linq;

namespace DDDSample1.Tests.Unit.Domain
{
    public class StaffServiceTests
    {
        private readonly Mock<IStaffRepository> _mockStaffRepo;
        private readonly Mock<UserManager<ApplicationUser>> _mockUserManager;
        private readonly Mock<IUnitOfWork> _mockUnitOfWork;
        private readonly Mock<ILoggingService> _mockLoggingService;
        private readonly Mock<IEmailService> _mockEmailService;
        private readonly StaffService _staffService;

        public StaffServiceTests()
        {
            _mockStaffRepo = new Mock<IStaffRepository>();
            _mockUserManager = new Mock<UserManager<ApplicationUser>>(
                Mock.Of<IUserStore<ApplicationUser>>(), null, null, null, null, null, null, null, null
            );
            _mockUnitOfWork = new Mock<IUnitOfWork>();
            _mockLoggingService = new Mock<ILoggingService>();
            _mockEmailService = new Mock<IEmailService>();

            _staffService = new StaffService(
                _mockStaffRepo.Object,
                _mockUserManager.Object,
                _mockUnitOfWork.Object,
                _mockLoggingService.Object,
                _mockEmailService.Object
            );
        }

        [Fact]
        public async Task CreateStaff_ValidData_ReturnsSuccess()
        {
            var createStaffDto = new CreateStaffDto
            {
                FirstName = "John",
                LastName = "Doe",
                Email = "john.doe@example.com",
                PhoneNumber = "123456789"
            };

            var staff = new Staff("staff-123", "John", "Doe", "john.doe@example.com", "123456789", "Doctor", "LIC-12345");

            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);
            _mockStaffRepo.Setup(repo => repo.IsPhoneNumberUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);

            var user = new ApplicationUser { Id = "user-123", Email = createStaffDto.Email };
            _mockUserManager.Setup(um => um.FindByEmailAsync(It.IsAny<string>())).ReturnsAsync(user);

            _mockStaffRepo.Setup(repo => repo.AddAsync(It.IsAny<Staff>())).ReturnsAsync(staff);
            _mockUnitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            var result = await _staffService.AddAsync(createStaffDto);

            Assert.NotNull(result);
            Assert.Equal("john.doe@example.com", result.Email);
            _mockStaffRepo.Verify(repo => repo.AddAsync(It.IsAny<Staff>()), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task CreateStaff_DuplicateEmail_ThrowsException()
        {
            var dto = new CreateStaffDto
            {
                Email = "test@example.com",
                PhoneNumber = "1234567890",
                FirstName = "John",
                LastName = "Doe",
                Specialization = "Doctor",
                AvailabilitySlots = new List<AvailabilitySlot>()
            };

            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);

            await Assert.ThrowsAsync<InvalidOperationException>(() => _staffService.AddAsync(dto));
        }

        [Fact]
        public async Task CreateStaff_DuplicatePhoneNumber_ThrowsException()
        {
            var dto = new CreateStaffDto
            {
                Email = "test@example.com",
                PhoneNumber = "1234567890",
                FirstName = "John",
                LastName = "Doe",
                Specialization = "Doctor",
                AvailabilitySlots = new List<AvailabilitySlot>()
            };

            _mockStaffRepo.Setup(repo => repo.IsPhoneNumberUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);

            await Assert.ThrowsAsync<InvalidOperationException>(() => _staffService.AddAsync(dto));
        }

        [Fact]
        public async Task CreateStaff_RepositoryAddFails_ThrowsException()
        {
            var createStaffDto = new CreateStaffDto
            {
                FirstName = "John",
                LastName = "Doe",
                Email = "existing.email@example.com",
                PhoneNumber = "123456789"
            };

            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync(It.IsAny<string>())).ReturnsAsync(false);

            await Assert.ThrowsAsync<InvalidOperationException>(() => _staffService.AddAsync(createStaffDto));
        }

        [Fact]
        public async Task CreateStaff_NoAvailabilitySlots_CreatesSuccessfully()
        {
            var createStaffDto = new CreateStaffDto
            {
                FirstName = "John",
                LastName = "Doe",
                Email = "john.doe@example.com",
                PhoneNumber = "123456789",
                Specialization = "Doctor",
                AvailabilitySlots = new List<AvailabilitySlot>()
            };

            var staff = new Staff("staff-123", "John", "Doe", "john.doe@example.com", "123456789", "Doctor", "LIC-12345");

            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);
            _mockStaffRepo.Setup(repo => repo.IsPhoneNumberUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);

            var user = new ApplicationUser { Id = "user-123", Email = createStaffDto.Email };
            _mockUserManager.Setup(um => um.FindByEmailAsync(It.IsAny<string>())).ReturnsAsync(user);
            _mockStaffRepo.Setup(repo => repo.AddAsync(It.IsAny<Staff>())).ReturnsAsync(staff);
            _mockUnitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            var result = await _staffService.AddAsync(createStaffDto);

            Assert.NotNull(result);
            _mockStaffRepo.Verify(repo => repo.AddAsync(It.IsAny<Staff>()), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task UpdateStaff_ValidData_UpdatesSuccessfully()
        {
            var staffId = "staff-123";
            var staff = new Staff(staffId, "John", "Doe", "test@example.com", "123456789", "Doctor", "LIC-12345");
            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(It.IsAny<string>())).ReturnsAsync(staff);

            var updateStaffDto = new UpdateStaffDto
            {
                Email = "newemail@example.com",
                PhoneNumber = "0987654321",
                Specialization = "Surgeon"
            };

            _mockStaffRepo.Setup(repo => repo.IsEmailUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);
            _mockStaffRepo.Setup(repo => repo.IsPhoneNumberUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);

            _mockUserManager.Setup(um => um.FindByEmailAsync(It.IsAny<string>())).ReturnsAsync(new ApplicationUser { Email = "test@example.com" });
            _mockUserManager.Setup(um => um.UpdateAsync(It.IsAny<ApplicationUser>())).ReturnsAsync(IdentityResult.Success);

            var result = await _staffService.UpdateStaffAsync(staffId, updateStaffDto);

            Assert.Equal("newemail@example.com", result.Email);
            Assert.Equal("0987654321", result.PhoneNumber);
            _mockStaffRepo.Verify(repo => repo.UpdateAsync(It.IsAny<Staff>()), Times.Once);
            _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task UpdateStaff_StaffNotFound_ThrowsNotFoundException()
        {
            var staffId = "staff-123";
            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(It.IsAny<string>())).ReturnsAsync((Staff)null);

            var updateStaffDto = new UpdateStaffDto
            {
                Email = "newemail@example.com",
                PhoneNumber = "0987654321",
                Specialization = "Surgeon"
            };

            await Assert.ThrowsAsync<NotFoundException>(() => _staffService.UpdateStaffAsync(staffId, updateStaffDto));
        }

        [Fact]
        public async Task UpdateStaff_OverlappingAvailabilitySlots_ThrowsBusinessRuleValidationException()
        {
            var staffId = "staff-123";
            var staff = new Staff(staffId, "John", "Doe", "test@example.com", "123456789", "Doctor", "LIC-12345");
            _mockStaffRepo.Setup(repo => repo.GetByIdAsync(It.IsAny<string>())).ReturnsAsync(staff);

            var updateStaffDto = new UpdateStaffDto
            {
                Email = "newemail@example.com",
                PhoneNumber = "0987654321",
                Specialization = "Surgeon",
                AvailabilitySlots = new List<AvailabilitySlot>
                {
                    new AvailabilitySlot(DateTime.Now, DateTime.Now.AddHours(2)),
                    new AvailabilitySlot(DateTime.Now.AddHours(1), DateTime.Now.AddHours(3))
                }
            };

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _staffService.UpdateStaffAsync(staffId, updateStaffDto));
        }

        [Fact]
        public async Task GetStaffFilteredAsync_ValidFilter_ReturnsMatchingStaffDtos()
        {
            // Arrange
            var filter = new StaffFilterDto
            {
                FirstName = "John"
            };

            var staffs = new List<Staff>
            {
                new Staff("staff-123", "John", "Doe", "john.doe@example.com", "123456789", "Doctor", "LIC-12345"),
                new Staff("staff-456", "Jane", "Doe", "jane.doe@example.com", "987654321", "Doctor", "LIC-67890")
            };

            _mockStaffRepo.Setup(repo => repo.GetFilteredStaffAsync(It.Is<StaffFilterDto>(f =>
                f.FirstName == filter.FirstName
            ), 1, 10)).ReturnsAsync((staffs.Where(s => s.FirstName == filter.FirstName).ToList(), staffs.Count));

            // Act
            var result = await _staffService.getStaffFilteredAsync(filter, 1, 10);

            // Assert
            Assert.NotNull(result);
            Assert.Single(result.Items);
            Assert.All(result.Items, s => Assert.Equal("John", s.FirstName));
            _mockStaffRepo.Verify(repo => repo.GetFilteredStaffAsync(It.IsAny<StaffFilterDto>(), 1, 10), Times.Once);
        }
    }
}
