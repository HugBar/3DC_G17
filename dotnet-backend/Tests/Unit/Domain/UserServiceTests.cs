using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.UserData;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Identity;
using Moq;
using Xunit;

namespace DDDSample1.Tests.Unit.Domain
{
    public class UserServiceTests
    {
        private readonly Mock<UserManager<ApplicationUser>> _mockUserManager;
        private readonly Mock<RoleManager<IdentityRole>> _mockRoleManager;
        private readonly Mock<IEmailService> _mockEmailService;
        private readonly UserService _userService;

        public UserServiceTests()
        {
            _mockUserManager = new Mock<UserManager<ApplicationUser>>(
                Mock.Of<IUserStore<ApplicationUser>>(), null, null, null, null, null, null, null, null
            );

            _mockRoleManager = new Mock<RoleManager<IdentityRole>>(
                Mock.Of<IRoleStore<IdentityRole>>(), null, null, null, null
            );

            _mockEmailService = new Mock<IEmailService>();

            _userService = new UserService(
                _mockUserManager.Object,
                _mockRoleManager.Object,
                _mockEmailService.Object
            );
        }

        [Fact]
        public async Task RegisterUser_ValidData_ReturnsSuccess()
        {
            var createUserDto = new CreateUserDto
            {
                UserName = "john.doe",
                Email = "john.doe@example.com",
                Password = "Password123!",
                Role = "Doctor"
            };

            _mockRoleManager.Setup(rm => rm.RoleExistsAsync(It.IsAny<string>())).ReturnsAsync(true);
            _mockUserManager.Setup(um => um.FindByEmailAsync(It.IsAny<string>())).ReturnsAsync((ApplicationUser)null);
            _mockUserManager.Setup(um => um.CreateAsync(It.IsAny<ApplicationUser>(), It.IsAny<string>())).ReturnsAsync(IdentityResult.Success);

            var result = await _userService.RegisterUserAsync(createUserDto);

            Assert.NotNull(result);
            Assert.Equal("john.doe@example.com", result.Email);
            Assert.Equal("Doctor", result.Role);
            _mockUserManager.Verify(um => um.CreateAsync(It.IsAny<ApplicationUser>(), It.IsAny<string>()), Times.Once);
        }

        [Fact]
        public async Task RegisterUser_RoleDoesNotExist_ThrowsException()
        {
            var createUserDto = new CreateUserDto
            {
                UserName = "john.doe",
                Email = "john.doe@example.com",
                Password = "Password123!",
                Role = "InvalidRole"
            };

            _mockRoleManager.Setup(rm => rm.RoleExistsAsync(It.IsAny<string>())).ReturnsAsync(false);

            await Assert.ThrowsAsync<InvalidOperationException>(() => _userService.RegisterUserAsync(createUserDto));
        }

        [Fact]
        public async Task RegisterUser_UserAlreadyExists_ThrowsException()
        {
            var createUserDto = new CreateUserDto
            {
                UserName = "john.doe",
                Email = "john.doe@example.com",
                Password = "Password123!",
                Role = "Doctor"
            };

            _mockRoleManager.Setup(rm => rm.RoleExistsAsync(It.IsAny<string>())).ReturnsAsync(true);
            _mockUserManager.Setup(um => um.FindByEmailAsync(It.IsAny<string>())).ReturnsAsync(new ApplicationUser());

            await Assert.ThrowsAsync<InvalidOperationException>(() => _userService.RegisterUserAsync(createUserDto));
        }

        [Fact]
        public async Task GetUserById_ValidId_ReturnsUser()
        {
            var userId = "user-123";
            var user = new ApplicationUser { Id = userId, UserName = "john.doe", Email = "john.doe@example.com" };
            _mockUserManager.Setup(um => um.FindByIdAsync(It.IsAny<string>())).ReturnsAsync(user);
            _mockUserManager.Setup(um => um.GetRolesAsync(It.IsAny<ApplicationUser>())).ReturnsAsync(new List<string> { "Doctor" });

            var result = await _userService.GetUserByIdAsync(userId);

            Assert.NotNull(result);
            Assert.Equal("john.doe@example.com", result.Email);
            Assert.Equal("Doctor", result.Role);
        }

        [Fact]
        public async Task GetUserById_UserNotFound_ReturnsNull()
        {
            var userId = "nonexistent-id";

            _mockUserManager.Setup(um => um.FindByIdAsync(It.IsAny<string>())).ReturnsAsync((ApplicationUser)null);  // Mock user not found

            var result = await _userService.GetUserByIdAsync(userId);

            Assert.Null(result);  // Expect the result to be null
        }


        [Fact]
        public async Task UpdateUser_ValidData_UpdatesSuccessfully()
        {
            var userId = "user-123";
            var user = new ApplicationUser { Id = userId, UserName = "john.doe", Email = "john.doe@example.com" };

            var updateUserDto = new UpdateUserDto
            {
                UserName = "newusername",
                Email = "newemail@example.com",
                NewRole = "Nurse"
            };

            _mockUserManager.Setup(um => um.FindByIdAsync(It.IsAny<string>())).ReturnsAsync(user);
            _mockUserManager.Setup(um => um.UpdateAsync(It.IsAny<ApplicationUser>())).ReturnsAsync(IdentityResult.Success);

            var result = await _userService.UpdateUserAsync(userId, updateUserDto);

            Assert.NotNull(result);
            Assert.Equal("newemail@example.com", result.Email);
            Assert.Equal("Nurse", result.Role);
            _mockUserManager.Verify(um => um.UpdateAsync(It.IsAny<ApplicationUser>()), Times.Once);
        }

        [Fact]
        public async Task UpdateUser_UserNotFound_ThrowsException()
        {
            var userId = "nonexistent-id";
            var updateUserDto = new UpdateUserDto
            {
                UserName = "newusername",
                Email = "newemail@example.com"
            };

            _mockUserManager.Setup(um => um.FindByIdAsync(It.IsAny<string>())).ReturnsAsync((ApplicationUser)null);

            await Assert.ThrowsAsync<InvalidOperationException>(() => _userService.UpdateUserAsync(userId, updateUserDto));
        }

        [Fact]
        public async Task DeleteUser_ValidUser_DeletesSuccessfully()
        {
            var userId = "user-123";
            var user = new ApplicationUser { Id = userId, UserName = "john.doe", Email = "john.doe@example.com" };
            _mockUserManager.Setup(um => um.FindByIdAsync(It.IsAny<string>())).ReturnsAsync(user);
            _mockUserManager.Setup(um => um.DeleteAsync(It.IsAny<ApplicationUser>())).ReturnsAsync(IdentityResult.Success);

            var result = await _userService.DeleteUserAsync(userId);

            Assert.True(result);
            _mockUserManager.Verify(um => um.DeleteAsync(It.IsAny<ApplicationUser>()), Times.Once);
        }

        [Fact]
        public async Task DeleteUser_UserNotFound_ThrowsException()
        {
            var userId = "nonexistent-id";
            _mockUserManager.Setup(um => um.FindByIdAsync(It.IsAny<string>())).ReturnsAsync((ApplicationUser)null);

            await Assert.ThrowsAsync<InvalidOperationException>(() => _userService.DeleteUserAsync(userId));
        }
    }
}
