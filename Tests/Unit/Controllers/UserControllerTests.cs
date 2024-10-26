using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.User;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace DDDSample1.Tests.Unit.Controllers
{
    public class UserControllerTests
    {
        private readonly Mock<IUserService> _mockUserService;
        private readonly Mock<ILogger<UserController>> _mockLogger;
        private readonly UserController _controller;

        public UserControllerTests()
        {
            _mockUserService = new Mock<IUserService>();
            _mockLogger = new Mock<ILogger<UserController>>();
            _controller = new UserController(_mockUserService.Object, _mockLogger.Object);
        }

        [Fact]
        public async Task Register_ValidData_ReturnsCreatedAtAction()
        {
            var createUserDto = new CreateUserDto
            {
                UserName = "john.doe",
                Email = "john.doe@example.com",
                Password = "Password123!",
                Role = "Doctor"
            };

            var userDto = new UserDto
            {
                Id = "user-123",
                UserName = "john.doe",
                Email = "john.doe@example.com",
                Role = "Doctor"
            };

            _mockUserService.Setup(s => s.RegisterUserAsync(It.IsAny<CreateUserDto>())).ReturnsAsync(userDto);

            var result = await _controller.Register(createUserDto);

            var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(result);
            Assert.Equal("GetUser", createdAtActionResult.ActionName);
            var returnValue = Assert.IsType<UserDto>(createdAtActionResult.Value);
            Assert.Equal("john.doe@example.com", returnValue.Email);
        }

        [Fact]
        public async Task Register_InvalidModelState_ReturnsBadRequest()
        {
            _controller.ModelState.AddModelError("Email", "Email is required");

            var createUserDto = new CreateUserDto
            {
                UserName = "john.doe",
                Password = "Password123!",
                Role = "Doctor"
            };

            var result = await _controller.Register(createUserDto);

            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task Register_UserAlreadyExists_ReturnsBadRequest()
        {
            var createUserDto = new CreateUserDto
            {
                UserName = "john.doe",
                Email = "john.doe@example.com",
                Password = "Password123!",
                Role = "Doctor"
            };

            _mockUserService.Setup(s => s.RegisterUserAsync(It.IsAny<CreateUserDto>()))
                .ThrowsAsync(new InvalidOperationException("User already exists."));

            var result = await _controller.Register(createUserDto);

            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.Equal("User already exists.", badRequestResult.Value);
        }

        [Fact]
        public async Task GetUser_ValidId_ReturnsOkResult()
        {
            var userId = "user-123";
            var userDto = new UserDto
            {
                Id = userId,
                UserName = "john.doe",
                Email = "john.doe@example.com",
                Role = "Doctor"
            };

            _mockUserService.Setup(s => s.GetUserByIdAsync(It.IsAny<string>())).ReturnsAsync(userDto);

            var result = await _controller.GetUser(userId);

            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<UserDto>(okResult.Value);
            Assert.Equal("john.doe@example.com", returnValue.Email);
        }

        [Fact]
        public async Task GetUser_UserNotFound_ReturnsNotFound()
        {
            var userId = "nonexistent-id";

            _mockUserService.Setup(s => s.GetUserByIdAsync(It.IsAny<string>())).ReturnsAsync((UserDto)null);

            var result = await _controller.GetUser(userId);

            Assert.IsType<NotFoundResult>(result);
        }


        [Fact]
        public async Task UpdateUser_ValidData_ReturnsOkResult()
        {
            var userId = "user-123";
            var updateUserDto = new UpdateUserDto
            {
                UserName = "john.doe",
                Email = "john.doe@example.com",
                NewRole = "Nurse"
            };

            var updatedUser = new UserDto
            {
                Id = userId,
                UserName = "john.doe",
                Email = "john.doe@example.com",
                Role = "Nurse"
            };

            _mockUserService.Setup(s => s.UpdateUserAsync(It.IsAny<string>(), It.IsAny<UpdateUserDto>())).ReturnsAsync(updatedUser);

            var result = await _controller.UpdateUser(userId, updateUserDto);

            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<UserDto>(okResult.Value);
            Assert.Equal("john.doe@example.com", returnValue.Email);
            Assert.Equal("Nurse", returnValue.Role);
        }

        [Fact]
        public async Task UpdateUser_UserNotFound_ReturnsBadRequest()
        {
            var userId = "nonexistent-id";
            var updateUserDto = new UpdateUserDto
            {
                UserName = "john.doe",
                Email = "john.doe@example.com"
            };

            _mockUserService.Setup(s => s.UpdateUserAsync(It.IsAny<string>(), It.IsAny<UpdateUserDto>()))
                .ThrowsAsync(new InvalidOperationException("User not found."));

            var result = await _controller.UpdateUser(userId, updateUserDto);

            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.Equal("User not found.", badRequestResult.Value);
        }

        [Fact]
        public async Task DeleteUser_ValidUser_NotFoundObjectResult()
        {
            var userId = "user-123";

            _mockUserService.Setup(s => s.DeleteUserAsync(It.IsAny<string>())).ReturnsAsync(true);

            var result = await _controller.DeleteUser(userId);

            Assert.IsType<NotFoundObjectResult>(result);
        }

        [Fact]
        public async Task DeleteUser_UserNotFound_NotFoundObjectResult()
        {
            var userId = "nonexistent-id";

            _mockUserService.Setup(s => s.DeleteUserAsync(It.IsAny<string>())).ReturnsAsync(false);

            var result = await _controller.DeleteUser(userId);

            var badRequestResult = Assert.IsType<NotFoundObjectResult>(result);
            Assert.Equal("User not found.", badRequestResult.Value);
        }
    }
}
