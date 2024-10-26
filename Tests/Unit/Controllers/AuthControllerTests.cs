using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;
using Microsoft.AspNetCore.Identity;
using DDDSample1.Domain.Auth;
using DDDSample1.Controllers;
using DDDSample1.Domain.UserData;
using System.Threading.Tasks;
using System;

public class AuthControllerTests
{
    private readonly Mock<IAuthService> _mockAuthService;
    private readonly Mock<ILogger<AuthController>> _mockLogger;
    private readonly Mock<UserManager<ApplicationUser>> _mockUserManager;
    private readonly AuthController _controller;

    public AuthControllerTests()
    {
        _mockAuthService = new Mock<IAuthService>();
        _mockLogger = new Mock<ILogger<AuthController>>();
        _mockUserManager = MockUserManager();
        _controller = new AuthController(_mockAuthService.Object, _mockLogger.Object, _mockUserManager.Object);
    }

    private Mock<UserManager<ApplicationUser>> MockUserManager()
    {
        var store = new Mock<IUserStore<ApplicationUser>>();
        return new Mock<UserManager<ApplicationUser>>(store.Object, null, null, null, null, null, null, null, null);
    }

    [Fact]
    public async Task Login_ValidCredentials_ReturnsOkResultWithToken()
    {
        // Arrange
        var loginDto = new LoginDto { Email = "test@example.com", Password = "Password123!" };
        var token = "valid-token";
        _mockAuthService.Setup(service => service.LoginAsync(loginDto)).ReturnsAsync(token);

        // Act
        var result = await _controller.Login(loginDto);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        Assert.Equal(token, ((dynamic)okResult.Value).Token);
    }

    [Fact]
    public async Task Login_InvalidCredentials_ReturnsUnauthorized()
    {
        // Arrange
        var loginDto = new LoginDto { Email = "test@example.com", Password = "WrongPassword" };
        _mockAuthService.Setup(service => service.LoginAsync(loginDto)).ThrowsAsync(new InvalidOperationException("Invalid credentials"));

        // Act
        var result = await _controller.Login(loginDto);

        // Assert
        var unauthorizedResult = Assert.IsType<UnauthorizedObjectResult>(result);
        Assert.Equal("Invalid credentials", unauthorizedResult.Value);
    }

    [Fact]
    public async Task Login_ExceptionThrown_ReturnsInternalServerError()
    {
        // Arrange
        var loginDto = new LoginDto { Email = "test@example.com", Password = "Password123!" };
        _mockAuthService.Setup(service => service.LoginAsync(loginDto)).ThrowsAsync(new Exception("Unexpected error"));

        // Act
        var result = await _controller.Login(loginDto);

        // Assert
        var statusCodeResult = Assert.IsType<ObjectResult>(result);
        Assert.Equal(500, statusCodeResult.StatusCode);
        Assert.Equal("An error occurred while processing your request.", statusCodeResult.Value);
    }

    [Fact]
    public async Task ResetPassword_ValidEmail_ReturnsOkResult()
    {
        // Arrange
        var model = new ResetPasswordDto { Email = "test@example.com" };
        _mockAuthService.Setup(x => x.ResetPasswordAsync(model.Email)).ReturnsAsync(true);

        // Act
        var result = await _controller.ResetPassword(model);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        Assert.Equal("Password reset email sent.", okResult.Value);
    }

    [Fact]
    public async Task ResetPassword_InvalidEmail_ReturnsBadRequest()
    {
        // Arrange
        var model = new ResetPasswordDto { Email = "invalid-email" };

        // Act
        var result = await _controller.ResetPassword(model);

        // Assert
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task ResetPassword_ServiceThrowsException_ReturnsBadRequest()
    {
        // Arrange
        var model = new ResetPasswordDto { Email = "test@example.com" };
        _mockAuthService.Setup(x => x.ResetPasswordAsync(model.Email))
            .ThrowsAsync(new InvalidOperationException("User not found"));

        // Act
        var result = await _controller.ResetPassword(model);

        // Assert
        var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal("User not found", badRequestResult.Value);
    }

    [Fact]
    public async Task ConfirmResetPassword_ValidData_ReturnsOkResult()
    {
        // Arrange
        var model = new ConfirmResetPasswordDto 
        { 
            Email = "test@example.com",
            Token = "valid-token",
            NewPassword = "NewPassword123!"
        };

        _mockUserManager.Setup(x => x.FindByEmailAsync(model.Email))
            .ReturnsAsync(new ApplicationUser());
        _mockAuthService.Setup(x => x.ConfirmResetPasswordAsync(model.Email, model.Token, model.NewPassword))
            .ReturnsAsync(true);

        // Act
        var result = await _controller.ConfirmResetPassword(model);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        Assert.Equal("Password has been reset successfully.", okResult.Value);
    }

    [Fact]
    public async Task ConfirmResetPassword_UserNotFound_ReturnsBadRequest()
    {
        // Arrange
        var model = new ConfirmResetPasswordDto
        {
            Email = "nonexistent@example.com",
            Token = "valid-token",
            NewPassword = "NewPassword123!"
        };

        _mockUserManager.Setup(x => x.FindByEmailAsync(model.Email))
            .ReturnsAsync((ApplicationUser)null);

        // Act
        var result = await _controller.ConfirmResetPassword(model);

        // Assert
        var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal("User with provided email not found.", badRequestResult.Value);
    }

    [Fact]
    public async Task ConfirmResetPassword_InvalidToken_ReturnsBadRequest()
    {
        // Arrange
        var model = new ConfirmResetPasswordDto
        {
            Email = "test@example.com",
            Token = "invalid-token",
            NewPassword = "NewPassword123!"
        };

        _mockUserManager.Setup(x => x.FindByEmailAsync(model.Email))
            .ReturnsAsync(new ApplicationUser());
        _mockAuthService.Setup(x => x.ConfirmResetPasswordAsync(model.Email, model.Token, model.NewPassword))
            .ReturnsAsync(false);

        // Act
        var result = await _controller.ConfirmResetPassword(model);

        // Assert
        var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal("Failed to reset password. Invalid token or token expired.", badRequestResult.Value);
    }
    [Fact]
    public async Task ConfirmResetPassword_SameAsOldPassword_ReturnsBadRequest()
    {
        // Arrange
        var model = new ConfirmResetPasswordDto
        {
            Email = "test@example.com",
            Token = "valid-token",
            NewPassword = "OldPassword123!" // Same as current password
        };

        _mockUserManager.Setup(x => x.FindByEmailAsync(model.Email))
            .ReturnsAsync(new ApplicationUser());
        _mockAuthService.Setup(x => x.ConfirmResetPasswordAsync(model.Email, model.Token, model.NewPassword))
            .ThrowsAsync(new InvalidOperationException("New password cannot be the same as your old password."));

        // Act
        var result = await _controller.ConfirmResetPassword(model);

        // Assert
        var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal("New password cannot be the same as your old password.", badRequestResult.Value);
    }

    [Fact]
    public async Task ConfirmResetPassword_InvalidPasswordComplexity_ReturnsBadRequest()
    {
        // Arrange
        var model = new ConfirmResetPasswordDto
        {
            Email = "test@example.com",
            Token = "valid-token",
            NewPassword = "weak" // Password that doesn't meet complexity requirements
        };

        _mockUserManager.Setup(x => x.FindByEmailAsync(model.Email))
            .ReturnsAsync(new ApplicationUser());
        _mockAuthService.Setup(x => x.ConfirmResetPasswordAsync(model.Email, model.Token, model.NewPassword))
            .ThrowsAsync(new InvalidOperationException("Password must be at least 8 characters long and contain at least one uppercase letter, one lowercase letter, one number, and one special character."));

        // Act
        var result = await _controller.ConfirmResetPassword(model);

        // Assert
        var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal("Password must be at least 8 characters long and contain at least one uppercase letter, one lowercase letter, one number, and one special character.", badRequestResult.Value);
    }

    [Fact]
    public async Task ConfirmResetPassword_NullOrEmptyPassword_ReturnsBadRequest()
    {
        // Arrange
        var model = new ConfirmResetPasswordDto
        {
            Email = "test@example.com",
            Token = "valid-token",
            NewPassword = "" // Empty password
        };

        _mockUserManager.Setup(x => x.FindByEmailAsync(model.Email))
            .ReturnsAsync(new ApplicationUser());

        // Act
        var result = await _controller.ConfirmResetPassword(model);

        // Assert
        var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal("New password is required.", badRequestResult.Value);
    }
}
