using Microsoft.AspNetCore.Identity;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;
using DDDSample1.Domain.UserData;
using DDDSample1.Domain.Auth;
using System.Threading.Tasks;
using System;
using System.Collections.Generic;

public class AuthServiceTests
{
    private readonly Mock<UserManager<ApplicationUser>> _mockUserManager;
    private readonly Mock<SignInManager<ApplicationUser>> _mockSignInManager;
    private readonly Mock<IConfiguration> _mockConfiguration;
    private readonly Mock<IEmailService> _mockEmailService;
    private readonly Mock<ILogger<AuthService>> _mockLogger;
    private readonly AuthService _service;

    public AuthServiceTests()
    {
        var store = new Mock<IUserStore<ApplicationUser>>();
        _mockUserManager = new Mock<UserManager<ApplicationUser>>(store.Object, null, null, null, null, null, null, null, null);
        _mockSignInManager = new Mock<SignInManager<ApplicationUser>>(_mockUserManager.Object, 
            new Mock<Microsoft.AspNetCore.Http.IHttpContextAccessor>().Object,
            new Mock<IUserClaimsPrincipalFactory<ApplicationUser>>().Object,
            null, null, null, null);
        _mockConfiguration = new Mock<IConfiguration>();
        _mockEmailService = new Mock<IEmailService>();
        _mockLogger = new Mock<ILogger<AuthService>>();

        _service = new AuthService(
            _mockUserManager.Object,
            _mockSignInManager.Object,
            _mockConfiguration.Object,
            _mockEmailService.Object,
            _mockLogger.Object
        );
    }

    [Fact]
    public async Task LoginAsync_UserNotFound_ThrowsException()
    {
        // Arrange
        var loginDto = new LoginDto { Email = "nonexistent@example.com", Password = "Password123!" };
        _mockUserManager.Setup(x => x.FindByEmailAsync(loginDto.Email)).ReturnsAsync((ApplicationUser)null);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.LoginAsync(loginDto));
    }

    [Fact]
    public async Task LoginAsync_ValidCredentials_ReturnsToken()
    {
        // Arrange
        var loginDto = new LoginDto { Email = "test@example.com", Password = "Password123!" };
        var user = new ApplicationUser { Email = loginDto.Email, UserName = "testuser" };

        _mockUserManager.Setup(x => x.FindByEmailAsync(loginDto.Email)).ReturnsAsync(user);
        _mockSignInManager.Setup(x => x.PasswordSignInAsync(user.UserName, loginDto.Password, false, true))
            .ReturnsAsync(SignInResult.Success);
        _mockUserManager.Setup(x => x.GetRolesAsync(user)).ReturnsAsync(new List<string>());
        _mockConfiguration.Setup(x => x["Jwt:Key"]).Returns("MySuperSecretKeyThatIsLongEnough32Chrs!");
        _mockConfiguration.Setup(x => x["Jwt:Issuer"]).Returns("MyUniversityProjectAPI");
        _mockConfiguration.Setup(x => x["Jwt:Audience"]).Returns("MyUniversityProjectClient");

        // Act
        var result = await _service.LoginAsync(loginDto);

        // Assert
        Assert.NotNull(result);
    }
    

    [Fact]
    public async Task LoginAsync_AccountLockedOut_ThrowsException()
    {
        // Arrange
        var loginDto = new LoginDto { Email = "lockedout@example.com", Password = "Password123!" };
        var user = new ApplicationUser { Email = loginDto.Email, UserName = "lockeduser" };

        _mockUserManager.Setup(x => x.FindByEmailAsync(loginDto.Email)).ReturnsAsync(user);
        _mockSignInManager.Setup(x => x.PasswordSignInAsync(user.UserName, loginDto.Password, false, true))
            .ReturnsAsync(SignInResult.LockedOut);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.LoginAsync(loginDto));
        _mockEmailService.Verify(x => x.SendAdminLockoutNotification(user.Email), Times.Once);
    }

    [Fact]
    public async Task LoginAsync_InvalidCredentials_ThrowsException()
    {
        // Arrange
        var loginDto = new LoginDto { Email = "test@example.com", Password = "WrongPassword" };
        var user = new ApplicationUser { Email = loginDto.Email, UserName = "testuser" };

        _mockUserManager.Setup(x => x.FindByEmailAsync(loginDto.Email)).ReturnsAsync(user);
        _mockSignInManager.Setup(x => x.PasswordSignInAsync(user.UserName, loginDto.Password, false, true))
            .ReturnsAsync(SignInResult.Failed);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.LoginAsync(loginDto));
    }

    [Fact]
    public async Task ResetPasswordAsync_ValidBackofficeUser_ReturnsTrue()
    {
        // Arrange
        var email = "test@example.com";
        var user = new ApplicationUser { Email = email };
        var resetToken = "reset-token";

        _mockUserManager.Setup(x => x.FindByEmailAsync(email)).ReturnsAsync(user);
        _mockUserManager.Setup(x => x.IsInRoleAsync(user, It.IsAny<string>())).ReturnsAsync(true);
        _mockUserManager.Setup(x => x.GeneratePasswordResetTokenAsync(user)).ReturnsAsync(resetToken);
        _mockEmailService.Setup(x => x.SendEmailAsync(email, It.IsAny<string>(), It.IsAny<string>()))
            .Returns(Task.CompletedTask);

        // Act
        var result = await _service.ResetPasswordAsync(email);

        // Assert
        Assert.True(result);
        _mockEmailService.Verify(x => x.SendEmailAsync(email, It.IsAny<string>(), It.IsAny<string>()), Times.Once);
    }

    [Fact]
    public async Task ResetPasswordAsync_UserNotFound_ThrowsException()
    {
        // Arrange
        var email = "nonexistent@example.com";
        _mockUserManager.Setup(x => x.FindByEmailAsync(email)).ReturnsAsync((ApplicationUser)null);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.ResetPasswordAsync(email));
    }

    [Fact]
    public async Task ResetPasswordAsync_NotBackofficeUser_ThrowsException()
    {
        // Arrange
        var email = "test@example.com";
        var user = new ApplicationUser { Email = email };
        _mockUserManager.Setup(x => x.FindByEmailAsync(email)).ReturnsAsync(user);
        _mockUserManager.Setup(x => x.IsInRoleAsync(user, It.IsAny<string>())).ReturnsAsync(false);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.ResetPasswordAsync(email));
    }

    [Fact]
    public async Task ConfirmResetPasswordAsync_ValidToken_ReturnsTrue()
    {
        // Arrange
        var email = "test@example.com";
        var token = "valid-token";
        var newPassword = "NewPassword123!";
        var user = new ApplicationUser { Email = email };

        _mockUserManager.Setup(x => x.FindByEmailAsync(email)).ReturnsAsync(user);
        _mockUserManager.Setup(x => x.ResetPasswordAsync(user, token, newPassword))
            .ReturnsAsync(IdentityResult.Success);

        // Act
        var result = await _service.ConfirmResetPasswordAsync(email, token, newPassword);

        // Assert
        Assert.True(result);
    }

    [Fact]
    public async Task ConfirmResetPasswordAsync_UserNotFound_ThrowsException()
    {
        // Arrange
        var email = "nonexistent@example.com";
        _mockUserManager.Setup(x => x.FindByEmailAsync(email)).ReturnsAsync((ApplicationUser)null);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => 
            _service.ConfirmResetPasswordAsync(email, "token", "newPassword"));
    }

    [Fact]
    public async Task ConfirmResetPasswordAsync_InvalidToken_ThrowsException()
    {
        // Arrange
        var email = "test@example.com";
        var token = "invalid-token";
        var newPassword = "NewPassword123!";
        var user = new ApplicationUser { Email = email };

        _mockUserManager.Setup(x => x.FindByEmailAsync(email)).ReturnsAsync(user);
        _mockUserManager.Setup(x => x.ResetPasswordAsync(user, token, newPassword))
            .ReturnsAsync(IdentityResult.Failed(new IdentityError { Description = "Invalid token" }));

        // Act & Assert
        var exception = await Assert.ThrowsAsync<Exception>(() => 
            _service.ConfirmResetPasswordAsync(email, token, newPassword));
        
        Assert.Equal("Password reset failed. Errors: Invalid token", exception.Message);
    }

}
