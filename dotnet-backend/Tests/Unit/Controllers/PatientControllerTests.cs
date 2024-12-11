/*using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.JsonPatch;
using System.Security.Claims;
using Xunit;
using Moq;
using DDDSample1.Controllers;
using DDDSample1.Domain.PatientData;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Identity;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.UserData;
using System.Threading.Tasks;
using System.Collections.Generic;
using System;
using System.Linq;
using Microsoft.IdentityModel.Tokens;

namespace DDDSample1.Tests.Unit.Controllers
{
    public class PatientControllerTests
    {
        private readonly PatientController _controller;
        private readonly Mock<IPatientRepository> _mockRepository;
        private readonly Mock<UserManager<ApplicationUser>> _mockUserManager;
        private readonly Mock<IUnitOfWork> _mockUnitOfWork;
        private readonly Mock<IEmailService> _mockEmailService;
        private readonly Mock<ILoggingService> _mockLoggingService;

        private readonly Mock<PatientService> _mockService;


        public PatientControllerTests()
        {
            _mockRepository = new Mock<IPatientRepository>();
            _mockUserManager = MockUserManager<ApplicationUser>();
            _mockUnitOfWork = new Mock<IUnitOfWork>();
            _mockEmailService = new Mock<IEmailService>();
            _mockLoggingService = new Mock<ILoggingService>();
            _mockService = new Mock<PatientService>();
            

            var service = new PatientService(
                _mockRepository.Object,
                _mockUserManager.Object,
                _mockUnitOfWork.Object,
                _mockEmailService.Object,
                _mockLoggingService.Object
            );

            _controller = new PatientController(service);
        }

        private Mock<UserManager<TUser>> MockUserManager<TUser>() where TUser : class
        {
            var store = new Mock<IUserStore<TUser>>();
            return new Mock<UserManager<TUser>>(store.Object, null, null, null, null, null, null, null, null);
        }
        private void SetupUserContext(ControllerBase controller, string email)
        {
            var user = new ClaimsPrincipal(new ClaimsIdentity(new Claim[]
            {
                new Claim(ClaimTypes.Email, email),
                new Claim(ClaimTypes.Role, "Patient")  
            }));

            controller.ControllerContext = new ControllerContext()
            {
                HttpContext = new DefaultHttpContext { User = user }
            };
        }
         private void SetupAdminUserContext(ControllerBase controller)
        {
            var user = new ClaimsPrincipal(new ClaimsIdentity(new Claim[]
            {
                new Claim(ClaimTypes.Role, "Admin")
            }));

            controller.ControllerContext = new ControllerContext()
            {
                HttpContext = new DefaultHttpContext { User = user }
            };
        }

        private void SetupNonAdminUserContext(ControllerBase controller)
        {
            var user = new ClaimsPrincipal(new ClaimsIdentity(new Claim[]
            {
                new Claim(ClaimTypes.Role, "User")
            }));

            controller.ControllerContext = new ControllerContext()
            {
                HttpContext = new DefaultHttpContext { User = user }
            };
        }


        [Fact]
        public async Task UpdatePatientProfile_UnauthorizedUser_ReturnsUnauthorized()
        {
            // Arrange
            var userEmail = "test@example.com";
            var patchDoc = new JsonPatchDocument<UpdatePatientDto>();

            // Setup unauthorized user context (not in "Patient" role)
            var claims = new List<Claim>
            {
                new Claim(ClaimTypes.Email, userEmail),
                new Claim(ClaimTypes.Role, "SomeOtherRole")
            };
            var identity = new ClaimsIdentity(claims, "TestAuthType");
            var user = new ClaimsPrincipal(identity);

            _controller.ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext { User = user }
            };

            // Act
            var result = await _controller.UpdatePatientProfile(userEmail, patchDoc);

            // Assert
            var unauthorizedResult = Assert.IsType<UnauthorizedObjectResult>(result);
            Assert.Equal("User is not authorized to perform this action.", unauthorizedResult.Value);
        }

        // In PatientControllerTests.cs

        [Fact]
        public async Task UpdatePatientProfile_NullPatchDocument_ReturnsBadRequest()
        {
            // Arrange
            var userEmail = "test@example.com";
            JsonPatchDocument<UpdatePatientDto> patchDoc = null;

            SetupUserContext(_controller, userEmail);

            // Act
            var result = await _controller.UpdatePatientProfile(userEmail, patchDoc);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.Equal("Patch document is null", badRequestResult.Value);
        }
        
        [Fact]
        public async Task DeletePatient_UserIsNotAdmin_ReturnsUnauthorized()
        {
            // Arrange
            var patientId = "valid-patient-id";
            var deleteDto = new DeletePatientDto { ConfirmDeletion = true };
            SetupNonAdminUserContext(_controller);

            // Act
            var result = await _controller.DeletePatient(patientId, deleteDto);

            // Assert
            var unauthorizedResult = Assert.IsType<UnauthorizedResult>(result);
        }

        [Fact]
        public async Task DeletePatient_DeletionNotConfirmed_ReturnsBadRequest()
        {
            // Arrange
            var patientId = "valid-patient-id";
            var deleteDto = new DeletePatientDto { ConfirmDeletion = false }; // Deletion not confirmed
            SetupAdminUserContext(_controller);

            // Act
            var result = await _controller.DeletePatient(patientId, deleteDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.Equal("Deletion not confirmed.", badRequestResult.Value);
        }
        [Fact]
        public async Task RequestAccountDeletion_ValidRequest_ReturnsOk()
        {
            // Arrange
            var userEmail = "test@example.com";
            SetupUserContext(_controller, userEmail);

            _mockEmailService
                .Setup(e => e.SendEmailAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
                .Returns(Task.CompletedTask);

            _mockRepository
                .Setup(r => r.GetByEmailAsync(userEmail))
                .ReturnsAsync(new Patient(
                    "MED123", "userId", "John", "Doe", userEmail,
                    "1990-01-01", "Male", "Address", "Emergency Contact",
                    "123456789", "", ""));

            // Act
            var result = await _controller.RequestAccountDeletion();

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            Assert.Equal("Confirmation email sent. Please check your email to confirm account deletion.", okResult.Value);
        }


        [Fact]
        public async Task RequestAccountDeletion_NoUserEmail_ReturnsUnauthorized()
        {
            // Arrange
            _controller.ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext { User = new ClaimsPrincipal() }
            };

            // Act
            var result = await _controller.RequestAccountDeletion();

            // Assert
            var unauthorizedResult = Assert.IsType<UnauthorizedObjectResult>(result);
            Assert.Equal("User email not found.", unauthorizedResult.Value);
        }

        [Fact]
        public async Task RequestAccountDeletion_ServiceThrowsException_ReturnsBadRequest()
        {
            // Arrange
            var userEmail = "test@example.com";
            SetupUserContext(_controller, userEmail);
            var errorMessage = "Patient not found."; // Match the actual error message

            _mockRepository
                .Setup(r => r.GetByEmailAsync(userEmail))
                .ReturnsAsync((Patient)null); // This will cause the service to throw the exception

            // Act
            var result = await _controller.RequestAccountDeletion();

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.Equal(errorMessage, badRequestResult.Value);
        }

        [Fact]
        public async Task ConfirmAccountDeletion_EmptyToken_ReturnsBadRequest()
        {
            // Arrange
            var userEmail = "test@example.com";
            var dto = new DeleteConfirmationDto { Token = "" };
            SetupUserContext(_controller, userEmail);

            // Act
            var result = await _controller.ConfirmAccountDeletion(dto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.Equal("Token is required.", badRequestResult.Value);
        }
        [Fact]
        public async Task ConfirmAccountDeletion_InvalidToken_ReturnsBadRequest()
        {
            // Arrange
            var userEmail = "test@example.com";
            var dto = new DeleteConfirmationDto { Token = "invalid-token" };
            SetupUserContext(_controller, userEmail);

            _mockEmailService
            .Setup(s => s.SendEmailAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
            .ThrowsAsync(new Exception("Email service error"));


            // Act
            var result = await _controller.ConfirmAccountDeletion(dto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.Contains("Invalid token", badRequestResult.Value.ToString());
        }


        [Fact]
        public async Task GetPatients_ReturnsOkResult_WithListOfPatientsAsync()
        {
            // Arrange
            var filter = new PatientFilterDTO{
                Email = "john.doe@email.com"
            };
            var patients = new List<Patient>
            {
                new Patient { FirstName= "John",
                    LastName = "Doe",
                    Email= "john.doe@email.com",
                    PhoneNumber = "+1-555-1234",
                    DateofBirth = "1985-04-05",
                    Gender = "Male",
                    ContactInfo = "123 Elm Street, Springfield, IL",
                    EmergencyContact = "Jane Doe (+1-555-9876)",
                    AppointmentHistory = "Routine checkup in 2024",
                    MedicalHistory =  "Hypertension, Diabetes" }
            };

            _mockRepository.Setup(r => r.GetFilteredPatientAsync(filter, 1, 5))
                .ReturnsAsync((patients, patients.Count));

            // Act
            var result = await _controller.GetPatients(filter, 1, 5);

            // Assert
            var actionResult = Assert.IsType<ActionResult<PagedResult<PatientDto>>>(result);
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var returnValue = Assert.IsType<PagedResult<PatientDto>>(okResult.Value);
            Assert.Single(returnValue.Items);
            Assert.Equal("John", returnValue.Items.First().FirstName);
            Assert.Equal(1, returnValue.TotalCount);
            Assert.Equal(1, returnValue.TotalPages);

            _mockRepository.Verify(r => r.GetFilteredPatientAsync(filter, 1, 5), Times.Once);
        }
        
        

        [Fact]
        public async Task GetPatients_ReturnsNotFound_WhenNoPatients()
        {
            // Arrange
            var filter = new PatientFilterDTO();
            var emptyPatientList = new List<Patient>();

            _mockRepository.Setup(r => r.GetFilteredPatientAsync(filter, 1, 5))
                .ReturnsAsync((emptyPatientList, 0));

            // Act
            var result = await _controller.GetPatients(filter, 1, 5);

            // Assert
            var actionResult = Assert.IsType<ActionResult<PagedResult<PatientDto>>>(result);
            Assert.IsType<NotFoundObjectResult>(result.Result);
            _mockRepository.Verify(r => r.GetFilteredPatientAsync(filter, 1, 5), Times.Once);
        }
        
        
        [Fact]
        public async Task AdminUpdatePatientProfile_ValidUpdate_ReturnsOkResult()
        {
            // Arrange
            var patientId = "user-123";
            var patchDoc = new JsonPatchDocument<UpdatePatientDto>();
            patchDoc.Replace(p => p.FirstName, "Jane");
            patchDoc.Replace(p => p.LastName, "Doe");

            SetupAdminUserContext(_controller);

            var existingPatient = new Patient("med-123", patientId, "John", "Doe", "patient@example.com", "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);
            _mockRepository.Setup(r => r.GetByIdAsync(patientId)).ReturnsAsync(existingPatient);
            _mockRepository.Setup(r => r.UpdateAsync(It.IsAny<Patient>())).ReturnsAsync(existingPatient);

            // Act
            var result = await _controller.AdminUpdatePatientProfile(patientId, patchDoc);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var updatedPatient = Assert.IsType<PatientDto>(okResult.Value);
            Assert.Equal("Jane", updatedPatient.FirstName);
            Assert.Equal("Doe", updatedPatient.LastName);
        }
        [Fact]
        public async Task AdminUpdatePatientProfile_NullPatchDocument_ReturnsBadRequest()
        {
            // Arrange
            var patientId = "user-123";
            JsonPatchDocument<UpdatePatientDto> patchDoc = null;

            SetupAdminUserContext(_controller);

            // Act
            var result = await _controller.AdminUpdatePatientProfile(patientId, patchDoc);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.Equal("Patch document is null", badRequestResult.Value);
        }
        [Fact]
        public async Task RegisterPatient_ValidDto_ReturnsOkResult()
        {
            // Arrange
            var registerDto = new RegisterPatientDto
            {
                FirstName = "John",
                LastName = "Doe",
                Email = "john.doe@example.com",
                DateofBirth = "01/01/1990",
                Gender = "Male",
                ContactInfo = "123 Elm Street",
                EmergencyContact = "Jane Doe",
                PhoneNumber = "1234567890"
            };

            var patientDto = new PatientDto("med-123", "user-123", registerDto.FirstName, registerDto.LastName, registerDto.Email, registerDto.DateofBirth, registerDto.Gender, registerDto.ContactInfo, registerDto.EmergencyContact, registerDto.PhoneNumber, null, null);

            _mockRepository.Setup(r => r.GetByUserIdAsync(It.IsAny<string>())).ReturnsAsync((Patient)null);
            _mockRepository.Setup(r => r.AddAsync(It.IsAny<Patient>())).ReturnsAsync(new Patient("med-123", "user-123", registerDto.FirstName, registerDto.LastName, registerDto.Email, registerDto.DateofBirth, registerDto.Gender, registerDto.ContactInfo, registerDto.EmergencyContact, registerDto.PhoneNumber, null, null));
            _mockUserManager.Setup(u => u.FindByEmailAsync(registerDto.Email)).ReturnsAsync(new ApplicationUser { Id = "user-123", Email = registerDto.Email });
            _mockUnitOfWork.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _controller.RegisterPatient(registerDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnedPatient = Assert.IsType<PatientDto>(okResult.Value);
            Assert.Equal(registerDto.FirstName, returnedPatient.FirstName);
            Assert.Equal(registerDto.LastName, returnedPatient.LastName);
            Assert.Equal(registerDto.Email, returnedPatient.Email);
        }
         
        

        


        
        
    }
}*/