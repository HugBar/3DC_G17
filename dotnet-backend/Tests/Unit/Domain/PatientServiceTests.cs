using System;
using System.Threading.Tasks;
using DDDSample1.Domain.PatientData;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Identity;
using Moq;
using Xunit;
using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding.Validation;
using System.Collections.Generic;
using System.Linq;
using Microsoft.IdentityModel.Tokens;
using Microsoft.AspNetCore.Mvc;


namespace DDDSample1.Tests.Unit.Domain
{
    public class PatientServiceTests
    {
        private readonly Mock<IPatientRepository> _mockPatientRepo;
        private readonly Mock<UserManager<ApplicationUser>> _mockUserManager;
        private readonly Mock<IUnitOfWork> _mockUnitOfWork;
        private readonly Mock<ILoggingService> _mockLoggingService;
        private readonly Mock<IEmailService> _mockEmailService;
        private readonly PatientService _patientService;
        

        public PatientServiceTests()
        {
            _mockPatientRepo = new Mock<IPatientRepository>();
            _mockUserManager = new Mock<UserManager<ApplicationUser>>(
                Mock.Of<IUserStore<ApplicationUser>>(), null, null, null, null, null, null, null, null);
            _mockUnitOfWork = new Mock<IUnitOfWork>();
            _mockLoggingService = new Mock<ILoggingService>();
            _mockEmailService = new Mock<IEmailService>();

            _patientService = new PatientService(
                _mockPatientRepo.Object,
                _mockUserManager.Object,
                _mockUnitOfWork.Object,
                _mockEmailService.Object,
                _mockLoggingService.Object
            );
        }
        private IList<ValidationResult> ValidateModel(object model)
        {
            var validationResults = new List<ValidationResult>();
            var ctx = new ValidationContext(model, null, null);
            Validator.TryValidateObject(model, ctx, validationResults, true);
            return validationResults;
        }

        [Fact]
        public async Task UpdatePatientProfileAsync_ValidUpdate_ReturnsUpdatedPatient()
        {
            var email = "patient@example.com";
            var updateDto = new UpdatePatientDto 
            { 
                FirstName = "Jane", 
                LastName = "Doe", 
                PhoneNumber = "9876543210",
                Email = email,
                ContactInfo = "New Contact",
                EmergencyContact = "New Emergency",
                Gender = "Female",
                DateOfBirth = "02/02/1991"
            };
            var existingPatient = new Patient("med-123", "user-123", "John", "Doe", email, "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);

            _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync(existingPatient);
            _mockPatientRepo.Setup(r => r.IsEmailUniqueAsync(It.IsAny<string>())).ReturnsAsync(true);
            _mockPatientRepo.Setup(r => r.UpdateAsync(It.IsAny<Patient>())).ReturnsAsync(existingPatient);
            _mockUnitOfWork.Setup(u => u.CommitAsync()).ReturnsAsync(1);
            _mockLoggingService.Setup(l => l.LogInformation(It.IsAny<string>()));


            var result = await _patientService.UpdatePatientProfileAsync(email, updateDto);

            Assert.NotNull(result);
            Assert.Equal(updateDto.FirstName, result.FirstName);
            Assert.Equal(updateDto.LastName, result.LastName);
            Assert.Equal(updateDto.PhoneNumber, result.PhoneNumber);
            Assert.Equal(updateDto.Email, result.Email);
            Assert.Equal(updateDto.ContactInfo, result.ContactInfo);
            Assert.Equal(updateDto.EmergencyContact, result.EmergencyContact);
            Assert.Equal(updateDto.Gender, result.Gender);
            Assert.Equal(updateDto.DateOfBirth, result.DateofBirth);
        }       
        [Fact]
        public async Task UpdatePatientProfileAsync_PatientNotFound_ThrowsException()
        {
            var email = "nonexistent@example.com";
            var updateDto = new UpdatePatientDto();

            _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync((Patient)null);

            await Assert.ThrowsAsync<Exception>(() => _patientService.UpdatePatientProfileAsync(email, updateDto));
        }

        [Fact]
        public async Task UpdatePatientProfileAsync_NoChanges_ReturnsUnchangedPatient()
        {
            var email = "patient@example.com";
            var updateDto = new UpdatePatientDto();
            var existingPatient = new Patient("med-123", "user-123", "John", "Doe", email, "01/01/1990", "Male", "Contact", "Emergency", "1234567890");

            _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync(existingPatient);
            _mockPatientRepo.Setup(r => r.UpdateAsync(It.IsAny<Patient>())).ReturnsAsync(existingPatient);
            _mockUnitOfWork.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            var result = await _patientService.UpdatePatientProfileAsync(email, updateDto);

            Assert.NotNull(result);
            Assert.Equal(existingPatient.FirstName, result.FirstName);
            Assert.Equal(existingPatient.LastName, result.LastName);
            Assert.Equal(existingPatient.Email, result.Email);
            Assert.Equal(existingPatient.PhoneNumber, result.PhoneNumber);
            Assert.Equal(existingPatient.DateofBirth, result.DateofBirth);
            Assert.Equal(existingPatient.Gender, result.Gender);
            Assert.Equal(existingPatient.ContactInfo, result.ContactInfo);
            Assert.Equal(existingPatient.EmergencyContact, result.EmergencyContact);
            Assert.Equal(existingPatient.MedicalNr, result.MedicalNr);

            _mockPatientRepo.Verify(r => r.UpdateAsync(It.IsAny<Patient>()), Times.Once);
            _mockUnitOfWork.Verify(u => u.CommitAsync(), Times.Once);
        }
        [Fact]
        public async Task UpdatePatientProfileAsync_EmailAlreadyExists_ThrowsException()
        {
            var existingEmail = "patient@example.com";
            var newEmail = "existing@example.com";
            var updateDto = new UpdatePatientDto 
            { 
                Email = newEmail
            };
            var existingPatient = new Patient("med-123", "user-123", "John", "Doe", existingEmail, "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);

            _mockPatientRepo.Setup(r => r.GetByEmailAsync(existingEmail)).ReturnsAsync(existingPatient);
            _mockPatientRepo.Setup(r => r.IsEmailUniqueAsync(newEmail)).ReturnsAsync(false);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => 
                _patientService.UpdatePatientProfileAsync(existingEmail, updateDto));

            _mockPatientRepo.Verify(r => r.UpdateAsync(It.IsAny<Patient>()), Times.Never);
            _mockUnitOfWork.Verify(u => u.CommitAsync(), Times.Never);
        }
        [Fact]
        public void UpdatePatientDto_InvalidEmail_FailsValidation()
        {
            var dto = new UpdatePatientDto { Email = "invalid-email" };

            var validationResults = ValidateModel(dto);

            Assert.Contains(validationResults, 
                v => v.MemberNames.Contains("Email") && 
                    v.ErrorMessage.Contains("Invalid email format"));
        }

        [Fact]
        public void UpdatePatientDto_InvalidPhoneNumber_FailsValidation()
        {
            var dto = new UpdatePatientDto { PhoneNumber = "123" };

            var validationResults = ValidateModel(dto);

            Assert.Contains(validationResults, 
                v => v.MemberNames.Contains("PhoneNumber") && 
                    v.ErrorMessage.Contains("Phone number must be exactly 9 digits"));
        }

        [Fact]
        public void UpdatePatientDto_InvalidMedicalNr_FailsValidation()
        {
            var dto = new UpdatePatientDto { MedicalNr = "INVALID" };

            var validationResults = ValidateModel(dto);

            Assert.Contains(validationResults, 
                v => v.MemberNames.Contains("MedicalNr") && 
                    v.ErrorMessage.Contains("Invalid medical number format"));
        }


        [Fact]
        public async Task DeletePatientAsync_ValidId_DeletesPatient()
        {
            var patientId = "valid-patient-id";
            var patient = new Patient();
            _mockPatientRepo.Setup(r => r.GetByUserIdAsync(patientId)).ReturnsAsync(patient);

            var result = await _patientService.DeletePatientAsync(patientId);

            Assert.True(result);
            _mockPatientRepo.Verify(r => r.RemoveAsync(patient), Times.Once);
            _mockUnitOfWork.Verify(u => u.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task DeletePatientAsync_InvalidId_ReturnsFalse()
        {
            var patientId = "invalid-patient-id";
            _mockPatientRepo.Setup(r => r.GetByUserIdAsync(patientId)).ReturnsAsync((Patient)null);

            var result = await _patientService.DeletePatientAsync(patientId);

            Assert.False(result);
        }

        [Fact]
        public async Task DeletePatientAsync_ValidId_LogsDeletion()
        {
            var patientId = "valid-patient-id";
            var patient = new Patient { FirstName = "John", LastName = "Doe" };
            _mockPatientRepo.Setup(r => r.GetByUserIdAsync(patientId)).ReturnsAsync(patient);

            await _patientService.DeletePatientAsync(patientId);

            _mockLoggingService.Verify(l => l.LogChangeAsync("Patient deleted", patientId, $"{patient.FirstName} {patient.LastName}", null), Times.Once);
        }
        [Fact]
    public async Task RequestAccountDeletionAsync_ValidEmail_SendsEmail()
    {
        // Arrange
        var email = "patient@example.com";
        var patient = new Patient("MED-123", "user-123", "John", "Doe", email, "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);
        _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync(patient);

        // Act
        await _patientService.RequestAccountDeletionAsync(email);

        // Assert
        _mockEmailService.Verify(e => e.SendEmailAsync(
            It.Is<string>(to => to == email),
            It.Is<string>(subject => subject.Contains("Confirm Account Deletion")),
            It.Is<string>(body => body.Contains("We received a request to delete your account"))
        ), Times.Once);
    }
    [Fact]
    public async Task RequestAccountDeletionAsync_InvalidEmail_ThrowsException()
    {
        // Arrange
        var email = "nonexistent@example.com";
        _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync((Patient)null);

        // Act & Assert
        await Assert.ThrowsAsync<Exception>(() => _patientService.RequestAccountDeletionAsync(email));
    }
    [Fact]
    public void ValidateTokenAndGetEmail_ValidToken_ReturnsEmail()
    {
        // Arrange
        var email = "patient@example.com";
        var token = _patientService.GenerateToken(email);

        // Act
        var result = _patientService.ValidateTokenAndGetEmail(token);

        // Assert
        Assert.Equal(email, result);
    }
    [Fact]
    public void ValidateTokenAndGetEmail_InvalidToken_ThrowsException()
    {
        // Arrange
        var invalidToken = "invalid-token";

        // Act & Assert
        Assert.Throws<SecurityTokenException>(() => _patientService.ValidateTokenAndGetEmail(invalidToken));
    }
    [Fact]
    public async Task ConfirmAccountDeletionAsync_ValidEmail_DeletesAndAnonymizesAccount()
    {
        // Arrange
        var email = "patient@example.com";
        var patient = new Patient("MED-123", "user-123", "John", "Doe", email, "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);
        var user = new ApplicationUser { Email = email };

        _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync(patient);
        _mockUserManager.Setup(u => u.FindByEmailAsync(email)).ReturnsAsync(user);
        _mockUserManager.Setup(u => u.GetRolesAsync(user)).ReturnsAsync(new List<string>());
        _mockUserManager.Setup(u => u.RemoveFromRolesAsync(user, It.IsAny<IEnumerable<string>>())).ReturnsAsync(IdentityResult.Success);
        _mockUserManager.Setup(u => u.DeleteAsync(user)).ReturnsAsync(IdentityResult.Success);

        // Act
        var result = await _patientService.ConfirmAccountDeletionAsync(email);

        // Assert
        Assert.True(result);
        _mockPatientRepo.Verify(r => r.UpdateAsync(It.Is<Patient>(p => p.IsAnonymized)), Times.Once);
        _mockUserManager.Verify(u => u.DeleteAsync(user), Times.Once);
        _mockEmailService.Verify(e => e.SendEmailAsync(
            It.Is<string>(to => to == email),
            It.Is<string>(subject => subject.Contains("Account Deletion Complete")),
            It.Is<string>(body => body.Contains("Your account has been successfully deleted"))
        ), Times.Once);
    }
    [Fact]
    public async Task ConfirmAccountDeletionAsync_InvalidEmail_ReturnsFalse()
    {
        // Arrange
        var email = "nonexistent@example.com";
        _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync((Patient)null);

        // Act
        var result = await _patientService.ConfirmAccountDeletionAsync(email);

        // Assert
        Assert.False(result);
    }
    [Fact]
        public async Task RequestAccountDeletionAsync_ValidEmail_SendsConfirmationEmail()
        {
            // Arrange
            var email = "patient@example.com";
            var patient = new Patient("MED-123", "user-123", "John", "Doe", email, "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);
            _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync(patient);

            // Act
            await _patientService.RequestAccountDeletionAsync(email);

            // Assert
            _mockEmailService.Verify(e => e.SendEmailAsync(
                It.Is<string>(to => to == email),
                It.Is<string>(subject => subject.Contains("Confirm Account Deletion")),
                It.Is<string>(body => body.Contains("We received a request to delete your account"))
            ), Times.Once);
        }
        [Fact]
        public async Task ConfirmAccountDeletionAsync_ValidEmail_AnonymizesPatientData()
        {
            // Arrange
            var email = "patient@example.com";
            var patient = new Patient("MED-123", "user-123", "John", "Doe", email, "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);
            var user = new ApplicationUser { Email = email };

            _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync(patient);
            _mockUserManager.Setup(u => u.FindByEmailAsync(email)).ReturnsAsync(user);
            _mockUserManager.Setup(u => u.GetRolesAsync(user)).ReturnsAsync(new List<string>());
            _mockUserManager.Setup(u => u.RemoveFromRolesAsync(user, It.IsAny<IEnumerable<string>>())).ReturnsAsync(IdentityResult.Success);
            _mockUserManager.Setup(u => u.DeleteAsync(user)).ReturnsAsync(IdentityResult.Success);

            // Act
            var result = await _patientService.ConfirmAccountDeletionAsync(email);

            // Assert
            Assert.True(result);
            _mockPatientRepo.Verify(r => r.UpdateAsync(It.Is<Patient>(p => p.IsAnonymized)), Times.Once);
            _mockUserManager.Verify(u => u.DeleteAsync(user), Times.Once);
            _mockEmailService.Verify(e => e.SendEmailAsync(
                It.Is<string>(to => to == email),
                It.Is<string>(subject => subject.Contains("Account Deletion Complete")),
                It.Is<string>(body => body.Contains("Your account has been successfully deleted"))
            ), Times.Once);
        }
        [Fact]
        public async Task ConfirmAccountDeletionAsync_TransactionFails_RollsBackChanges()
        {
            // Arrange
            var email = "patient@example.com";
            var patient = new Patient("MED-123", "user-123", "John", "Doe", email, "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);
            var user = new ApplicationUser { Email = email };

            _mockPatientRepo.Setup(r => r.GetByEmailAsync(email)).ReturnsAsync(patient);
            _mockUserManager.Setup(u => u.FindByEmailAsync(email)).ReturnsAsync(user);
            _mockUserManager.Setup(u => u.GetRolesAsync(user)).ReturnsAsync(new List<string>());
            _mockUserManager.Setup(u => u.RemoveFromRolesAsync(user, It.IsAny<IEnumerable<string>>())).ReturnsAsync(IdentityResult.Success);
            _mockUserManager.Setup(u => u.DeleteAsync(user)).ThrowsAsync(new Exception("Database error"));

            // Act & Assert
            await Assert.ThrowsAsync<Exception>(() => _patientService.ConfirmAccountDeletionAsync(email));

            // Ensure rollback was called
            _mockUnitOfWork.Verify(u => u.RollbackTransactionAsync(), Times.Once);
        }


        
        [Fact]
        public async Task GetPatientFilteredAsync_ValidFilter_ReturnsMatchingPatientDtos()
        {
            var filter = new PatientFilterDTO
            {
                FirstName = "John"
            };

            var patients = new List<Patient>
            {
                new Patient("MED-123", "user-123", "John", "Doe", "john.doe@example.com", "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null),
                new Patient("MED-456", "user-456", "Jane", "Doe", "jane.doe@example.com", "02/02/1991", "Female", "Contact", "Emergency", "9876543210", null, null)
            };

            _mockPatientRepo.Setup(repo => repo.GetFilteredPatientAsync(It.IsAny<PatientFilterDTO>(), 1, 5)).ReturnsAsync((patients.Where(p => p.FirstName == filter.FirstName).ToList(), patients.Count));

            // Act
            var result = await _patientService.GetFilteredPatient(filter, 1, 5);

            // Assert
            Assert.NotNull(result);
            Assert.Single(result.Items);
            Assert.Equal(filter.FirstName, result.Items.First().FirstName);

            _mockPatientRepo.Verify(repo => repo.GetFilteredPatientAsync(It.IsAny<PatientFilterDTO>(), 1, 5), Times.Once);
        }
        
       [Fact]
        public async Task AdminUpdatePatientProfileAsync_ValidUpdate_ReturnsUpdatedPatient()
        {
            // Arrange
            var patientId = "user-123";
            var updateDto = new UpdatePatientDto
            {
                FirstName = "Jane",
                LastName = "Doe",
                PhoneNumber = "9876543210",
                Email = "jane.doe@example.com",
                ContactInfo = "Updated Contact",
                EmergencyContact = "Updated Emergency",
                Gender = "Female",
                DateOfBirth = "02/02/1991"
            };
            var existingPatient = new Patient("med-123", patientId, "John", "Doe", "john.doe@example.com", "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);

            _mockPatientRepo.Setup(r => r.GetByIdAsync(patientId)).ReturnsAsync(existingPatient);
            _mockPatientRepo.Setup(r => r.IsEmailUniqueAsync(updateDto.Email)).ReturnsAsync(true);
            _mockPatientRepo.Setup(r => r.IsPhoneNumberUniqueAsync(updateDto.PhoneNumber)).ReturnsAsync(true); // Adicione esta linha
            _mockPatientRepo.Setup(r => r.UpdateAsync(It.IsAny<Patient>())).ReturnsAsync(existingPatient);
            _mockUnitOfWork.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _patientService.AdminUpdatePatientProfileAsync(patientId, updateDto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal(updateDto.FirstName, result.FirstName);
            Assert.Equal(updateDto.LastName, result.LastName);
            Assert.Equal(updateDto.PhoneNumber, result.PhoneNumber);
            Assert.Equal(updateDto.Email, result.Email);
            Assert.Equal(updateDto.ContactInfo, result.ContactInfo);
            Assert.Equal(updateDto.EmergencyContact, result.EmergencyContact);
            Assert.Equal(updateDto.Gender, result.Gender);
            Assert.Equal(updateDto.DateOfBirth, result.DateofBirth);

            _mockPatientRepo.Verify(r => r.UpdateAsync(It.IsAny<Patient>()), Times.Once);
            _mockUnitOfWork.Verify(u => u.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task AdminUpdatePatientProfileAsync_EmailAlreadyExists_ThrowsException()
        {
            // Arrange
            var patientId = "user-123";
            var updateDto = new UpdatePatientDto { Email = "existing@example.com" };
            var existingPatient = new Patient("med-123", patientId, "John", "Doe", "patient@example.com", "01/01/1990", "Male", "Contact", "Emergency", "1234567890", null, null);

            _mockPatientRepo.Setup(r => r.GetByIdAsync(patientId)).ReturnsAsync(existingPatient);
            _mockPatientRepo.Setup(r => r.IsEmailUniqueAsync(updateDto.Email)).ReturnsAsync(false);

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _patientService.AdminUpdatePatientProfileAsync(patientId, updateDto));
        }
        [Fact]
        public async Task AddAsync_ValidPatient_ReturnsPatientDto()
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

            var user = new ApplicationUser { Id = "user-123", Email = registerDto.Email };
            var newPatient = new Patient("med-123", user.Id, registerDto.FirstName, registerDto.LastName, registerDto.Email, registerDto.DateofBirth, registerDto.Gender, registerDto.ContactInfo, registerDto.EmergencyContact, registerDto.PhoneNumber, null, null);

            _mockUserManager.Setup(u => u.FindByEmailAsync(registerDto.Email)).ReturnsAsync(user);
            _mockPatientRepo.Setup(r => r.GetByUserIdAsync(user.Id)).ReturnsAsync((Patient)null);
            _mockPatientRepo.Setup(r => r.AddAsync(It.IsAny<Patient>())).ReturnsAsync(newPatient); // Corrigido para retornar Task<Patient>
            _mockUnitOfWork.Setup(u => u.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _patientService.AddAsync(registerDto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal(registerDto.FirstName, result.FirstName);
            Assert.Equal(registerDto.LastName, result.LastName);
            Assert.Equal(registerDto.Email, result.Email);
            _mockPatientRepo.Verify(r => r.AddAsync(It.IsAny<Patient>()), Times.Once);
            _mockUnitOfWork.Verify(u => u.CommitAsync(), Times.Once);
        }
        [Fact]
        public async Task AddAsync_EmailAlreadyExists_ThrowsException()
        {
            // Arrange
            var registerDto = new RegisterPatientDto
            {
                FirstName = "John",
                LastName = "Doe",
                Email = "existing@example.com",
                DateofBirth = "01/01/1990",
                Gender = "Male",
                ContactInfo = "123 Elm Street",
                EmergencyContact = "Jane Doe",
                PhoneNumber = "1234567890"
            };

            var existingPatient = new Patient("med-123", "user-123", registerDto.FirstName, registerDto.LastName, registerDto.Email, registerDto.DateofBirth, registerDto.Gender, registerDto.ContactInfo, registerDto.EmergencyContact, registerDto.PhoneNumber, null, null);

            _mockPatientRepo.Setup(r => r.GetByEmailAsync(registerDto.Email)).ReturnsAsync(existingPatient);

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _patientService.AddAsync(registerDto));
        }

       

    }
}