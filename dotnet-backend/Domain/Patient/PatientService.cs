using System;
using System.Threading.Tasks;
using DDDSample1.Domain.PatientData;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Identity;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Security.Claims;
using Microsoft.IdentityModel.Tokens;
using System.Text;
using System.IdentityModel.Tokens.Jwt;



namespace DDDSample1.Domain.PatientData
{
    public class PatientService
    {
        private readonly IPatientRepository _repository;

        private readonly UserManager<ApplicationUser> _userManager;

        private readonly IUnitOfWork _unitOfWork;

        private readonly IEmailService _emailService;

        private readonly ILoggingService _loggingService;

        private readonly IMedicalRecordService _medicalRecordService;

        public PatientService(IPatientRepository repository, UserManager<ApplicationUser> userManager, IUnitOfWork unitOfWork, IEmailService emailService, ILoggingService loggingService, IMedicalRecordService medicalRecordService)
        {
            _repository = repository;
            _userManager = userManager;
            _unitOfWork = unitOfWork;
            _emailService = emailService;
            _loggingService = loggingService;
            _medicalRecordService = medicalRecordService;

        }

        public async Task<PatientDto> AddAsync(RegisterPatientDto dto)
        {
            var user = await _userManager.FindByEmailAsync(dto.Email);
            if (user == null)
            {
                throw new BusinessRuleValidationException("User not found.");
            }

            var existingpatient = await _repository.GetByUserIdAsync(user.Id);
            if (existingpatient != null)
            {
                throw new BusinessRuleValidationException("A patient profile for this user already exists.");
            }

            var MedicalNr = MedicalRecordNumber.Generate();

            var newPatient = new Patient(
                MedicalNr,
                user.Id,
                dto.FirstName,
                dto.LastName,
                dto.Email,
                dto.DateofBirth,
                dto.Gender,
                dto.ContactInfo,
                dto.EmergencyContact,
                dto.PhoneNumber,
                dto.AppointmentHistory,
                dto.MedicalHistory
            );

            await _repository.AddAsync(newPatient);
            await _unitOfWork.CommitAsync();

            // Create blank medical record
            await _medicalRecordService.CreateBlankMedicalRecord(newPatient.MedicalNr);

            return new PatientDto(
                newPatient.MedicalNr,
                newPatient.UserId,
                newPatient.FirstName,
                newPatient.LastName,
                newPatient.Email,
                newPatient.DateofBirth,
                newPatient.Gender,
                newPatient.ContactInfo,
                newPatient.EmergencyContact,
                newPatient.PhoneNumber,
                newPatient.AppointmentHistory,
                newPatient.MedicalHistory);
        }

        public async Task<PagedResult<PatientDto>> GetFilteredPatient(PatientFilterDTO filter, int pageNumber, int pageSize)
        {
            var (patients, totalCount) = await _repository.GetFilteredPatientAsync(filter, pageNumber, pageSize);

            var dtos = patients.Select(p => new PatientDto
            {
                Id = p.UserId,
                FirstName = p.FirstName,
                LastName = p.LastName,
                Email = p.Email,
                PhoneNumber = p.PhoneNumber,
                DateofBirth = p.DateofBirth,
                ContactInfo = p.ContactInfo,
                EmergencyContact = p.EmergencyContact,
                AppointmentHistory = p.AppointmentHistory,
                MedicalHistory = p.MedicalHistory,
                MedicalNr = p.MedicalNr
            }).ToList();

            return new PagedResult<PatientDto>
            {
                Items = dtos,
                TotalCount = totalCount,
                PageNumber = pageNumber,
                PageSize = pageSize,
            };
        }
        public async Task<PatientDto> UpdatePatientProfileAsync(string email, UpdatePatientDto dto)
        {
            var patient = await _repository.GetByEmailAsync(email);
            if (patient == null)
            {
                throw new Exception("Patient not found.");
            }

            bool contactInfoChanged = false;

            if (!string.IsNullOrEmpty(dto.Email) && dto.Email != patient.Email)
            {
                if (!await _repository.IsEmailUniqueAsync(dto.Email))
                {
                    throw new BusinessRuleValidationException("Email is already in use.");
                }
                contactInfoChanged = true;
            }

            if (!string.IsNullOrEmpty(dto.PhoneNumber) && dto.PhoneNumber != patient.PhoneNumber)
            {
                contactInfoChanged = true;
            }

            if (contactInfoChanged)
            {
                patient.UpdateContactInfo(dto.Email ?? patient.Email, dto.PhoneNumber ?? patient.PhoneNumber);
                await SendContactInfoUpdateEmail(patient);
            }
            patient.UpdateContactInfo(dto.Email ?? patient.Email, dto.PhoneNumber ?? patient.PhoneNumber);
            await SendContactInfoUpdateEmail(patient);

            var updatedPatient = await _repository.GetByEmailAsync(email);
            var user = await _userManager.FindByIdAsync(updatedPatient.UserId);
            if (user != null)

            {
                user.Email = dto.Email;
                user.NormalizedEmail = dto.Email.ToUpper();

                var updateResult = await _userManager.UpdateAsync(user);
                if (!updateResult.Succeeded)
                {
                    throw new BusinessRuleValidationException("Failed to update user email in AspNetUsers table.");
                }
            }
            else
            {
                _loggingService.LogWarning($"User account not found for patient: {patient.UserId}");
            }




            // Update other fields only if they are not null
            if (dto.FirstName != null) patient.FirstName = dto.FirstName;
            if (dto.LastName != null) patient.LastName = dto.LastName;
            if (dto.ContactInfo != null) patient.ContactInfo = dto.ContactInfo;
            if (dto.EmergencyContact != null) patient.EmergencyContact = dto.EmergencyContact;
            if (dto.Gender != null) patient.Gender = dto.Gender;
            if (dto.DateOfBirth != null) patient.DateofBirth = dto.DateOfBirth;

            await _repository.UpdateAsync(patient);
            await _unitOfWork.CommitAsync();

            _loggingService.LogInformation($"Patient profile updated: {patient.UserId}");

            return new PatientDto
            {
                Id = patient.UserId,
                FirstName = patient.FirstName,
                LastName = patient.LastName,
                Email = patient.Email,
                PhoneNumber = patient.PhoneNumber,
                DateofBirth = patient.DateofBirth,
                Gender = patient.Gender,
                ContactInfo = patient.ContactInfo,
                EmergencyContact = patient.EmergencyContact,
                MedicalNr = patient.MedicalNr
            };
        }
        public async Task<PatientDto> GetByEmailAsync(string email)
        {
            var patient = await _repository.GetByEmailAsync(email);
            if (patient == null)
            {
                return null;
            }

            return new PatientDto
            {
                MedicalNr = patient.MedicalNr,
                Id = patient.UserId,
                FirstName = patient.FirstName,
                LastName = patient.LastName,
                Email = patient.Email,
                DateofBirth = patient.DateofBirth,
                Gender = patient.Gender,
                ContactInfo = patient.ContactInfo,
                EmergencyContact = patient.EmergencyContact,
                PhoneNumber = patient.PhoneNumber,
                AppointmentHistory = patient.AppointmentHistory,
                MedicalHistory = patient.MedicalHistory
            };
        }

        private async Task SendContactInfoUpdateEmail(Patient patient)
        {
            string subject = "Contact Information Updated";
            string body = $"Dear {patient.FirstName} {patient.LastName},\n\n" +
                          $"Your contact information has been updated.\n" +
                          $"New Email: {patient.Email}\n" +
                          $"New Phone Number: {patient.PhoneNumber}\n\n" +
                          $"If you did not make this change, please contact the administrator immediately.";

            await _emailService.SendEmailAsync(patient.Email, subject, body);
        }

        public async Task<bool> DeletePatientAsync(string id)
        {
            var patient = await _repository.GetByUserIdAsync(id);
            if (patient == null)
            {
                return false;
            }


            await _loggingService.LogChangeAsync("Patient deleted", id, $"{patient.FirstName} {patient.LastName}", null);

            await _repository.RemoveAsync(patient);
            await _unitOfWork.CommitAsync();

            return true;
        }

        public async Task<PatientDto> AdminUpdatePatientProfileAsync(string id, UpdatePatientDto dto)
        {
            var patient = await _repository.GetByIdAsync(id);
            if (patient == null)
                throw new NotFoundException("Patient not found.");

            Console.WriteLine(dto.Email);

            if (!await _repository.IsEmailUniqueAsync(dto.Email) && dto.Email != patient.Email)
            {
                throw new BusinessRuleValidationException("Email is already in use.");
            }
            if (!await _repository.IsPhoneNumberUniqueAsync(dto.PhoneNumber) && dto.PhoneNumber != patient.PhoneNumber)
            {
                throw new BusinessRuleValidationException("Phone number is already in use.");
            }

            bool contactInfoChanged = false;

            if (dto.Email != patient.Email && !string.IsNullOrEmpty(dto.Email))
            {
                contactInfoChanged = true;
            }

            if (!string.IsNullOrEmpty(dto.PhoneNumber) && dto.PhoneNumber != patient.PhoneNumber)
            {
                contactInfoChanged = true;
            }

            // Update other fields as necessary
            if (dto.FirstName != null)
                patient.FirstName = dto.FirstName;
            if (dto.LastName != null)
                patient.LastName = dto.LastName;
            if (dto.DateOfBirth != null)
                patient.DateofBirth = dto.DateOfBirth;
            if (dto.Gender != null)
                patient.Gender = dto.Gender;
            if (dto.ContactInfo != null)
                patient.ContactInfo = dto.ContactInfo;
            if (dto.EmergencyContact != null)
                patient.EmergencyContact = dto.EmergencyContact;

            if (contactInfoChanged)
            {
                patient.UpdateContactInfo(dto.Email, dto.PhoneNumber);
                await SendContactInfoUpdateEmail(patient);
            }

            await _repository.UpdateAsync(patient);
            await _unitOfWork.CommitAsync();

            _loggingService.LogInformation($"Patient profile updated by admin: {patient.UserId}");

            return new PatientDto(patient.MedicalNr, patient.UserId, patient.FirstName, patient.LastName, patient.Email, patient.DateofBirth, patient.Gender, patient.ContactInfo, patient.EmergencyContact, patient.PhoneNumber, patient.AppointmentHistory, patient.MedicalHistory);

        }

        public async Task<PatientDto> GetByIdAsync(string id)
        {
            var patient = await _repository.GetByIdAsync(id);
            if (patient == null)
                return null;

            return new PatientDto(
                patient.MedicalNr,
                patient.UserId,
                patient.FirstName,
                patient.LastName,
                patient.Email,
                patient.DateofBirth,
                patient.Gender,
                patient.ContactInfo,
                patient.EmergencyContact,
                patient.PhoneNumber,
                patient.AppointmentHistory,
                patient.MedicalHistory

            );
        }
        public string GenerateToken(string email)
        {
            var claims = new List<Claim>
    {
        new Claim(JwtRegisteredClaimNames.Email, email),
        new Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()),
        new Claim("tokenPurpose", "accountDeletion") // Propósito específico para exclusão de conta
    };

            var key = new SymmetricSecurityKey(Encoding.UTF8.GetBytes("your-very-long-secret-key-with-32-characters-at-least"));
            var creds = new SigningCredentials(key, SecurityAlgorithms.HmacSha256);

            // O token vai expirar em 1 hora
            var token = new JwtSecurityToken(
                issuer: "YourIssuer",
                audience: "YourAudience",
                claims: claims,
                expires: DateTime.Now.AddHours(1),
                signingCredentials: creds
            );

            return new JwtSecurityTokenHandler().WriteToken(token);
        }

        public async Task RequestAccountDeletionAsync(string email)
        {
            var patient = await _repository.GetByEmailAsync(email);
            if (patient == null)
            {
                throw new Exception("Patient not found.");
            }

            // Gera o token JWT para confirmar a exclusão da conta
            string token = GenerateToken(patient.Email);
            string confirmationLink = $"https://localhost:5001/api/patient/account-deletion?token={token}";

            string subject = "Confirm Account Deletion";
            string body = $"Dear {patient.FirstName} {patient.LastName},\n\n" +
                          $"We received a request to delete your account. Please confirm your request by clicking the link below:\n" +
                          $"{confirmationLink}\n\n" +
                          $"If you did not request this, please ignore this email.";

            await _emailService.SendEmailAsync(patient.Email, subject, body);
        }

        public string ValidateTokenAndGetEmail(string token)
        {
            // Decodificar o token
            var decodedToken = Uri.UnescapeDataString(token);

            var tokenHandler = new JwtSecurityTokenHandler();
            var key = Encoding.ASCII.GetBytes("your-very-long-secret-key-with-32-characters-at-least");

            try
            {
                tokenHandler.ValidateToken(decodedToken, new TokenValidationParameters
                {
                    ValidateIssuerSigningKey = true,
                    IssuerSigningKey = new SymmetricSecurityKey(key),
                    ValidateIssuer = false,
                    ValidateAudience = false,
                    ClockSkew = TimeSpan.FromMinutes(10)
                }, out SecurityToken validatedToken);

                var jwtToken = (JwtSecurityToken)validatedToken;

                var email = jwtToken.Claims.First(x => x.Type == JwtRegisteredClaimNames.Email).Value;
                var purpose = jwtToken.Claims.FirstOrDefault(x => x.Type == "tokenPurpose")?.Value;

                if (purpose != "accountDeletion")
                {
                    throw new SecurityTokenException("Invalid token purpose.");
                }

                return email;
            }
            catch
            {
                throw new SecurityTokenException("Invalid token.");
            }
        }

        public async Task<bool> ConfirmAccountDeletionAsync(string email)
        {
            var patient = await _repository.GetByEmailAsync(email);
            if (patient == null)
            {
                return false;
            }

            try
            {
                var contactInfo = new
                {
                    Email = patient.Email,
                    FirstName = patient.FirstName,
                    LastName = patient.LastName
                };

                await _unitOfWork.BeginTransactionAsync();

                patient.Anonymize();

                await _repository.UpdateAsync(patient);

                await _loggingService.LogChangeAsync(
                    "Patient data anonymized",
                    patient.UserId,
                    $"Patient data anonymized and retained for research/legal purposes. Original ID: {patient.UserId}",
                    null
                );

                var user = await _userManager.FindByEmailAsync(email);
                if (user != null)
                {
                    var userRoles = await _userManager.GetRolesAsync(user);
                    await _userManager.RemoveFromRolesAsync(user, userRoles);
                    await _userManager.DeleteAsync(user);
                }

                await _unitOfWork.CommitTransactionAsync();

                string subject = "Account Deletion Complete";
                string body = $"Dear {contactInfo.FirstName} {contactInfo.LastName},\n\n" +
                             $"Your account has been successfully deleted and your personal information has been anonymized in our system.\n" +
                             $"In accordance with our data retention policy and legal requirements, some non-identifiable data " +
                             $"will be retained in an anonymized form for research and legal purposes.\n\n" +
                             $"Thank you for understanding.\n\n" +
                             $"Best regards,\nMedical System Team";

                await _emailService.SendEmailAsync(contactInfo.Email, subject, body);

                return true;
            }
            catch (Exception ex)
            {
                await _unitOfWork.RollbackTransactionAsync();
                _loggingService.LogError($"Error during account deletion and anonymization: {ex.Message}");
                throw;
            }
        }

        public async Task<bool> ExistsByMedicalNumberAsync(string medicalNumber)
        {
            var patient = await _repository.GetByMedicalRecordNumberAsync(medicalNumber);
            return patient != null;
        }





    }



}