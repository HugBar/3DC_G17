using System;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.UserData;
using Microsoft.AspNetCore.Identity;
using System.Collections.Generic;

namespace DDDSample1.Domain.StaffData
{
    public class StaffService
    {
        private readonly IStaffRepository _staffRepo;
        private readonly UserManager<ApplicationUser> _userManager;
        private readonly IUnitOfWork _unitOfWork;

        private readonly ILoggingService _loggingService;
        private readonly IEmailService _emailService;

        public StaffService(IStaffRepository staffRepo, UserManager<ApplicationUser> userManager, IUnitOfWork unitOfWork, ILoggingService loggingService, IEmailService emailService)
        {
            {
                _staffRepo = staffRepo;
                _userManager = userManager;
                _unitOfWork = unitOfWork;
                _loggingService = loggingService;
                _emailService = emailService;
            }
        }

        public async Task<StaffDto> AddAsync(CreateStaffDto dto)
        {
            if (!await _staffRepo.IsEmailUniqueAsync(dto.Email))
                throw new InvalidOperationException("Email already exists.");

            if (!await _staffRepo.IsPhoneNumberUniqueAsync(dto.PhoneNumber))
                throw new InvalidOperationException("Phone number already exists.");

            // Get the user associated with this email
            var user = await _userManager.FindByEmailAsync(dto.Email);
            if (user == null)
                throw new InvalidOperationException("No user found with this email.");


            var licenseNumber = GenerateUniqueLicenseNumber();

            var staff = new Staff(user.Id, dto.FirstName, dto.LastName, dto.Email, dto.PhoneNumber, dto.Specialization, licenseNumber);

            try
            {
                foreach (var slot in dto.AvailabilitySlots)
                {
                    staff.AddAvailabilitySlot(slot.StartTime, slot.EndTime);
                }
            }
            catch (BusinessRuleValidationException ex)
            {
                _loggingService.LogWarning($"Failed to add availability slot: {ex.Message}");
                throw;
            }


            await _staffRepo.AddAsync(staff);
            await _unitOfWork.CommitAsync();

            _loggingService.LogInformation($"New staff member created: {staff.Id}");


            return new StaffDto(staff.Id, staff.FirstName, staff.LastName, staff.Email, staff.PhoneNumber, staff.Specialization, staff.LicenseNumber, staff.Active, staff.AvailabilitySlots);

        }



        private string GenerateUniqueLicenseNumber()
        {
            return "LIC-" + Guid.NewGuid().ToString().Substring(0, 8).ToUpper();
        }

        public bool IsSlotAvailable(string staffId, DateTime startTime, DateTime endTime)
        {
            var staff = _staffRepo.GetByIdAsync(staffId).Result;
            if (staff == null) return false;

            return !staff.AvailabilitySlots.Any(slot =>
                (startTime >= slot.StartTime && startTime < slot.EndTime) ||
                (endTime > slot.StartTime && endTime <= slot.EndTime) ||
                (startTime <= slot.StartTime && endTime >= slot.EndTime));
        }

        public async Task<StaffDto> UpdateStaffAsync(string id, UpdateStaffDto dto)
        {
            var staff = await _staffRepo.GetByIdAsync(id);
            if (staff == null)
                throw new NotFoundException("Staff not found.");

            bool contactInfoChanged = false;

            if (!await _staffRepo.IsEmailUniqueAsync(dto.Email) && dto.Email != staff.Email)
            {
                throw new BusinessRuleValidationException("Email is already in use.");
            }

            if (!await _staffRepo.IsPhoneNumberUniqueAsync(dto.PhoneNumber) && dto.PhoneNumber != staff.PhoneNumber)
            {
                throw new BusinessRuleValidationException("Phone number is already in use.");
            }

            if (dto.Email != staff.Email && !string.IsNullOrEmpty(dto.Email))
            {
                contactInfoChanged = true;
            }

            if (!string.IsNullOrEmpty(dto.PhoneNumber) && dto.PhoneNumber != staff.PhoneNumber)
            {
                contactInfoChanged = true;
            }

            if (dto.Specialization != null)
            {
                if (string.IsNullOrWhiteSpace(dto.Specialization))
                    throw new BusinessRuleValidationException("Specialization cannot be empty.");
                staff.UpdateSpecialization(dto.Specialization);
            }

            if (dto.AvailabilitySlots != null)
            {
                staff.ClearAvailabilitySlots();
                try
                {
                    foreach (var slot in dto.AvailabilitySlots)
                    {
                        staff.AddAvailabilitySlot(slot.StartTime, slot.EndTime);
                    }
                }
                catch (BusinessRuleValidationException ex)
                {
                    _loggingService.LogWarning($"Failed to update availability slots: {ex.Message}");
                    throw;
                }
            }

            if (contactInfoChanged)
            {

                var user = await _userManager.FindByEmailAsync(staff.Email);
                staff.UpdateContactInfo(dto.Email, dto.PhoneNumber);
                await SendContactInfoUpdateEmail(staff);

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
                    _loggingService.LogWarning($"User account not found for staff: {staff.Id}");
                }
            }

            await _staffRepo.UpdateAsync(staff);
            await _unitOfWork.CommitAsync();

            _loggingService.LogInformation($"Staff profile updated: {staff.Id}");

            return new StaffDto(staff.Id, staff.FirstName, staff.LastName, staff.Email, staff.PhoneNumber, staff.Specialization, staff.LicenseNumber, staff.Active, staff.AvailabilitySlots);

        }



        public async Task<bool> DeactivateAsync(string id)
        {
            var staff = await _staffRepo.GetByIdAsync(id);
            if (staff == null)
            {
                _loggingService.LogError($"Staff member not found: ID {id}");  // Adicionando log de erro aqui
                return false;
            }

            try
            {
                staff.Deactivate();
                await _staffRepo.UpdateAsync(staff);
                await _unitOfWork.CommitAsync();
                _loggingService.LogInformation($"Staff member deactivated: ID {id}, Name: {staff.FirstName} {staff.LastName}");
                return true;
            }
            catch (BusinessRuleValidationException ex)
            {
                // Log the exception
                _loggingService.LogWarning($"Failed to deactivate staff: {ex.Message}");

                // Re-throw the exception
                throw;
            }
            catch (Exception ex)
            {
                // Log the exception
                _loggingService.LogError($"An error occurred while deactivating staff: {ex.Message}");

                // Re-throw the exception
                throw;
            }


        }



        public async Task<StaffDto> GetByIdAsync(string id)
        {
            var staff = await _staffRepo.GetByIdAsync(id);
            if (staff == null)
                return null;
            return new StaffDto(staff.Id, staff.FirstName, staff.LastName, staff.Email, staff.PhoneNumber, staff.Specialization, staff.LicenseNumber, staff.Active, staff.AvailabilitySlots);

        }

        private async Task SendContactInfoUpdateEmail(Staff staff)
        {
            string subject = "Contact Information Updated";
            string body = $"Dear {staff.FirstName} {staff.LastName},\n\n" +
                          $"Your contact information has been updated.\n" +
                          $"New Email: {staff.Email}\n" +
                          $"New Phone Number: {staff.PhoneNumber}\n\n" +
                          $"If you did not make this change, please contact the administrator immediately.";

            await _emailService.SendEmailAsync(staff.Email, subject, body);
        }

        public async Task<IEnumerable<StaffDto>> getStaffFilteredAsync(StaffFilterDto filter)
        {
            var staffs = await _staffRepo.GetFilteredStaffAsync(filter);

            return staffs.Select(s => new StaffDto(
                s.Id,
                s.FirstName,
                s.LastName,
                s.Email,
                s.PhoneNumber,
                s.Specialization,
                s.LicenseNumber,
                s.Active,
                s.AvailabilitySlots
            )).ToList();
        }

        public async Task<bool> DeleteAsync(string id)
        {
            var staff = await _staffRepo.GetByIdAsync(id);
            if (staff == null)
                return false;
            _staffRepo.Remove(staff);
            await _unitOfWork.CommitAsync();
            return true;
        }

        public async Task<List<Staff>> GetDeactivatedStaffAsync()
        {
            return await _staffRepo.GetDeactivatedStaffAsync();
        }
    }
}
