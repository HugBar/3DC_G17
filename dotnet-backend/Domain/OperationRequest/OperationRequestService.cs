using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.PatientData;
using System.Collections.Generic;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Domain.OperationRequestData;
using System.Linq;

namespace DDDSample1.Domain.OperationRequestData
{
    public class OperationRequestService
    {
        private readonly IOperationRequestRepository _repository;
        private readonly IUnitOfWork _unitOfWork;

        private readonly IStaffRepository _staffRepo;
        private readonly IPatientRepository _patientRepo;

        private readonly ILoggingService _loggingService;

        private readonly IOperationTypeRepository _operationTypeRepo;

        public OperationRequestService(IOperationRequestRepository repository, IUnitOfWork unitOfWork, IStaffRepository staffrepo, IPatientRepository patientrepo, ILoggingService loggingService, IOperationTypeRepository operationTypeRepo)
        {
            _repository = repository;
            _unitOfWork = unitOfWork;
            _staffRepo = staffrepo;
            _patientRepo = patientrepo;
            _loggingService = loggingService;
            _operationTypeRepo = operationTypeRepo;
        }


        public async Task<OperationRequestDto> CreateOperationRequestAsync(CreateOperationRequestDto dto)
        {
            // Find patient by medical record number
            var patient = await _patientRepo.GetByMedicalRecordNumberAsync(dto.PatientMRN);
            if (patient == null)
            {
                throw new InvalidOperationException("No patient found with this Medical Record Number.");
            }

            // Find doctor by license number
            var doctor = await _staffRepo.GetByLicenseNumberAsync(dto.DoctorLicenseNumber);
            if (doctor == null)
            {
                throw new InvalidOperationException("No doctor found with this License Number or user is not a doctor.");
            }

            var operationType = await _operationTypeRepo.GetByIdAsync(new OperationTypeId(dto.OperationTypeId));
            if (operationType == null)
            {
                throw new BusinessRuleValidationException("Invalid operation type.");
            }

            var requiredSpecializations = operationType.RequiredStaffBySpecialization;

            if (!requiredSpecializations.ContainsKey(doctor.Specialization))
            {
                throw new BusinessRuleValidationException("Doctor's specialization does not match the required specialization for this operation type.");
            }

            var validPriorities = new List<string> { "elective", "urgent", "emergency" };
            if (!validPriorities.Contains(dto.Priority.ToLower()))
            {
                throw new InvalidOperationException("Invalid priority. Valid values are: elective, urgent, emergency.");
            }

            var operationRequestId = GenerateUniqueOperationRequestId();

            var operationRequest = new OperationRequest(
                operationRequestId,
                patient.UserId,  // Use the found patient's ID
                doctor.Id,   // Use the found doctor's ID
                dto.OperationTypeId,
                dto.Deadline,
                dto.Priority);

            try
            {
                await _repository.AddAsync(operationRequest);
                await _unitOfWork.CommitAsync();
            }
            catch (BusinessRuleValidationException ex)
            {
                _loggingService.LogWarning($"Failed to create operation request: {ex.Message}");
                throw;
            }

            _loggingService.LogInformation($"New operation request created: {operationRequest.Id}");

            return new OperationRequestDto(
                operationRequest.Id,
                patient.UserId,
                doctor.UserId,
                operationRequest.OperationTypeId,
                operationRequest.Deadline,
                operationRequest.Priority
            );
        }

        private string GenerateUniqueOperationRequestId()
        {
            return "OP-" + Guid.NewGuid().ToString().Substring(0, 8).ToUpper();
        }
        public async Task<OperationRequestDto> UpdateAsync(string id, UpdateOperationRequestDto dto)
        {
            var operationRequest = await _repository.GetByIdAsync(id);
            if (operationRequest == null)
                throw new NotFoundException("Operation request not found.");

            // Update the operation request properties
            if (!string.IsNullOrEmpty(dto.OperationTypeId) && dto.OperationTypeId != operationRequest.OperationTypeId)
            {
                var newOperationType = await _operationTypeRepo.GetByIdAsync(new OperationTypeId(dto.OperationTypeId));
                if (newOperationType == null)
                    throw new NotFoundException("Operation type not found.");
                operationRequest.ChangeOperationType(dto.OperationTypeId);
            }

            if (dto.Deadline != default && dto.Deadline != operationRequest.Deadline)
                operationRequest.ChangeDeadline(dto.Deadline);

            if (!string.IsNullOrEmpty(dto.Priority) && dto.Priority != operationRequest.Priority)
                operationRequest.UpdatePriority(dto.Priority);

            await _repository.UpdateAsync(operationRequest);
            await _unitOfWork.CommitAsync();

            return new OperationRequestDto(
                operationRequest.Id,
                operationRequest.PatientId,
                operationRequest.DoctorId,
                operationRequest.OperationTypeId,
                operationRequest.Deadline,
                operationRequest.Priority
            );
        }



        public async Task<OperationRequestDto> GetByIdAsync(string id)
        {
            var operationRequest = await _repository.GetByIdAsync(id);
            if (operationRequest == null)
                return null;

            return new OperationRequestDto(
                operationRequest.Id,
                operationRequest.PatientId,
                operationRequest.DoctorId,
                operationRequest.OperationTypeId,
                operationRequest.Deadline,
                operationRequest.Priority
            );
        }





        public async Task<bool> DeleteOperationRequestAsync(string id, string doctorEmail)
        {
            var doctor = await _staffRepo.GetByEmailAsync(doctorEmail);
            if (doctor == null)
            {
                throw new NotFoundException("Doctor not found.");
            }

            var operationRequest = await _repository.GetByIdAsync(id);
            if (operationRequest == null)
            {
                throw new NotFoundException("Operation request not found.");
            }

            if (operationRequest.DoctorId != doctor.Id)
            {
                throw new BusinessRuleValidationException($"You can only delete operation requests that you created. Request Doctor ID: {doctor.Id}, Operation Request Doctor ID: {operationRequest.DoctorId}");
            }

            if (operationRequest.IsScheduled)
            {
                throw new BusinessRuleValidationException("Cannot delete a scheduled operation request.");
            }

            await _repository.RemoveAsync(operationRequest);
            await _unitOfWork.CommitAsync();

            await _loggingService.LogChangeAsync("Operation request deleted", doctor.Id, $"Operation request {id} deleted", null);
            return true;
        }


        public async Task<IEnumerable<OperationRequestDto>> SearchOperationRequestsAsync(SearchOperationRequestDto searchDto)
        {
            try
            {
                if (searchDto == null)
                {
                    throw new ArgumentNullException(nameof(searchDto), "Search criteria cannot be null.");
                }

                var requests = await _repository.GetFilteredOperationRequestsAsync(searchDto);

                if (requests == null || !requests.Any())
                {
                    _loggingService.LogInformation("No operation requests found matching the search criteria.");
                    return Enumerable.Empty<OperationRequestDto>();
                }

                return requests.Select(r => new OperationRequestDto(
                    r.Id,
                    r.PatientId,
                    r.DoctorId,
                    r.OperationTypeId,
                    r.Deadline,
                    r.Priority
                )).ToList();
            }
            catch (ArgumentNullException ex)
            {
                _loggingService.LogError($"Invalid search criteria: {ex.Message}");
                throw;
            }
            catch (ArgumentException ex)
            {
                _loggingService.LogError($"Invalid search parameters: {ex.Message}");
                throw;
            }
            catch (Exception ex)
            {
                _loggingService.LogError($"An error occurred while searching for operation requests: {ex.Message}");
                throw new ApplicationException("An error occurred while processing your search request.", ex);
            }
        }



    }
}
