using System;
using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using System.Linq;
using Microsoft.AspNetCore.JsonPatch;
using Microsoft.EntityFrameworkCore.Metadata.Internal;

namespace DDDSample1.Domain.OperationTypeData
{
    public class OperationTypeService
    {
        private readonly IOperationTypeRepository _repo;
        private readonly IUnitOfWork _unitOfWork;
        private readonly ILoggingService _loggingService;

        public OperationTypeService(IOperationTypeRepository repo, IUnitOfWork unitOfWork, ILoggingService loggingService)
        {
            _repo = repo;
            _unitOfWork = unitOfWork;
            _loggingService = loggingService;
        }

        public async Task<OperationTypeDto> AddAsync(CreateOperationTypeDto dto)
        {
            if (await _repo.ExistsByNameAsync(dto.Name))
                throw new BusinessRuleValidationException("An operation type with this name already exists.");

            var operationPhases = new OperationPhases(dto.AnesthesiaPreparation, dto.Surgery, dto.Cleaning);
           

            var operationType = new OperationType(dto.Name, dto.RequiredStaffBySpecialization, operationPhases);

            await _repo.AddAsync(operationType);
            await _unitOfWork.CommitAsync();

            _loggingService.LogInformation($"New operation type created: {operationType.Id}");

            return new OperationTypeDto(operationType.Id, operationType.Name, operationType.RequiredStaffBySpecialization, operationType.Duration);
        }

        public async Task<IEnumerable<OperationTypeDto>> GetAllAsync()
        {
            var operationTypes = await _repo.GetAllAsync();
            return operationTypes.Select(o => new OperationTypeDto(o.Id, o.Name, o.RequiredStaffBySpecialization, o.Duration));
        }

        public async Task<OperationTypeDto> UpdateOperationType(OperationTypeId id, JsonPatchDocument<UpdateOperationTypeDto> patchDoc)
        {
            var operationType = await _repo.GetByIdAsync(id);

            if (operationType == null)
                throw new BusinessRuleValidationException("Operation type not found.");

            var operationTypeDto = ConvertToDto(operationType);

            patchDoc.ApplyTo(operationTypeDto);

            if(operationTypeDto.Name != operationType.Name && await _repo.ExistsByNameAsync(operationTypeDto.Name))
                throw new BusinessRuleValidationException("An operation type with this name already exists.");
            
            var operationPhases = new OperationPhases(operationTypeDto.AnesthesiaPreparation, operationTypeDto.Surgery, operationTypeDto.Cleaning);

            operationType.SetName(operationTypeDto.Name);
            operationType.SetRequiredStaffBySpecialization(operationTypeDto.RequiredStaffBySpecialization);
            operationType.SetEstimatedDuration(operationPhases);
                

            await _repo.UpdateAsync(operationType);
            await _unitOfWork.CommitAsync();

            _loggingService.LogInformation($"Operation type updated: {operationType.Id}");

            return new OperationTypeDto(operationType.Id, operationType.Name, operationType.RequiredStaffBySpecialization, operationType.Duration);
        }

        private UpdateOperationTypeDto ConvertToDto(OperationType operationType)
        {
            return new UpdateOperationTypeDto
            {
                Name = operationType.Name,
                RequiredStaffBySpecialization = operationType.RequiredStaffBySpecialization,
                AnesthesiaPreparation = operationType.Duration.AnesthesiaPreparation,
                Surgery = operationType.Duration.Surgery,
                Cleaning = operationType.Duration.Cleaning
            };
        }
    
    }
    
}