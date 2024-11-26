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

            return new OperationTypeDto(operationType);
        }

        public async Task<IEnumerable<OperationTypeDto>> GetAllAsync()
        {
            var operationTypes = await _repo.GetAllAsync();
            return operationTypes.Select(o => new OperationTypeDto(o));
        }

        public async Task<OperationTypeDto> UpdateOperationType(OperationTypeId id, JsonPatchDocument<UpdateOperationTypeDto> patchDoc)
        {
            var operationType = await _repo.GetByIdAsync(id);

            if (operationType == null)
                throw new BusinessRuleValidationException("Operation type not found.");

            if (!operationType.IsActive)
            throw new BusinessRuleValidationException("Cannot update an inactive version.");

            var operationTypeDto = ConvertToDto(operationType);

            patchDoc.ApplyTo(operationTypeDto);

            if(operationTypeDto.Name != operationType.Name && await _repo.ExistsByNameAsync(operationTypeDto.Name))
                throw new BusinessRuleValidationException("An operation type with this name already exists.");
            
            var operationPhases = new OperationPhases(operationTypeDto.AnesthesiaPreparation, operationTypeDto.Surgery, operationTypeDto.Cleaning);

            var newVersion = operationType.CreateNewVersion(operationTypeDto.RequiredStaffBySpecialization, operationPhases);

            await _repo.UpdateAsync(operationType);
            await _repo.AddAsync(newVersion);
            await _unitOfWork.CommitAsync();

            _loggingService.LogInformation($"Nova versão do tipo de operação criada: {newVersion.Id}, versão anterior: {operationType.Id}");

            return new OperationTypeDto(newVersion);
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

        public async Task<OperationTypeDto> DeactivateOperationType(Guid id)
        {
            var operationTypeId = new OperationTypeId(id);
            var operationType = await _repo.GetByIdAsync(operationTypeId);

            if (operationType == null)
                throw new BusinessRuleValidationException("Operation type not found.");
            
            if (!operationType.IsActive)
                throw new BusinessRuleValidationException("Operation type is already inactive.");

            operationType.Deactivate();

            await _repo.UpdateAsync(operationType);
            await _unitOfWork.CommitAsync();

            return new OperationTypeDto(operationType);
        }

        public async Task<List<OperationType>> SearchOperationType(OperationTypeFilterDto filterDto)
        {
            if (filterDto.PageNumber < 1 || filterDto.PageSize < 1)
                throw new BusinessRuleValidationException("Invalid page number or page size.");

            var operationTypes = await _repo.SearchOperationType(filterDto);
            
            return operationTypes;
        }
    
    }
    
}