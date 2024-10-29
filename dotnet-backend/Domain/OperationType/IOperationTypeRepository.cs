using System;
using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationTypeData
{
    public interface IOperationTypeRepository : IRepository<OperationType, Guid>
    {

        Task<OperationType> addAsync(OperationType operationType);

        Task<OperationType> GetByNameAsync(string name);

        Task<bool> ExistsByNameAsync(string name);

        Task<OperationType> UpdateAsync(OperationType operationType);

        Task<OperationType> GetByIdAsync(OperationTypeId id);

        Task<Dictionary<string, int>> GetRequiredStaffBySpecializationAsync(OperationTypeId id);

    }
}