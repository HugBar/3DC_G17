using System;
using System.Threading.Tasks;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.OperationTypeData
{
    public class OperationTypeRepository : BaseRepository<OperationType, OperationTypeId>, IOperationTypeRepository
    {
        private readonly DDDSample1DbContext _context;

        public OperationTypeRepository(DDDSample1DbContext context) : base(context.OperationTypes)
        {
            _context = context;
        }


        public async Task<OperationType> addAsync(OperationType operationType)
        {
            var result = await _context.OperationTypes.AddAsync(operationType);
            await _context.SaveChangesAsync();
            return result.Entity;

        }

        public async Task<OperationType> GetByNameAsync(string name)
        {
            return await _context.OperationTypes.SingleOrDefaultAsync(o => o.Name == name);
        }

        public async Task<bool> ExistsByNameAsync(string name)
        {
            return await _context.OperationTypes.AnyAsync(o => o.Name == name);
        }

        public async Task<OperationType> UpdateAsync(OperationType operationType)
        {
            var result = _context.OperationTypes.Update(operationType);
            await _context.SaveChangesAsync();
            return result.Entity;
        }

        public new Task<OperationType> GetByIdAsync(OperationTypeId id)
        {
            return _context.OperationTypes.SingleOrDefaultAsync(o => o.Id == id);
        }

        public Task<List<OperationType>> GetByIdsAsync(List<Guid> ids)
        {
            throw new NotImplementedException();
        }

        public Task<OperationType> GetByIdAsync(Guid id)
        {
            throw new NotImplementedException();
        }

        public async Task<Dictionary<string, int>> GetRequiredStaffBySpecializationAsync(OperationTypeId id)
        {
            var operationType = await _context.OperationTypes
                .AsNoTracking()
                .SingleOrDefaultAsync(o => o.Id == id);

            if (operationType == null)
            {
                return null;
            }

            return operationType.RequiredStaffBySpecialization;
        }
    }
}