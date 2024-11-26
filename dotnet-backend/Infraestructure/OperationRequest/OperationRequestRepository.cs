using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRequestData;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.OperationRequestData
{
    public class OperationRequestRepository : IOperationRequestRepository
    {
        private readonly DDDSample1DbContext _context;

        public OperationRequestRepository(DDDSample1DbContext context)
        {
            _context = context;
        }
        public async Task<OperationRequest> UpdateAsync(OperationRequest operationRequest)
        {
            _context.OperationRequests.Update(operationRequest);
            await _context.SaveChangesAsync();
            return operationRequest;
        }


        public async Task<OperationRequest> AddAsync(OperationRequest operationRequest)
        {
            var result = await _context.OperationRequests.AddAsync(operationRequest);
            await _context.SaveChangesAsync();
            return result.Entity;
        }

        public async Task<OperationRequest> GetByIdAsync(string id)
        {
            return await _context.OperationRequests.FindAsync(id);
        }

        public async Task RemoveAsync(OperationRequest operationRequest)
        {
            _context.OperationRequests.Remove(operationRequest);
            await _context.SaveChangesAsync();
        }

        public async Task<IEnumerable<OperationRequest>> GetFilteredOperationRequestsAsync(SearchOperationRequestDto filter)
        {
            var operationRequests = _context.OperationRequests.AsQueryable();

            if (!string.IsNullOrEmpty(filter.PatientId))
            {
                operationRequests = operationRequests.Where(or => or.PatientId.StartsWith(filter.PatientId));
            }

            if (!string.IsNullOrEmpty(filter.OperationTypeId))
            {
                operationRequests = operationRequests.Where(or => or.OperationTypeId.StartsWith(filter.OperationTypeId));
            }

            if (!string.IsNullOrEmpty(filter.Priority))
            {
                operationRequests = operationRequests.Where(or => or.Priority.StartsWith(filter.Priority));
            }

            return await operationRequests.ToListAsync();
        }





    }
}
