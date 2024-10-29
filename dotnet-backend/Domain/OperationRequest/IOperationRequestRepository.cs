using System.Threading.Tasks;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationRequestData
{
        public interface IOperationRequestRepository
        {
                Task<OperationRequest> AddAsync(OperationRequest operationRequest);
                Task<OperationRequest> GetByIdAsync(string id);
                Task<OperationRequest> UpdateAsync(OperationRequest operationRequest);
                Task RemoveAsync(OperationRequest operationRequest);
                Task<IEnumerable<OperationRequest>> GetFilteredOperationRequestsAsync(SearchOperationRequestDto filter);

        }
}
