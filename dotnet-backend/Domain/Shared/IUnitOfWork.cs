using System.Threading.Tasks;

namespace DDDSample1.Domain.Shared
{
    public interface IUnitOfWork
    {
        Task<int> CommitAsync();
        Task BeginTransactionAsync();
        Task CommitTransactionAsync();
        Task RollbackTransactionAsync();
    }
}