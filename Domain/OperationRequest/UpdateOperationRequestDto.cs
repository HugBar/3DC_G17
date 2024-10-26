using System;

namespace DDDSample1.Domain.OperationRequestData
{
    public class UpdateOperationRequestDto
    {
        public string OperationTypeId { get; set; }
        public DateTime Deadline { get; set; }
        public string Priority { get; set; }
    }
}
