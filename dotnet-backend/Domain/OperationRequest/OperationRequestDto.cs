using System;

namespace DDDSample1.Domain.OperationRequestData
{
    public class OperationRequestDto
    {
        public string Id { get; set; }
        public string PatientId { get; set; }
        public string DoctorId { get; set; }
        public string OperationTypeId { get; set; }
        public DateTime Deadline { get; set; }
        public string Priority { get; set; }

        public OperationRequestDto(string id, string patientId, string doctorId, string operationTypeId, DateTime deadline, string priority)
        {
            Id = id;
            PatientId = patientId;
            DoctorId = doctorId;
            OperationTypeId = operationTypeId;
            Deadline = deadline;
            Priority = priority;
        }
    }
}
