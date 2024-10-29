using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationRequestData
{
    public class OperationRequest
    {
        private static readonly List<string> ValidPriorities = new List<string> { "elective", "urgent", "emergency" };
        public string Id { get; private set; }
        public string PatientId { get; private set; }
        public string DoctorId { get; private set; }
        public string OperationTypeId { get; private set; }
        public DateTime Deadline { get; private set; }
        public string Priority { get; private set; }

        public bool IsScheduled { get; private set; }



        private OperationRequest()
        {

        }

        public OperationRequest(string id, string patientId, string doctorId, string operationTypeId, DateTime deadline, string priority)
        {
            if (!ValidPriorities.Contains(priority.ToLower()))
            {
                throw new BusinessRuleValidationException("Invalid priority. Valid values are: elective, urgent, emergency.");
            }

            Id = id;
            PatientId = patientId;
            DoctorId = doctorId;
            OperationTypeId = operationTypeId;
            Deadline = deadline;
            Priority = priority.ToLower();
            IsScheduled = false;
        }

        public void ChangeDeadline(DateTime newDeadline)
        {
            if (newDeadline < DateTime.Now)
            {
                throw new BusinessRuleValidationException("The deadline cannot be in the past.");
            }

            Deadline = newDeadline;
        }

        public void UpdatePriority(string newPriority)
        {
            if (!ValidPriorities.Contains(newPriority.ToLower()))
            {
                throw new BusinessRuleValidationException("Invalid priority. Valid values are: elective, urgent, emergency.");
            }

            Priority = newPriority.ToLower();
        }

        public void MarkAsScheduled()
        {
            IsScheduled = true;
        }

        public void MarkAsUnscheduled()
        {
            IsScheduled = false;
        }
        public void ChangeOperationType(string newOperationTypeId)
        {
            if (string.IsNullOrEmpty(newOperationTypeId))
            {
                throw new BusinessRuleValidationException("Operation type ID cannot be null or empty.");
            }
            OperationTypeId = newOperationTypeId;
        }

    }

}
