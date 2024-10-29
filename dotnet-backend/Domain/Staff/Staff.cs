using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffData
{
    public class Staff
    {
        [Key]
        public string Id { get; private set; }
        public string UserId { get; private set; }
        public string FirstName { get; private set; }
        public string LastName { get; private set; }
        public string Email { get; private set; }
        public string PhoneNumber { get; private set; }
        public string Specialization { get; private set; }
        public string LicenseNumber { get; private set; }
        public bool Active { get; private set; }

        public List<AvailabilitySlot> AvailabilitySlots { get; private set; } = new List<AvailabilitySlot>();

        public Staff()
        {
        }

        public Staff(string userId, string firstName, string lastName, string email, string phoneNumber, string specialization, string licenseNumber)
        {
            Id = userId;
            UserId = userId;
            FirstName = firstName;
            LastName = lastName;
            Email = email;
            PhoneNumber = phoneNumber;
            Specialization = specialization;
            LicenseNumber = licenseNumber;
            Active = true;
        }

        public void AddAvailabilitySlot(DateTime startTime, DateTime endTime)
        {
            if (HasOverlappingSlot(startTime, endTime))
            {
                throw new BusinessRuleValidationException("The new slot overlaps with an existing slot.");
            }

            AvailabilitySlots.Add(new AvailabilitySlot(startTime, endTime));
        }

        private bool HasOverlappingSlot(DateTime startTime, DateTime endTime)
        {
            return AvailabilitySlots.Any(slot =>
                (startTime >= slot.StartTime && startTime < slot.EndTime) ||
                (endTime > slot.StartTime && endTime <= slot.EndTime) ||
                (startTime <= slot.StartTime && endTime >= slot.EndTime));
        }

        public void ClearAvailabilitySlots()
        {
            AvailabilitySlots.Clear();
        }

        public void UpdateContactInfo(string email, string phoneNumber)
        {
            if (string.IsNullOrWhiteSpace(email)) throw new ArgumentException("Email cannot be null or empty.");
            if (string.IsNullOrWhiteSpace(phoneNumber)) throw new ArgumentException("PhoneNumber cannot be null or empty.");

            Email = email;
            PhoneNumber = phoneNumber;
        }

        public void UpdateSpecialization(string specialization)
        {
            if (string.IsNullOrWhiteSpace(specialization))
                throw new ArgumentException("Specialization cannot be null or empty.");

            Specialization = specialization;
        }

        public void Deactivate()
        {
            if (!Active)
                throw new BusinessRuleValidationException("Staff is already inactive.");

            Active = false;
        }

        public void Reactivate()
        {
            if (Active)
                throw new BusinessRuleValidationException("Staff is already active.");

            Active = true;
        }

        public bool IsActive()
        {
            return Active;
        }
    }
}
