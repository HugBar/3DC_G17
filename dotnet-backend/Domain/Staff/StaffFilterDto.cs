
using System.Collections.Generic;
using DDDSample1.Domain.StaffData;

public class StaffFilterDto
{
    public string Id { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public string Email { get; set; }
    public string PhoneNumber { get; set; }
    public string Specialization { get; set; }
    public string LicenseNumber { get; set; }
    public bool Active { get; set; }
    public List<AvailabilitySlot> AvailabilitySlots { get; set; }

    public StaffFilterDto(string id, string firstName, string lastName, string email, string phoneNumber, string specialization, string licenseNumber, bool active, List<AvailabilitySlot> availabilitySlots)
    {
        Id = id;
        FirstName = firstName;
        LastName = lastName;
        Email = email;
        PhoneNumber = phoneNumber;
        Specialization = specialization;
        LicenseNumber = licenseNumber;
        Active = active;
        AvailabilitySlots = availabilitySlots;
    }

    public StaffFilterDto()
    {
        
    }
}

