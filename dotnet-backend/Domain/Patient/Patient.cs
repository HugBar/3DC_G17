using System;
namespace DDDSample1.Domain.PatientData;
public class Patient
{
    public string UserId { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public string Email { get; set; }

    public string DateofBirth { get; set; }

    public string Gender { get; set; }

    public string ContactInfo { get; set; }

    public string EmergencyContact { get; set; }

    public string AppointmentHistory { get; set; }
    public string PhoneNumber { get; set; }
    public string MedicalHistory { get; set; }

    public string MedicalNr { get; set; }
    public bool IsAnonymized { get; private set; }
    public DateTime? AnonymizedAt { get; private set; }

    public Patient()
    {

    }

    public Patient(string medicalNr, string userId, string firstName, string lastName, string email, string dateOfBirth, string gender, string contactInfo, string emergencyContact, string phoneNumber, string appointmentHistory = null, string medicalHistory = null)
    {
        if (string.IsNullOrWhiteSpace(userId)) throw new ArgumentException("Id cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(firstName)) throw new ArgumentException("FirstName cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(lastName)) throw new ArgumentException("LastName cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(email)) throw new ArgumentException("Email cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(dateOfBirth)) throw new ArgumentException("Date of Birth cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(gender)) throw new ArgumentException("Gender cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(contactInfo)) throw new ArgumentException("Contact Information cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(emergencyContact)) throw new ArgumentException("Emergency Contact cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(phoneNumber)) throw new ArgumentException("Phone Number cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(medicalNr)) throw new ArgumentException("Medical Number cannot be null or empty.");

        MedicalNr = medicalNr;
        UserId = userId;
        FirstName = firstName;
        LastName = lastName;
        Email = email;
        DateofBirth = dateOfBirth;
        Gender = gender;
        ContactInfo = contactInfo;
        EmergencyContact = emergencyContact;
        PhoneNumber = phoneNumber;
        AppointmentHistory = appointmentHistory ?? "";
        MedicalHistory = medicalHistory ?? ""; // Default to empty string if not provided
    }

    public void UpdateContactInfo(string email, string phoneNumber)
    {
        if (string.IsNullOrWhiteSpace(email)) throw new ArgumentException("Email cannot be null or empty.");
        if (string.IsNullOrWhiteSpace(phoneNumber)) throw new ArgumentException("PhoneNumber cannot be null or empty.");

        Email = email ?? Email;
        PhoneNumber = phoneNumber ?? PhoneNumber;
    }
     public void Anonymize()
    {
        // Keep non-identifiable information
        Gender = Gender;
        DateofBirth = GetAgeRange(DateofBirth);
        MedicalHistory = HashMedicalHistory(MedicalHistory);
        AppointmentHistory = HashAppointmentHistory(AppointmentHistory);

        // Clear identifiable information
        FirstName = "ANONYMIZED";
        LastName = "ANONYMIZED";
        Email = $"anonymized_{Guid.NewGuid()}@deleted.com";
        ContactInfo = "ANONYMIZED";
        EmergencyContact = "ANONYMIZED";
        PhoneNumber = "ANONYMIZED";

        IsAnonymized = true;
        AnonymizedAt = DateTime.UtcNow;
    }

    private string GetAgeRange(string dateOfBirth)
    {
               if (DateTime.TryParse(dateOfBirth, out DateTime dob))
        {
            var age = DateTime.Today.Year - dob.Year;
            return $"{age / 10 * 10}-{age / 10 * 10 + 10}";
        }
        return "UNKNOWN";
    }

    private string HashMedicalHistory(string history)
    {
        if (string.IsNullOrEmpty(history)) return "";
        using (var sha256 = System.Security.Cryptography.SHA256.Create())
        {
            var hashedBytes = sha256.ComputeHash(System.Text.Encoding.UTF8.GetBytes(history));
            return Convert.ToBase64String(hashedBytes);
        }
    }

    private string HashAppointmentHistory(string history)
    {
        if (string.IsNullOrEmpty(history)) return "";
        using (var sha256 = System.Security.Cryptography.SHA256.Create())
        {
            var hashedBytes = sha256.ComputeHash(System.Text.Encoding.UTF8.GetBytes(history));
            return Convert.ToBase64String(hashedBytes);
        }
    }


}
