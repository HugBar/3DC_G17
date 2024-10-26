using System;
using Microsoft.AspNetCore.Mvc.ViewFeatures;

public class PatientDto
{
    public string Id { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public string Email { get; set; } 
    public string PhoneNumber { get; set; }
    public string DateofBirth {get; set;}
    public string Gender {get; set;}
    public string ContactInfo {get; set;}
    public string EmergencyContact {get; set;}
    public string AppointmentHistory {get; set;}
    public string MedicalHistory { get; set; }
    public string MedicalNr {get; set;}



public PatientDto(string medicalNr, string id, string firstName, string lastName, string email, string dateOfBirth, string gender, string contactInfo, string emergencyContact,string phoneNumber, string appointmentHistory, string medicalHistory)
{
    if (string.IsNullOrWhiteSpace(id)) throw new ArgumentException("Id cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(firstName)) throw new ArgumentException("FirstName cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(lastName)) throw new ArgumentException("LastName cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(email)) throw new ArgumentException("Email cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(phoneNumber)) throw new ArgumentException("PhoneNumber cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(dateOfBirth)) throw new ArgumentException("Date of Birth cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(gender)) throw new ArgumentException("Gender cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(contactInfo)) throw new ArgumentException("ContactInfo cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(emergencyContact)) throw new ArgumentException("EmergencyContact cannot be null or empty.");
    if (string.IsNullOrWhiteSpace(medicalNr)) throw new ArgumentException("MedicalNr cannot be null or empty.");

    Id = id;
    FirstName = firstName;
    LastName = lastName;
    Email = email;
    PhoneNumber = phoneNumber;
    DateofBirth = dateOfBirth;
    Gender = gender;
    ContactInfo = contactInfo;
    EmergencyContact = emergencyContact;
    AppointmentHistory = appointmentHistory;
    MedicalHistory = medicalHistory;
    MedicalNr = medicalNr;
}
public PatientDto(){
    
}

}
