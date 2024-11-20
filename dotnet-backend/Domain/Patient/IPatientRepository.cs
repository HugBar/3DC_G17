using System.Threading.Tasks;
using DDDSample1.Domain.PatientData;
using System.Collections.Generic;

public interface IPatientRepository
{
    Task<Patient> GetByUserIdAsync(string userId);

    Task<Patient> AddAsync(Patient patient);

    Task<(List<Patient> Patients, int TotalCount)> GetFilteredPatientAsync(PatientFilterDTO filter, int pageNumber, int pageSize);

    Task<bool> ExistsAsync(string patientId);

    Task<Patient> RemoveAsync(Patient patient);

    Task<bool> IsEmailUniqueAsync(string email);

    Task<bool> IsPhoneNumberUniqueAsync(string phoneNumber);

    Task<Patient> UpdateAsync(Patient patient);

    Task<Patient> GetByEmailAsync(string email);

    Task<Patient> GetByIdAsync(string id);
    Task<Patient> AnonymizeAsync(Patient patient);

    Task<Patient> GetByMedicalRecordNumberAsync(string mrn);



}
