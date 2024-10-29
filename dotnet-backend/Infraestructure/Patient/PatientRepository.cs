using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.PatientData;
using static Microsoft.EntityFrameworkCore.DbLoggerCategory;

namespace DDDSample1.Infrastructure.PatientData
{
    public class PatientRepository : IPatientRepository
    {
        private readonly DDDSample1DbContext _context;

        public PatientRepository(DDDSample1DbContext context)
        {
            _context = context;
        }


        public async Task<Patient> AddAsync(Patient patient)
        {
            var result = await _context.Patients.AddAsync(patient);
            await _context.SaveChangesAsync();
            return result.Entity;
        }

        public async Task<Patient> GetByUserIdAsync(string userId)
        {
            return await _context.Patients.FirstOrDefaultAsync(s => s.UserId == userId);
        }
        public async Task<IEnumerable<Patient>> GetFilteredPatientAsync(PatientFilterDTO filter, int pageNumber, int pageSize)
        {

            var patients = _context.Patients.AsQueryable();

            if (filter.FirstName != null)
            {
                patients = patients.Where(p => p.FirstName == filter.FirstName);
            }

            if (filter.LastName != null)
            {
                patients = patients.Where(p => p.LastName == filter.LastName);
            }

            if (filter.Email != null)
            {
                patients = patients.Where(p => p.Email == filter.Email);
            }
                 if (filter.PhoneNumber != null)
            {
                patients = patients.Where(p => p.PhoneNumber == filter.PhoneNumber);
            }
            if (filter.Id != null)
            {
                patients = patients.Where(p => p.UserId == filter.Id);
            }
            if (filter.DateofBirth != null)
            {
                patients = patients.Where(p => p.DateofBirth == filter.DateofBirth);
            }
            if (filter.Gender != null)
            {
                patients = patients.Where(p => p.Gender == filter.Gender);
            }
            if (filter.ContactInfo != null)
            {
                patients = patients.Where(p => p.ContactInfo == filter.ContactInfo);
            }
            if (filter.EmergencyContact != null)
            {
                patients = patients.Where(p => p.EmergencyContact == filter.EmergencyContact);
            }
            if (filter.AppointmentHistory != null)
            {
                patients = patients.Where(p => p.AppointmentHistory == filter.AppointmentHistory);
            }
            if (filter.MedicalHistory != null)
            {
                patients = patients.Where(p => p.MedicalHistory == filter.MedicalHistory);
            }
            if (filter.MedicalNr != null)
            {
                patients = patients.Where(p => p.MedicalNr == filter.MedicalNr);
            }

            return await patients.Skip((pageNumber - 1) * pageSize).Take(pageSize).ToListAsync();
        }

        public async Task<bool> ExistsAsync(string patientId)
        {
            return await _context.Patients.AnyAsync(p => p.UserId == patientId);
        }
        public async Task<bool> IsEmailUniqueAsync(string email)
        {
            return !await _context.Patients.AnyAsync(p => p.Email == email);
        }
        public async Task<bool> IsPhoneNumberUniqueAsync(string phoneNumber)
        {
            return !await _context.Patients.AnyAsync(s => s.PhoneNumber == phoneNumber);
        }
        public async Task<Patient> RemoveAsync(Patient patient)
        {
             _context.Patients.Remove(patient);
             await _context.SaveChangesAsync();
            return patient;
        }	
        public async Task<Patient> UpdateAsync(Patient patient)
        {
            _context.Patients.Update(patient);
            await _context.SaveChangesAsync();
            return patient;
        }

        public async Task<Patient> GetByEmailAsync(string email)
        {
            return await _context.Patients.FirstOrDefaultAsync(p => p.Email == email);
        }

        public async Task<Patient> GetByIdAsync(string id)
        {   
        return await _context.Patients.FirstOrDefaultAsync(p => p.UserId == id);
        }
        public async Task<Patient> AnonymizeAsync(Patient patient)
        {
            _context.Patients.Update(patient);
            await _context.SaveChangesAsync();
            return patient;
        }


    }
}
