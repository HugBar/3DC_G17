using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.PatientData;
using static Microsoft.EntityFrameworkCore.DbLoggerCategory;
using System;

namespace DDDSample1.Infrastructure.PatientData
{
    public class PatientRepository : IPatientRepository
    {
        private readonly DDDSample1DbContext _context;

        public PatientRepository(DDDSample1DbContext context)
        {
            _context = context;
        }

        public async Task<(List<Patient> Patients, int TotalCount)> GetFilteredPatientAsync(PatientFilterDTO filter, int pageNumber, int pageSize)
        {
            var query = _context.Patients.Where(p => p.IsAnonymized == false).AsQueryable();

            if (filter.FirstName != null)
            {
                query = query.Where(p => p.FirstName.StartsWith(filter.FirstName));
            }

            if (filter.LastName != null)
            {
                query = query.Where(p => p.LastName.StartsWith(filter.LastName));
            }

            if (filter.Email != null)
            {
                query = query.Where(p => p.Email.StartsWith(filter.Email));
            }
            if (filter.PhoneNumber != null)
            {
                query = query.Where(p => p.PhoneNumber.StartsWith(filter.PhoneNumber));
            }
            if (filter.DateofBirth != null)
            {
                query = query.Where(p => p.DateofBirth == filter.DateofBirth);
            }
            if (filter.Gender != null)
            {
                query = query.Where(p => p.Gender == filter.Gender);
            }
            if (filter.ContactInfo != null)
            {
                query = query.Where(p => p.ContactInfo == filter.ContactInfo);
            }
            if (filter.MedicalNr != null)
            {
                query = query.Where(p => p.MedicalNr.StartsWith(filter.MedicalNr));
            }

            var totalCount = await query.CountAsync();
            Console.WriteLine("Total count: -----------------------------------------------------------------------------------------------------------------------------" + totalCount);
            var patients = await query
                .Skip((pageNumber - 1) * pageSize)
                .Take(pageSize)
                .ToListAsync();

            return (patients, totalCount);
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

        public async Task<Patient> GetByMedicalRecordNumberAsync(string mrn)
        {
            return await _context.Patients
                .FirstOrDefaultAsync(p => p.MedicalNr == mrn);
        }


    }
}
