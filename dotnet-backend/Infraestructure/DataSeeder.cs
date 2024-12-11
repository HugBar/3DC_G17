using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Identity;
using Microsoft.Extensions.DependencyInjection;
using DDDSample1.Domain.StaffData;
using DDDSample1.Domain.PatientData;
using DDDSample1.Domain.OperationTypeData;
using DDDSample1.Domain.OperationRequestData;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.Shared;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.UserData;
using DDDSample1.Domain.SurgeryRoomData;
using DDDSample1.Domain.SurgeryRoom;

namespace DDDSample1.Infrastructure
{
    public static class DataSeeder
    {
        public static async Task SeedData(IServiceProvider serviceProvider)
        {
            using (var scope = serviceProvider.CreateScope())
            {
                var context = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();
                var userManager = scope.ServiceProvider.GetRequiredService<UserManager<ApplicationUser>>();
                var roleManager = scope.ServiceProvider.GetRequiredService<RoleManager<IdentityRole>>();
                var medicalRecordService = scope.ServiceProvider.GetRequiredService<IMedicalRecordService>();


                await SeedUsers(userManager, roleManager);
                await SeedStaff(context, userManager);
                await SeedPatients(context, userManager, medicalRecordService);
                await SeedOperationTypes(context);
                await SeedOperationRequests(context, userManager);
                await SeedSurgeryRooms(context);

                await context.SaveChangesAsync();
            }
        }

        public static async Task SeedUsers(UserManager<ApplicationUser> userManager, RoleManager<IdentityRole> roleManager)
        {
            var usersToSeed = new[]
            {
                new { Email = "admin@admin.com", UserName = "admin", Password = "Admin123!", Role = "Admin" },
                new { Email = "nurse@nurse.com", UserName = "nurse", Password = "Nurse123!", Role = "Nurse" },
                new { Email = "doctor@doctor.com", UserName = "doctor", Password = "Doctor123!", Role = "Doctor" },
                new { Email = "technician@technician.com", UserName = "technician", Password = "Tech123!", Role = "Technician" },
                new { Email = "patient@patient.com", UserName = "patient", Password = "Patient123!", Role = "Patient" },
                new { Email = "patient2@patient.com", UserName = "patient2", Password = "Patient123!", Role = "Patient" },
                new { Email = "stafftest@stafftest.com", UserName = "newstafftest", Password = "Staff123!", Role = "Doctor" },
                new { Email = "patienttest@patienttest.com", UserName = "newspatienttest", Password = "Patient123!", Role = "Patient" }
            };

            try
            {
                // Ensure all roles exist
                var roles = new[] { "Admin", "Nurse", "Doctor", "Technician", "Patient" };
                foreach (var role in roles)
                {
                    if (!await roleManager.RoleExistsAsync(role))
                    {
                        var roleResult = await roleManager.CreateAsync(new IdentityRole(role));
                        if (!roleResult.Succeeded)
                        {
                            throw new Exception($"Failed to create {role} role");
                        }
                    }
                }

                // Create one user for each role
                foreach (var userInfo in usersToSeed)
                {
                    var user = await userManager.FindByEmailAsync(userInfo.Email);
                    if (user == null)
                    {
                        user = new ApplicationUser { UserName = userInfo.UserName, Email = userInfo.Email };
                        var createUserResult = await userManager.CreateAsync(user, userInfo.Password);

                        if (!createUserResult.Succeeded)
                        {
                            throw new Exception($"Failed to create {userInfo.Role} user: " + string.Join(", ", createUserResult.Errors.Select(e => e.Description)));
                        }

                        await userManager.AddToRoleAsync(user, userInfo.Role);
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"An error occurred while seeding the users: {ex.Message}");
                throw;
            }
        }

        private static async Task SeedStaff(DDDSample1DbContext context, UserManager<ApplicationUser> userManager)
        {
            var staffUsers = new[]
            {
        new { Email = "admin@admin.com", FirstName = "Admin", LastName = "Main", PhoneNumber = "123456789", Specialization = "Surgery", LicenseNumber = "LIC-12345678" },
        new { Email = "nurse@nurse.com", FirstName = "Nurse", LastName = "Main", PhoneNumber = "098765432", Specialization = "Cardiology", LicenseNumber = "LIC-78901278" },
        new { Email = "doctor@doctor.com", FirstName = "Doctor", LastName = "Main", PhoneNumber = "112233445", Specialization = "Neurology", LicenseNumber = "LIC-34567878" },
        new { Email = "technician@technician.com", FirstName = "Technician", LastName = "Main", PhoneNumber = "112233445", Specialization = "Neurology", LicenseNumber = "LIC-34567878" }

            };

            foreach (var staffUserInfo in staffUsers)
            {
                // Fetch the UserId from AspNetUsers by Email
                var user = await userManager.FindByEmailAsync(staffUserInfo.Email);
                if (user != null && !context.Staffs.Any(s => s.UserId == user.Id))
                {
                    // Create Staff where Id and UserId are the same
                    var staff = new Staff(user.Id, staffUserInfo.FirstName, staffUserInfo.LastName, staffUserInfo.Email, staffUserInfo.PhoneNumber, staffUserInfo.Specialization, staffUserInfo.LicenseNumber);
                    staff.AddAvailabilitySlot(DateTime.Now.AddDays(1), DateTime.Now.AddDays(1).AddHours(8));
                    await context.Staffs.AddAsync(staff);
                }
            }
        }


        private static async Task SeedPatients(DDDSample1DbContext context, UserManager<ApplicationUser> userManager, IMedicalRecordService medicalRecordService)
        {
            var patientUsers = new[]
            {
                new { Email = "patient@patient.com", FirstName = "Patient", LastName = "Main", DateOfBirth = "1990-01-01", Gender = "Female", ContactInfo = "123 Main St", EmergencyContact = "Emergency Contact 1", PhoneNumber = "1231231234", MedicalNr = "202412000001" },
                new { Email = "patient2@patient.com", FirstName = "Patient", LastName = "Second", DateOfBirth = "1985-05-15", Gender = "Male", ContactInfo = "456 Elm St", EmergencyContact = "Emergency Contact 2", PhoneNumber = "4564564567", MedicalNr = "202412000002" },
            };

            foreach (var patientUserInfo in patientUsers)
            {
                // Fetch the UserId from AspNetUsers by Email
                var user = await userManager.FindByEmailAsync(patientUserInfo.Email);
                if (user != null && !context.Patients.Any(p => p.UserId == user.Id))
                {
                    // Create Patient using the constructor you defined
                    var patient = new Patient(patientUserInfo.MedicalNr, user.Id, patientUserInfo.FirstName, patientUserInfo.LastName, patientUserInfo.Email, patientUserInfo.DateOfBirth, patientUserInfo.Gender, patientUserInfo.ContactInfo, patientUserInfo.EmergencyContact, patientUserInfo.PhoneNumber);
                    await context.Patients.AddAsync(patient);

                    // Create blank medical record for the seeded patient
                    await medicalRecordService.CreateBlankMedicalRecord(patient.MedicalNr);
                }
            }
        }


        private static async Task SeedOperationTypes(DDDSample1DbContext context)
        {
            if (!context.OperationTypes.Any())
            {
                var operationTypeList = new List<OperationType>
        {
            new OperationType("Heart Surgery",
                new Dictionary<string, int> { { "Surgery", 1 }, { "Cardiology", 1 } },
                new OperationPhases(TimeSpan.FromMinutes(30), TimeSpan.FromHours(4), TimeSpan.FromMinutes(30))
            ),
            new OperationType("Brain Surgery",
                new Dictionary<string, int> { { "Surgery", 1 }, { "Neurology", 1 } },
                new OperationPhases(TimeSpan.FromMinutes(45), TimeSpan.FromHours(6), TimeSpan.FromMinutes(45))
            ),
            new OperationType("Appendectomy",
                new Dictionary<string, int> { { "Surgery", 1 } },
                new OperationPhases(TimeSpan.FromMinutes(15), TimeSpan.FromHours(2), TimeSpan.FromMinutes(15))
            )
        };
                foreach (var operationType in operationTypeList)
                {
                    await context.OperationTypes.AddAsync(operationType);
                }

                await context.SaveChangesAsync();
            }
        }


        private static async Task SeedOperationRequests(DDDSample1DbContext context, UserManager<ApplicationUser> userManager)
        {
            if (!context.OperationRequests.Any())
            {
                var patients = await context.Patients.ToListAsync();

                var specificDoctorUser = await userManager.FindByEmailAsync("doctor@doctor.com");

                var operationTypes = await context.OperationTypes.ToListAsync();

                if (patients.Any() && operationTypes.Any())
                {
                    var operationRequestList = new List<OperationRequest>
            {
                new OperationRequest("OP-00000001", patients[0].UserId, specificDoctorUser.Id, operationTypes[0].Id.Value, DateTime.Now.AddDays(7), "urgent"),
                new OperationRequest("OP-00000002", patients[1].UserId, specificDoctorUser.Id, operationTypes[1].Id.Value, DateTime.Now.AddDays(14), "elective"),
            };

                    foreach (var operationRequest in operationRequestList)
                    {
                        await context.OperationRequests.AddAsync(operationRequest);
                    }

                    await context.SaveChangesAsync();
                }
            }
        }

        private static async Task SeedSurgeryRooms(DDDSample1DbContext context)
        {
            if (!context.SurgeryRooms.Any())
            {
                var surgeryRoomList = new List<SurgeryRoom>
                {
                    new SurgeryRoom(
                        "OR-101",
                        RoomType.OperatingRoom,
                        6
                    ),
                    new SurgeryRoom(
                        "OR-102",
                        RoomType.OperatingRoom,
                        4
                    ),
                    new SurgeryRoom(
                        "OR-103",
                        RoomType.ICU,
                        2
                    ),
                    new SurgeryRoom(
                        "OR-104",
                        RoomType.ConsultationRoom,
                        3
                    )
                };


                surgeryRoomList[0].SetStatus(RoomStatus.Occupied);
                surgeryRoomList[1].SetStatus(RoomStatus.Occupied);
                surgeryRoomList[2].SetStatus(RoomStatus.Occupied);
                surgeryRoomList[3].SetStatus(RoomStatus.Occupied);


                foreach (var surgeryRoom in surgeryRoomList)
                {
                    await context.SurgeryRooms.AddAsync(surgeryRoom);
                }

                await context.SaveChangesAsync();
            }
        }
    }
}
