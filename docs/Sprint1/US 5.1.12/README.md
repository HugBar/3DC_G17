# User Story 5.1.12: Create New Staff Profile

## Description
As an Admin, I want to create a new staff profile, so that I can add them to the hospital's roster.

## Acceptance Criteria
1. Admins can input staff details such as first name, last name, contact information, and specialization.
2. A unique staff ID (License Number) is generated upon profile creation.
3. The system ensures that the staff's email and phone number are unique.
4. The profile is stored securely, and access is based on role-based permissions.

## Implementation Details

### Controller: StaffController
- Located in `Controllers/StaffController.cs`
- Handles HTTP requests for staff profile management
- Endpoints:
  - POST `/api/staff/create-staff-profile`: Creates a new staff profile
  - PATCH `/api/staff/edit-staff-profile`: Updates an existing staff profile
  - DELETE `/api/staff/delete-staff/{id}`: Deactivates a staff profile

### Service: StaffService
- Located in `Domain/Staff/StaffService.cs`
- Implements business logic for staff profile management
- Key methods:
  - `AddAsync`: Creates a new staff profile
  - `UpdateStaffAsync`: Updates a staff profile
  - `DeactivateAsync`: Deactivates a staff profile
  - `GetByIdAsync`: Retrieves a staff profile by ID

### Data Transfer Objects (DTOs)
- `CreateStaffDto`: Used for staff profile creation
- `StaffDto`: Represents staff data returned to the client
- `UpdateStaffDto`: Used for updating staff information

### Entity: Staff
- Located in `Domain/Staff/Staff.cs`
- Represents a staff member in the system

### Repository: IStaffRepository
- Located in `Domain/Staff/IStaffRepository.cs`
- Defines methods for staff data persistence

### Database
- Uses Entity Framework Core
- Staff data is configured in `Infrastructure/Staff/StaffEntityTypeConfiguration.cs`

### Authentication & Authorization
- JWT (JSON Web Token) authentication is implemented
- Role-based authorization restricts staff profile creation to Admin users

### Email Service
- Confirmation emails for profile creation are sent using `IEmailService`
- Implementation details can be found in `Infrastructure/SmtpEmailService.cs`

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.12/SD.puml`) illustrates the flow of the staff profile creation process, including:
1. Admin initiates staff profile creation
2. StaffController receives the request
3. StaffService processes the creation
4. UserManager checks for existing user
5. StaffRepository checks for unique email and phone
6. StaffService generates a unique license number
7. StaffRepository saves the new staff profile
8. EmailService sends a confirmation email

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.12/CD.puml`) shows the relationships between:
- StaffController
- StaffService
- Staff
- CreateStaffDto
- StaffDto
- IStaffRepository
- UserManager<ApplicationUser>
- IUnitOfWork
- IEmailService

## Testing
- Unit tests are located in `Tests/Unit/Controllers/StaffControllerTests.cs` and `Tests/Unit/Domain/StaffServiceTests.cs`
- Integration tests should be implemented in a new file `Tests/Integration/StaffCreationIntegrationTests.cs`
- E2E tests should be implemented in a new file `Tests/E2E/StaffCreationE2ETests.cs`

## Future Improvements
- Implement a more sophisticated method for generating unique license numbers
- Add functionality for bulk staff profile creation
- Enhance the staff profile with additional fields like certifications, specialties, etc.
- Implement a notification system to inform new staff members about their account creation
