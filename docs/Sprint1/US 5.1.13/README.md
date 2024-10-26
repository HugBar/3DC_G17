# User Story 5.1.13: Edit Staff Profile

## Description
As an Admin, I want to edit a staff's profile, so that I can update their information and maintain accurate records in the hospital's roster.

## Acceptance Criteria
1. Admins can search for and select a staff profile to edit.
2. Editable fields include contact information, availability slots, and specialization.
3. The system logs all profile changes, and any changes to contact information trigger a confirmation email to the staff member.
4. The edited data is updated in real-time across the system.

## Implementation Details

### Controller: StaffController
- Located in `Controllers/StaffController.cs`
- Handles HTTP requests for staff profile management
- Endpoints:
  - PATCH `/api/staff/edit-staff-profile/{id}`: Updates an existing staff profile

### Service: StaffService
- Located in `Domain/Staff/StaffService.cs`
- Implements business logic for staff profile management
- Key methods:
  - `UpdateStaffAsync`: Updates a staff profile
  - `GetByIdAsync`: Retrieves a staff profile by ID

### Data Transfer Objects (DTOs)
- `UpdateStaffDto`: Used for updating staff information
- `StaffDto`: Represents staff data returned to the client

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
- Role-based authorization restricts staff profile editing to Admin users

### Email Service
- Confirmation emails for profile updates are sent using `IEmailService`
- Implementation details can be found in `Infrastructure/SmtpEmailService.cs`

### Logging Service
- Profile changes are logged using `ILoggingService`
- Implementation details can be found in `Infrastructure/LoggingService.cs`

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.13/SD.puml`) illustrates the flow of the staff profile editing process, including:
1. Admin initiates staff profile update
2. StaffController receives the request
3. StaffService processes the update
4. StaffRepository checks for unique email and phone (if changed)
5. StaffRepository updates the staff profile
6. EmailService sends a confirmation email if contact info is changed
7. LoggingService logs the profile changes

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.13/CD.puml`) shows the relationships between:
- StaffController
- StaffService
- Staff
- UpdateStaffDto
- StaffDto
- IStaffRepository
- IUnitOfWork
- IEmailService
- ILoggingService

## Testing
- Unit tests are located in `Tests/Unit/Controllers/StaffControllerTests.cs` and `Tests/Unit/Domain/StaffServiceTests.cs`
- Integration tests should be implemented in a new file `Tests/Integration/StaffUpdateIntegrationTests.cs`
- E2E tests should be implemented in a new file `Tests/E2E/StaffUpdateE2ETests.cs`

## Future Improvements
- Implement a change history feature to track all modifications to a staff profile over time
- Add the ability to revert changes or view previous versions of a staff profile
- Implement a notification system to inform relevant departments about significant changes to staff profiles
