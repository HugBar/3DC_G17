# User Story 5.1.1: Register New Backoffice Users

## Description
As an Admin, I want to register new backoffice users (e.g., doctors, nurses, technicians, admins) via an out-of-band process, so that they can access the backoffice system with appropriate permissions.

## Acceptance Criteria
1. Backoffice users (e.g., doctors, nurses, technicians) are registered by an Admin via an internal process, not via self-registration.
2. Admin assigns roles (e.g., Doctor, Nurse, Technician) during the registration process.
3. The system enforces strong password requirements for security.
4. A confirmation email is sent to verify the user's registration.

## Implementation Details

### Controller: UserController
- Located in `Controllers/UserController.cs`
- Handles HTTP requests for user registration and management
- Endpoints:
  - POST `/api/User/register`: Registers a new user
  - GET `/api/User/{id}`: Retrieves a specific user
  - GET `/api/User`: Retrieves all users
  - PUT `/api/User/{id}`: Updates a user
  - DELETE `/api/User/{id}`: Deletes a user

### Service: UserService
- Located in `Domain/User/UserService.cs`
- Implements business logic for user management
- Key methods:
  - `RegisterUserAsync`: Registers a new user
  - `GetUserByIdAsync`: Retrieves a user by ID
  - `GetAllUsersAsync`: Retrieves all users
  - `UpdateUserAsync`: Updates a user's information
  - `DeleteUserAsync`: Deletes a user

### Data Transfer Objects (DTOs)
- `CreateUserDto`: Used for user registration
- `UserDto`: Represents user data returned to the client
- `UpdateUserDto`: Used for updating user information

### Entity: ApplicationUser
- Located in `Domain/ApplicationUser/ApplicationUser.cs`
- Extends IdentityUser to represent a user in the system

### Database
- Uses Entity Framework Core with SQL Server
- User data is stored in the AspNetUsers table (provided by ASP.NET Core Identity)

### Authentication & Authorization
- JWT (JSON Web Token) authentication is implemented
- Role-based authorization is used to restrict access to admin-only endpoints

### Email Service
- Confirmation emails are sent using `IEmailService`
- Implementation details can be found in `Infrastructure/SmtpEmailService.cs`

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.1/SD.puml`) illustrates the flow of the user registration process, including:
1. Admin initiates registration
2. UserController receives the request
3. UserService processes the registration
4. RoleManager checks role validity
5. UserManager creates the user and assigns roles
6. EmailService sends a confirmation email

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.1/CD.puml`) shows the relationships between:
- UserController
- UserService
- ApplicationUser
- CreateUserDto
- UserDto

## Testing
- Unit tests are located in `Tests/Unit/Controllers/UserControllerTests.cs` and `Tests/Unit/Domain/UserServiceTests.cs`
- Integration tests are in `Tests/Integration/UserRegistrationIntegrationTests.cs`
- E2E tests are in `Tests/E2E/UserRegistrationE2ETests.cs`

## Future Improvements
- Implement a one-time setup link for new users to set their passwords
- Enhance error handling and user feedback
