# User Story 5.1.6: Backoffice User Login System

## Description
As a (non-authenticated) Backoffice User, I want to log in to the system using my credentials, so that I can access the backoffice features according to my assigned role.

## Dependencies

### User Story Dependencies
1. **US 5.1.1** - User Registration System
   - Required for user accounts to exist in the system
   - Provides basic user account structure

### System Dependencies
1. **Authentication System**
   - JWT token-based authentication (AuthService.cs)
   - Role-based access control
   - Session management
   - Account lockout handling

2. **Email System**
   - Admin notifications for account lockouts
   - Uses SmtpEmailService.cs

## Acceptance Criteria
- Backoffice users log in using their username and password.
- Role-based access control ensures that users only have access to features appropriate to their
role (e.g., doctors can manage appointments, admins can manage users and settings).

- After five failed login attempts, the user account is temporarily locked, and a notification is
sent to the admin.
- Login sessions expire after a period of inactivity to ensure security

## Implementation Details

### Controllers
- Located in `Controllers/AuthController.cs`
- Key endpoints:
  - POST `/api/auth/login`: Authenticates users

### Services
- AuthService for authentication logic:
  - LoginAsync: Handles user authentication
  - GenerateJwtToken: Creates security tokens
  - ResetPasswordAsync: Manages password resets

### Security Features
- JWT Authentication:

### Email Notifications
1. **SmtpEmailService handles:
    - Lockout notifications
    - Password reset emails
    - Security alerts
### Error Handling
1. **Comprehensive error handling for:
    - Invalid credentials
    - Account lockouts
    - Server errors
    - Token validation

## Sequence Diagram
![Sequence Diagram](docs/Sprint1/US%205.1.6/02.design/svg/sequence_diagram.svg)

## Class Diagram
![Class Diagram](docs/Sprint1/US%205.1.6/02.design/svg/class_diagram.svg)

## Testing
- Unit tests are located in `Tests/Unit/Controllers/AuthControllerTests.cs` and `Tests/Unit/Domain/AuthServiceTests.cs`
- E2E tests are located in `cypress/e2e/Auth.cy.js` 

## Future Improvements
  