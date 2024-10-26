# User Story 5.1.2: Password Reset for Backoffice Users

## Description
As a Backoffice User (Admin, Doctor, Nurse, Technician), I want to reset my password if I forget it, so that I can regain access to the system securely.

## Acceptance Criteria
1. Backoffice users can request a password reset by providing their email.
2. The system sends a password reset link via email.
3. The reset link expires after a predefined period (e.g., 24 hours) for security.
4. Users must provide a new password that meets the system's password complexity rules.

## Implementation Details

### Controller: AuthController
- Located in `Controllers/AuthController.cs`
- Handles HTTP requests for password reset
- Endpoints:
  - POST `/api/auth/reset-password`: Initiates password reset
  - POST `/api/auth/confirm-reset-password`: Confirms and processes reset

### Service: AuthService
- Located in `Domain/Auth/AuthService.cs`
- Implements business logic for password reset
- Key methods:
  - `ResetPasswordAsync`: Initiates password reset process
  - `ConfirmResetPasswordAsync`: Processes password reset confirmation
  - `ValidatePasswordComplexity`: Validates new password requirements

### Data Transfer Objects (DTOs)
- `ResetPasswordDto`: Used for reset request (contains email)
- `ConfirmResetPasswordDto`: Used for reset confirmation (contains token and new password)

### Entity: ApplicationUser
- Located in `Domain/ApplicationUser/ApplicationUser.cs`
- Extends IdentityUser for user management
- Handles password and security token storage

### Email Service
- Interface: `Domain/Shared/IEmailService.cs`
- Implementation: `Infrastructure/SmtpEmailService.cs`
- Handles sending reset link emails
- Manages email templates and formatting

### Security Features
- JWT token-based reset links
- Token expiration handling
- Password complexity validation
- Secure email delivery

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.2/02.design/puml/sequenceDiagram.puml`) illustrates:
1. User requests password reset
2. System validates email and user status
3. Reset token generation and email sending
4. User confirms reset with token
5. Password validation and update
6. Confirmation response

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.2/02.design/puml/class_diagram.puml`) shows relationships between:
- AuthController
- AuthService
- IEmailService
- ApplicationUser
- UserManager
- Data transfer objects

## Testing
- Unit tests in `Tests/Unit/Controllers/AuthControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/AuthServiceTests.cs`
- Tests cover:
  - Email validation
  - Token generation/validation
  - Password complexity rules
  - Error handling

## Future Improvements
- Enhance error handling and user feedback
