# User Story 5.1.7: Patient Login with External IAM

## Description
As a Patient, I want to log in to the healthcare system using my external IAM credentials, so that I can access my appointments, medical records, and other features securely.

## Acceptance Criteria
1. Patients log in via an external Identity and Access Management (IAM) provider (e.g., Google, Facebook, or hospital SSO).
2. After successful authentication via the IAM, patients are redirected to the healthcare system with a valid session. 
3. Patients have access to their appointment history, medical records, and other features relevant to their profile.
4. Sessions expire after a defined period of inactivity, requiring reauthentication.

## Implementation Details

### Controller: AuthController
- Located in `Controllers/AuthController.cs`
- Handles HTTP requests for authentication
- Endpoints:
  - POST `/api/auth/login`: Processes login requests
  - POST `/api/auth/refresh-token`: Handles token refresh

### Service: AuthService
- Located in `Domain/Auth/AuthService.cs`
- Implements business logic for authentication
- Key methods:
  - `LoginAsync`: Processes login attempts
  - `ValidateCredentials`: Verifies IAM credentials
  - `GenerateToken`: Creates JWT tokens

### Data Transfer Objects (DTOs)
- `LoginDto`: Contains login credentials
- `AuthResponseDto`: Returns authentication result

### Security Features
- JWT token authentication
- OAuth 2.0 integration
- Secure session management
- Rate limiting
- HTTPS enforcement

### External IAM Integration
- Interface: `Domain/Auth/IAuthService.cs`
- Implementation: Handles:
  - Credential validation
  - Token management
  - User synchronization
  - Session handling

### Logging Service
- Interface: `Domain/Shared/ILoggingService.cs`
- Implementation: `Infrastructure/LoggingService.cs`
- Logs:
  - Login attempts
  - Authentication failures
  - Session events
  - Security incidents

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.7/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Detailed IAM authentication flow
2. Service interactions
3. External IAM validation
4. Token generation process
5. Error handling paths

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.7/02.design/puml/class_diagram.puml`) shows relationships between:
- AuthController
- AuthService
- IAuthService
- IAMLoginDto
- AuthenticationResult
- ApplicationUser
- ILoggingService

## Testing
- Unit tests in `Tests/Unit/Controllers/AuthControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/AuthServiceTests.cs`
- Tests cover:
  - Login validation
  - Token generation
  - Session management
  - Error handling

## Future Improvements
1. Authentication Enhancement

   - Multi-factor authentication
   - Biometric login options
   - Single Sign-On expansion
   - Enhanced security logging
   - Automated security alerts
