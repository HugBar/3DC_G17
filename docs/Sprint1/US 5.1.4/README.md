# User Story 5.1.4: Update Patient Profile

## Description
As a Patient, I want to update my user profile, so that I can change my personal details and preferences while maintaining security and data integrity.

## Acceptance Criteria
1. Patients can log in and update their profile details (e.g., name, contact information, preferences).
2. Changes to sensitive data, such as email, trigger an additional verification step (e.g., confirmation email).
3. All profile updates are securely stored in the system.
4. The system logs all changes made to the patient's profile for audit purposes.

## Implementation Details

### Controller: PatientController
- Located in `Controllers/PatientController.cs`
- Handles HTTP requests for profile updates
- Endpoints:
  - PATCH `/api/patient/edit-patient-profile`: Updates patient profile
  - GET `/api/patient/get-patient-profile/{id}`: Retrieves patient profile

### Service: PatientService
- Located in `Domain/Patient/PatientService.cs`
- Implements business logic for profile updates
- Key methods:
  - `UpdatePatientProfileAsync`: Updates patient information
  - `GetByEmailAsync`: Retrieves patient by email
  - `SendContactInfoUpdateEmail`: Sends confirmation emails
  - `IsEmailUniqueAsync`: Validates email uniqueness

### Data Transfer Objects (DTOs)
- `UpdatePatientDto`: Contains updateable profile fields
- `PatientDto`: Represents patient data returned to client

### Security Features
- JWT token-based authentication
- Email verification for sensitive changes
- Role-based access control
- Audit logging for changes

### Email Service
- Interface: `Domain/Shared/IEmailService.cs`
- Implementation: `Infrastructure/SmtpEmailService.cs`
- Handles:
  - Update confirmation emails
  - Security notifications
  - Contact information change verification

### Logging Service
- Interface: `Domain/Shared/ILoggingService.cs`
- Implementation: `Infrastructure/LoggingService.cs`
- Logs:
  - Profile updates
  - Verification attempts
  - Security events

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.4/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Patient initiates profile update
2. System validates update request
3. Sensitive data changes trigger verification
4. Updates are persisted
5. Confirmation notifications sent

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.4/02.design/puml/class_diagram.puml`) shows relationships between:
- PatientController
- PatientService
- IPatientRepository
- IEmailService
- ILoggingService
- Data transfer objects

## Testing
- Unit tests in `Tests/Unit/Controllers/PatientControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/PatientServiceTests.cs`
- Tests cover:
  - Profile update validation
  - Email verification process
  - Security checks
  - Audit logging

## Future Improvements

   - Profile completion progress tracking
   - Multi-factor authentication for sensitive updates
   - Enhanced audit trail features
  
