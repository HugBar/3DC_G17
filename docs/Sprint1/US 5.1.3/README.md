# User Story 5.1.3: Patient Registration

## Description
As a Patient, I want to register for the healthcare application, so that I can create a user profile and book appointments online.

## Acceptance Criteria
1. Patients can self-register using the external IAM system
2. During registration, patients provide personal details (e.g., name, email, phone) and create a profile.
3. The system validates the email address by sending a verification email with a confirmation link.
4. Patients cannot list their appointments without completing the registration process.

## Implementation Details

### Controller: PatientController
- Located in `Controllers/PatientController.cs`
- Handles HTTP requests for patient registration
- Endpoints:
  - POST `/api/user/register`: Creates new patient profile

### Service: PatientService
- Located in `Domain/Patient/PatientService.cs`
- Implements business logic for patient registration
- Key methods:
  - `RegisterPatientAsync`: Processes new patient registration
  - `GenerateMRN`: Creates unique Medical Record Number
  - `ValidatePatientData`: Ensures data integrity

### Data Transfer Objects (DTOs)
- `RegisterPatientDto`: Contains patient registration data
- `PatientDto`: Used for patient profile responses

### Security Features
- Password encryption
- Email verification system
- Data validation rules
- HIPAA compliance measures

### Email Service
- Interface: `Domain/Shared/IEmailService.cs`
- Implementation: `Infrastructure/SmtpEmailService.cs`
- Handles:
  - Verification emails
  - Welcome messages
  - Registration confirmations

### Logging Service
- Interface: `Domain/Shared/ILoggingService.cs`
- Implementation: `Infrastructure/LoggingService.cs`
- Logs:
  - Registration attempts
  - Email verifications
  - Profile creation events

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.3/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Patient submits registration form
2. System validates input data
3. MRN generation process
4. Email verification flow
5. Profile creation completion

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.3/02.design/puml/class_diagram.puml`) shows relationships between:
- PatientController
- PatientService
- IPatientRepository
- IEmailService
- RegisterPatientDto
- PatientDto

## Testing
- Unit tests in `Tests/Unit/Controllers/PatientControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/PatientServiceTests.cs`
- Tests cover:
  - Input validation
  - MRN generation
  - Email verification
  - Profile creation

## Future Improvements
1. Registration Process

   - Social media registration options
   - Multi-language support
   - Enhanced data validation rules
   - Streamlined verification process
   - Progressive profile completion
