# User Story 5.1.8: Create Patient Profile (Admin)

## Description
As an Admin, I want to create a new patient profile, so that I can register their personal details and medical history.

## Acceptance Criteria
1. Admins can input patient details such as first name, last name, date of birth, contact information, and medical history.
2. A unique patient ID (Medical Record Number) is generated upon profile creation.
3. The system validates that the patient’s email and phone number are unique.
4. The profile is stored securely in the system, and access is governed by role-based permissions.

## Implementation Details

### Controller: PatientController
- Located in `Controllers/PatientController.cs`
- Handles HTTP requests for patient creation
- Endpoints:
  - POST `/api/patient`: Creates new patient profile
  - Requires admin role authorization

### Service: PatientService
- Located in `Domain/Patient/PatientService.cs`
- Implements business logic for patient creation
- Key methods:
  - `CreatePatientAsync`: Processes new patient creation
  - `GenerateUniqueMRN`: Creates Medical Record Number
  - `ValidatePatientData`: Ensures data integrity

### Data Transfer Objects (DTOs)
- `CreatePatientDto`: Contains patient creation data
  - Personal information
  - Contact details
  - Medical history
  - Emergency contacts
- `PatientDto`: Returns created patient data

### Security Features
- JWT token-based authentication
- Admin role verification
- Input validation
- Data encryption
- HIPAA compliance measures

### Email Service
- Interface: `Domain/Shared/IEmailService.cs`
- Implementation: `Infrastructure/SmtpEmailService.cs`
- Handles:
  - Welcome emails
  - Verification messages
  - Profile confirmations

### Logging Service
- Interface: `Domain/Shared/ILoggingService.cs`
- Implementation: `Infrastructure/LoggingService.cs`
- Logs:
  - Profile creation
  - Admin actions
  - Validation events
  - Email notifications

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.8/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Admin initiates patient creation
2. System validates admin privileges
3. Data validation process
4. MRN generation
5. Profile creation completion

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.8/02.design/puml/class_diagram.puml`) shows relationships between:
- PatientController
- PatientService
- IPatientRepository
- IEmailService
- CreatePatientDto
- PatientDto

## Testing
- Unit tests in `Tests/Unit/Controllers/PatientControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/PatientServiceTests.cs`
- Tests cover:
  - Admin authorization
  - Data validation
  - MRN generation
  - Profile creation
  - Email notifications

## Future Improvements
1. Profile Creation
   - Bulk patient import
   - Document upload support
   - Template-based creation
   - Auto-fill capabilities
   - Progressive data entry

2. User Experience
   - Streamlined input forms
   - Real-time validation
   - Quick duplicate checking
   - Smart data suggestions
   - Mobile-friendly interface

3. Integration
   - External system connectivity
   - Medical device data import
   - Insurance verification
   - Automated record requests
   - Healthcare provider linking
