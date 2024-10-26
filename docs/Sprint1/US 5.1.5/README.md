# User Story 5.1.5: Patient Account Deletion and GDPR Compliance

## Description
As a Patient, I want to delete my account and all associated data, so that I can exercise my right to be forgotten as per GDPR regulations while allowing the system to retain anonymized data for research purposes.

## Acceptance Criteria
1. Patients can request to delete their account through the profile settings.
2. The system sends a confirmation email to the patient before proceeding with account deletion.
3. Upon confirmation, all personal data is permanently deleted from the system within the legally required time frame (e.g., 30 days).
4. Patients are notified once the deletion is complete, and the system logs the action for GDPR compliance.
5. Some anonymized data may be retained for legal or research purposes, but all identifiable information is erased.

## Implementation Details

### Controller: PatientController
- Located in `Controllers/PatientController.cs`
- Handles HTTP requests for account deletion
- Endpoints:
  - POST `/api/patient/account-deletion-request`: Initiates account deletion
  - DELETE `/api/patient/confirm-account-deletion`: Confirms and processes deletion

### Service: PatientService
- Located in `Domain/Patient/PatientService.cs`
- Implements business logic for account deletion
- Key methods:
  - `RequestAccountDeletionAsync`: Initiates deletion process
  - `ConfirmAccountDeletionAsync`: Processes confirmed deletion
  - `ValidateTokenAndGetEmail`: Validates deletion confirmation token
  - `GenerateToken`: Creates secure confirmation tokens

### Data Transfer Objects (DTOs)
- `DeleteConfirmationDto`: Used for deletion confirmation
- `DeletePatientDto`: Used for deletion requests

### Security & Privacy Features
- JWT token-based authentication
- Email confirmation for deletion requests
- Data anonymization process
- GDPR compliance logging

### Data Anonymization
- Patient identifiable data is removed
- Medical history is retained in anonymized form
- Research-relevant data is preserved without personal identifiers

### Email Service
- Confirmation emails are sent using `IEmailService`
- Implementation in `Infrastructure/SmtpEmailService.cs`
- Notifications sent for:
  - Deletion request confirmation
  - Deletion completion

### Logging Service
- GDPR compliance logging via `ILoggingService`
- Implementation in `Infrastructure/LoggingService.cs`
- Logs:
  - Deletion requests
  - Confirmation attempts
  - Successful deletions
  - Data anonymization

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.5/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Patient initiates account deletion
2. System generates confirmation token
3. Email service sends confirmation
4. Patient confirms deletion
5. System anonymizes data
6. Final confirmation sent

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.5/02.design/puml/class_diagram.puml`) shows relationships between:
- PatientController
- PatientService
- IEmailService
- ILoggingService
- IPatientRepository
- Data transfer objects

## Testing
- Unit tests in `Tests/Unit/Controllers/PatientControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/PatientServiceTests.cs`
- Integration tests for complete deletion flow
- Security testing for token validation

## Future Improvements
1. Enhanced Data Export
   - Allow patients to export their data before deletion
   - Provide data in standard formats (JSON, PDF)

2. Deletion Schedule Options
   - Immediate deletion with confirmation
   - Scheduled deletion with cancellation window
   - Automated deletion after inactivity

3. Advanced Anonymization
   - Configurable anonymization rules
   - Enhanced data retention policies
   - Research data categorization

