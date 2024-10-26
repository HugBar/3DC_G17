# User Story 5.1.10: Delete Patient Profile

## Description
As an Admin, I want to delete a patient profile, so that I can remove patients who are no longer under care while maintaining compliance with data protection regulations.

## Acceptance Criteria
1. Admins can search for a patient profile and mark it for deletion.
2. Before deletion, the system prompts the admin to confirm the action.
3. Once deleted, all patient data is permanently removed from the system within a predefined time frame.
4. The system logs the deletion for audit and GDPR compliance purposes.

## Implementation Details

### Controller: PatientController
- Located in `Controllers/PatientController.cs`
- Handles HTTP requests for patient deletion
- Endpoints:
  - DELETE `/api/patient/delete-patient/{id}`: Deletes patient profile
  - Requires admin role authorization
  - Requires confirmation in request body

### Service: PatientService
- Located in `Domain/Patient/PatientService.cs`
- Implements business logic for patient deletion
- Key methods:
  - `DeletePatientAsync`: Processes patient deletion
  - `GetByUserIdAsync`: Retrieves patient for verification
  - Manages deletion confirmation

### Data Transfer Objects (DTOs)
- `DeletePatientDto`: Contains deletion confirmation flag
- `DeleteConfirmationDto`: Used for deletion process

### Security Features
- JWT token-based authentication
- Admin role verification
- Deletion confirmation requirement
- GDPR compliance measures

### Email Service
- Interface: `Domain/Shared/IEmailService.cs`
- Implementation: `Infrastructure/SmtpEmailService.cs`
- Handles:
  - Deletion notifications
  - Confirmation communications
  - Admin alerts

### Logging Service
- Interface: `Domain/Shared/ILoggingService.cs`
- Implementation: `Infrastructure/LoggingService.cs`
- Logs:
  - Deletion requests
  - Confirmation actions
  - Completed deletions
  - GDPR compliance data

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.10/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Admin initiates patient deletion
2. System validates admin privileges
3. Confirmation request and validation
4. Data deletion process
5. Audit logging completion

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.10/02.design/puml/class_diagram.puml`) shows relationships between:
- PatientController
- PatientService
- IPatientRepository
- ILoggingService
- DeletePatientDto
- DeleteConfirmationDto

## Testing
- Unit tests in `Tests/Unit/Controllers/PatientControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/PatientServiceTests.cs`
- Tests cover:
  - Admin authorization
  - Deletion confirmation
  - Data removal process
  - Audit logging

## Future Improvements
1. Data Management

   - Configurable retention periods
   - Backup creation before deletion
   - Enhanced audit tracking
   - Data anonymization options
   - Better feedback mechanisms
