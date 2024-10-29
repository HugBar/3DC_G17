# User Story 5.1.9: Edit Patient Profile (Admin)

## Description
As an Admin, I want to edit an existing patient profile, so that I can update their information when needed.

## Acceptance Criteria
1. Admins can search for and select a patient profile to edit.
2. Editable fields include name, contact information, medical history, and allergies.
3. Changes to sensitive data (e.g., contact information) trigger an email notification to the patient.
4. The system logs all profile changes for auditing purposes.

## Implementation Details

### Controller: PatientController
- Located in `Controllers/PatientController.cs`
- Handles HTTP requests for patient profile updates
- Endpoints:
  - PATCH `/api/patient/admin/edit-patient-profile/{id}`: Updates patient profile
  - Requires admin role authorization
  - Validates input data

### Service: PatientService
- Located in `Domain/Patient/PatientService.cs`
- Implements business logic for profile updates
- Key methods:
  - `UpdatePatientAsync`: Updates patient information
  - `ValidateUpdates`: Ensures data integrity
  - `NotifyChanges`: Handles update notifications

### Data Transfer Objects (DTOs)
- `UpdatePatientDto`: Contains updateable fields
  - Personal information
  - Contact details
  - Medical history
  - Emergency contacts
- `PatientDto`: Returns updated patient data

### Security Features
- JWT token-based authentication
- Admin role verification
- Input validation
- Audit logging
- HIPAA compliance measures

### Email Service
- Interface: `Domain/Shared/IEmailService.cs`
- Implementation: `Infrastructure/SmtpEmailService.cs`
- Handles:
  - Update notifications
  - Change confirmations
  - Admin alerts

### Logging Service
- Interface: `Domain/Shared/ILoggingService.cs`
- Implementation: `Infrastructure/LoggingService.cs`
- Logs:
  - Profile updates
  - Admin actions
  - Validation events
  - Change history

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.9/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Admin initiates profile update
2. System validates admin privileges
3. Update process validation
4. Change notification handling
5. Update confirmation

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.9/02.design/puml/class_diagram.puml`) shows relationships between:
- PatientController
- PatientService
- IPatientRepository
- IEmailService
- ILoggingService
- UpdatePatientDto
- PatientDto

## Testing
- Unit tests in `Tests/Unit/Controllers/PatientControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/PatientServiceTests.cs`
- Tests cover:
  - Admin authorization
  - Update validation
  - Data persistence
  - Notification sending
  - Audit logging

## Future Improvements
1. Profile Management
   - Change history viewer
   - Batch update capabilities
   - Field-level change tracking
   - Automated data verification
   - Version control

2. User Experience
   - Real-time validation
   - Change preview
   - Inline editing
   - Smart suggestions
   - Mobile responsiveness

3. Security
   - Enhanced audit trails
   - Change approval workflow
   - Field-level access control
   - Sensitive data protection
   - Compliance reporting
