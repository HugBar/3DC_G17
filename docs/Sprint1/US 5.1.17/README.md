# User Story 5.1.17: Update Operation Requisition

## Description
As a Doctor, I want to update an operation requisition, so that the Patient has access to the necessary healthcare.

## Acceptance Criteria
1. Doctors can update operation requests they created (e.g., change the deadline or priority).
2. The system checks that only the requesting doctor can update the operation request.
3. The system logs all updates to the operation request (e.g., changes to priority or deadline).
4. Updated requests are reflected immediately in the system and notify the Planning Module of any changes.

## Implementation Details

### Controller: OperationRequestController
- Located in `Controllers/OperationRequestController.cs`
- Handles HTTP requests for operation requisition updates
- Endpoints:
  - PATCH `/api/OperationRequest/{id}`: Updates operation requisition
  - Requires doctor role authorization

### Service: OperationRequestService
- Located in `Domain/OperationRequest/OperationRequestService.cs`
- Implements business logic for requisition updates
- Key methods:
  - `UpdateOperationRequestAsync`: Processes requisition updates
  - `ValidateOperationRequest`: Ensures medical validity
  - `NotifyPatient`: Handles patient notifications

### Data Transfer Objects (DTOs)
- `UpdateOperationRequestDto`: Contains update information
  - Operation details
  - Medical requirements
  - Schedule changes
- `OperationRequestDto`: Returns updated requisition data

### Security Features
- JWT token-based authentication
- Doctor role verification
- Input validation
- Medical data protection
- HIPAA compliance measures

### Email Service
- Interface: `Domain/Shared/IEmailService.cs`
- Implementation: `Infrastructure/SmtpEmailService.cs`
- Handles:
  - Update notifications
  - Patient communications
  - Medical staff alerts

### Logging Service
- Interface: `Domain/Shared/ILoggingService.cs`
- Implementation: `Infrastructure/LoggingService.cs`
- Logs:
  - Requisition updates
  - Doctor actions
  - Patient notifications
  - Medical changes

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.17/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Doctor initiates requisition update
2. System validates doctor privileges
3. Update process validation
4. Patient notification handling
5. Update confirmation

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.17/02.design/puml/class_diagram.puml`) shows relationships between:
- OperationRequestController
- OperationRequestService
- IOperationRequestRepository
- IEmailService
- ILoggingService
- UpdateOperationRequestDto
- OperationRequestDto

## Testing
- Unit tests in `Tests/Unit/Controllers/OperationRequestControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/OperationRequestServiceTests.cs`
- Tests cover:
  - Doctor authorization
  - Update validation
  - Medical data integrity
  - Notification process
  - Audit logging

## Future Improvements
1. Operation Management
   - Real-time status updates
   - Medical history integration
   - Automated scheduling
   - Resource allocation
   - Priority management

2. User Experience
   - Medical template library
   - Quick update options
   - Digital signature support
   - Mobile accessibility
   - Collaborative editing

3. Medical Integration
   - Equipment requirement tracking
   - Staff availability checking
   - Medical protocol compliance
   - Insurance verification
   - Emergency prioritization
