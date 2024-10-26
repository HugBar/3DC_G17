# User Story 5.1.18: Remove an Operation Requisition

## Description
As a Doctor, I want to remove an operation requisition, so that the healthcare activities are provided as necessary.

## Acceptance Criteria
1. Doctors can delete operation requests they created if the operation has not yet been scheduled.
2. A confirmation prompt is displayed before deletion.
3. Once deleted, the operation request is removed from the patient's medical record and cannot be recovered.
4. The system logs the deletion action for audit purposes.

## Implementation Details

### Controller: OperationRequestController
- Located in `Controllers/OperationRequestController.cs`
- Handles HTTP requests for operation request management
- Endpoints:
  - DELETE `/api/operation-request/delete-operation-request/{id}`: Deletes an operation request

### Service: OperationRequestService
- Located in `Domain/OperationRequest/OperationRequestService.cs`
- Implements business logic for operation request management
- Key methods:
  - `DeleteOperationRequestAsync`: Deletes an operation request

### Data Transfer Objects (DTOs)
- `OperationRequestDto`: Represents operation request data returned to the client

### Entity: OperationRequest
- Located in `Domain/OperationRequest/OperationRequest.cs`
- Represents an operation request in the system

### Repository: IOperationRequestRepository
- Located in `Domain/OperationRequest/IOperationRequestRepository.cs`
- Defines methods for operation request data persistence

### Database
- Uses Entity Framework Core
- Operation request data is configured in `Infrastructure/OperationRequest/OperationRequestEntityTypeConfiguration.cs`

### Authentication & Authorization
- JWT (JSON Web Token) authentication is implemented
- Only doctors are authorized to delete their own operation requests

### Logging Service
- Deletion actions are logged using `ILoggingService`
- Implementation details can be found in `Infrastructure/LoggingService.cs`

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.18/SD.puml`) illustrates the flow of the operation request deletion process, including:
1. Doctor initiates deletion request
2. OperationRequestController receives the request
3. OperationRequestService processes the deletion
4. OperationRequestRepository removes the request from the database
5. LoggingService logs the deletion action

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.18/CD.puml`) shows the relationships between:
- OperationRequestController
- OperationRequestService
- OperationRequest
- IOperationRequestRepository
- ILoggingService

## Testing
- Unit tests: `Tests/Unit/Controllers/OperationRequestControllerTests.cs` and `Tests/Unit/Domain/OperationRequestServiceTests.cs`
- Integration tests: `Tests/Integration/OperationRequestDeletionIntegrationTests.cs`
- E2E tests: `Tests/E2E/OperationRequestDeletionE2ETests.cs`

## Future Improvements
- Implement a notification system to inform relevant staff about the deletion of an operation request
- Add a feature to reschedule or modify an operation request instead of deleting it
- Add more detailed auditing information, such as reason for deletion
