# User Story 5.1.16: Request Operation

## Description
As a Doctor, I want to request an operation, so that the Patient has access to the necessary healthcare.

## Acceptance Criteria
1. Doctors can create an operation request by selecting the patient, operation type, priority, and suggested deadline.
2. The system validates that the operation type matches the doctor's specialization.
3. The operation request includes:
   - Patient ID
   - Doctor ID
   - Operation Type
   - Deadline
   - Priority
4. The system confirms successful submission of the operation request and logs the request in the patient's medical history.

## Implementation Details

### Controller: OperationRequestController
- Located in `Controllers/OperationRequestController.cs`
- Handles HTTP requests for operation request management
- Endpoints:
  - POST `/api/operation-request/create`: Creates a new operation request

### Service: OperationRequestService
- Located in `Domain/OperationRequest/OperationRequestService.cs`
- Implements business logic for operation request management
- Key methods:
  - `CreateOperationRequestAsync`: Creates a new operation request

### Data Transfer Objects (DTOs)
- `CreateOperationRequestDto`: Used for creating an operation request
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
- Role-based authorization restricts operation request creation to Doctor users

### Logging Service
- Operation request creation is logged using `ILoggingService`
- Implementation details can be found in `Infrastructure/LoggingService.cs`

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.16/SD.puml`) illustrates the flow of the operation request creation process, including:
1. Doctor initiates operation request creation
2. OperationRequestController receives the request
3. OperationRequestService processes the creation
4. Various repositories are used to validate data and relationships
5. OperationRequestRepository saves the new operation request
6. UnitOfWork commits the transaction
7. LoggingService logs the operation request creation

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.16/CD.puml`) shows the relationships between:
- OperationRequestController
- OperationRequestService
- OperationRequest
- CreateOperationRequestDto
- OperationRequestDto
- IOperationRequestRepository
- IUnitOfWork
- ILoggingService
- IStaffRepository
- IPatientRepository
- IOperationTypeRepository

## Testing
- Unit tests should be implemented in `Tests/Unit/Controllers/OperationRequestControllerTests.cs` and `Tests/Unit/Domain/OperationRequestServiceTests.cs`
- Integration tests should be implemented in a new file `Tests/Integration/OperationRequestCreationIntegrationTests.cs`
- E2E tests should be implemented in a new file `Tests/E2E/OperationRequestCreationE2ETests.cs`

## Future Improvements
- Implement a notification system to alert relevant staff about new operation requests
- Implement a dashboard for doctors to view and manage their operation requests
- Add the ability to update or cancel operation requests
