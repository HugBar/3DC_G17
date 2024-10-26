# User Story 5.1.19: List/Search Operation Requisitions

## Description
As a Doctor, I want to list/search operation requisitions, so that I can see the details, edit, and remove operation requisitions.

## Acceptance Criteria
1. Doctors can search operation requests by patient name, operation type, priority, and status.
2. The system displays a list of operation requests in a searchable and filterable view.
3. Each entry in the list includes operation request details (e.g., patient name, operation type, status).
4. Doctors can select an operation request to view, update, or delete it.

## Implementation Details

### Controller: OperationRequestController
- Located in `Controllers/OperationRequestController.cs`
- Handles HTTP requests for operation request management
- Endpoints:
  - GET `/api/operation-request/filter`: Searches and lists operation requests

### Service: OperationRequestService
- Located in `Domain/OperationRequest/OperationRequestService.cs`
- Implements business logic for operation request management
- Key methods:
  - `SearchOperationRequestsAsync`: Searches and retrieves operation requests based on criteria

### Data Transfer Objects (DTOs)
- `SearchOperationRequestDto`: Used for specifying search criteria
- `OperationRequestDto`: Represents operation request data returned to the client

### Entity: OperationRequest
- Located in `Domain/OperationRequest/OperationRequest.cs`
- Represents an operation request in the system

### Repository: IOperationRequestRepository
- Located in `Domain/OperationRequest/IOperationRequestRepository.cs`
- Defines methods for operation request data persistence and retrieval

### Database
- Uses Entity Framework Core
- Operation request data is configured in `Infrastructure/OperationRequest/OperationRequestEntityTypeConfiguration.cs`

### Authentication & Authorization
- JWT (JSON Web Token) authentication is implemented
- Only doctors are authorized to search and list operation requests

### Logging Service
- Search actions are logged using `ILoggingService`
- Implementation details can be found in `Infrastructure/LoggingService.cs`

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.19/SD.puml`) illustrates the flow of the operation request search process, including:
1. Doctor initiates search request
2. OperationRequestController receives the request
3. OperationRequestService processes the search
4. OperationRequestRepository queries the database
5. Results are mapped to DTOs and returned to the client

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.19/CD.puml`) shows the relationships between:
- OperationRequestController
- OperationRequestService
- OperationRequest
- IOperationRequestRepository
- SearchOperationRequestDto
- OperationRequestDto

## Testing
- Unit tests: `Tests/Unit/Controllers/OperationRequestControllerTests.cs` and `Tests/Unit/Domain/OperationRequestServiceTests.cs`
- Integration tests: `Tests/Integration/OperationRequestSearchIntegrationTests.cs`
- E2E tests: `Tests/E2E/OperationRequestSearchE2ETests.cs`

## Future Improvements
- Add advanced filtering options (e.g., date range, multiple operation types)
- Implement sorting functionality for search results
- Add the ability to export search results to various formats (e.g., CSV, PDF)
- Implement real-time updates for the operation request list
