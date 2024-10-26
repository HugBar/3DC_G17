# User Story 5.1.20: Create New Operation Types

## Description
As an Admin, I want to add new types of operations, so that I can reflect on the available medical procedures in the system.

## Dependencies

### User Story Dependencies
1. **US 5.1.6** - Admin Authentication
   - Required for admin access validation
   - Provides JWT authentication

### System Dependencies
1. **Authentication System**
   - JWT token-based authentication
   - Admin role verification
   - Role-based authorization

2. **Operation Management System**
   - Operation type persistence
   - Validation rules
   - Logging system

## Acceptance Criteria

- Admins can add new operation types with attributes like:
 - Operation Name
 - Required Staff by Specialization
 - Estimated Duration
- The system validates that the operation name is unique.
- The system logs the creation of new operation types and makes them available for scheduling
immediately.

## Implementation Details

### Controller: OperationTypeController
Located in `Controllers/OperationTypeController.cs

### Domain Model
1. **OperationType Entity

2. **OperationPhases

### Data Transfers Objects
1. **CreateOperationTypeDto

## Technical Implementation

### Service Layer (OperationTypeService)
- Task<OperationTypeDto> AddAsync(CreateOperationTypeDto dto)

### Repository Layer
- Task<OperationType> addAsync(OperationType operationType)

## Sequence Diagram
![Sequence Diagram](docs/Sprint1/US%205.1.20/02.design/svg/sequenceDiagram.svg)

## Testing

## Security

### Authentication
- JWT token requirement
- Admin role verification

### Validation
- Input validation
- Business rule enforcement
- Error handling
 
 ### Future Improvements