# User Story 5.1.21: Edit Operation Types

## Description
As an Admin, I want to edit existing operation types, so that I can update or correct information about the procedures in the system.

## Dependencies

### User Story Dependencies
1. **US 5.1.20** - Create Operation Types
   - Required for operation types to exist in the system
   - Provides base operation type structure

2. **US 5.1.6** - Admin Authentication
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

## Implementation Details

### Controller: OperationTypeController
Located in `Controllers/OperationTypeController.cs`

### Key Components
1. **Domain Model
- OperationType entity with validation rules: "Operation type name can´t be empty"

2. **Data Transfer Objects
- UpdateOperationTypeDto:

## Accptance Criteria 
- Admins can search for and select an existing operation type to edit.
- Editable fields include operation name, required staff by specialization, and estimated
duration.
- Changes are reflected in the system immediately for future operation requests.
- Historical data is maintained, but new operation requests will use the updated operation type
information.

## Technical Implementation
### Service Layer (OperationTypeService)
1. **Task<OperationTypeDto> UpdateOperationType(OperationTypeId id, JsonPatchDocument<UpdateOperationTypeDto> patchDoc)
- Handles business logic
- Validates updates
- Manages transactions

### Repository Layer
1. **Task<OperationType> UpdateAsync(OperationType operationType)

## Security
### Authentication & Authorization

### Validation
- Input validation
- Business rule enforcement
- Role-based access control

## Sequence Diagram
![Sequence Diagram](docs/Sprint1/US%205.1.21/02.design/svg/sequenceDiagram.svg)

## Class Diagram 
![Class Diagram](docs/Sprint1/US%205.1.21/02.design/svg/class_diagram.svg)

## Testing

## Future Improvements

