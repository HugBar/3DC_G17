# User Story 5.1.14: Deactivate Staff Profile

## Description
As an Admin, I want to deactivate a staff profile, so that I can remove them from the hospital's active roster while preserving their historical data for record-keeping purposes.

## Acceptance Criteria
1. Admins can search for and select a staff profile to deactivate.
2. Deactivating a staff profile removes them from the active roster, but their historical data (e.g., appointments) remains accessible.
3. The system confirms deactivation and records the action for audit purposes.

## Implementation Details

### Controller: StaffController
- Located in `Controllers/StaffController.cs`
- Handles HTTP requests for staff profile management
- Endpoints:
  - PATCH `/api/staff/deactivate/{id}`: Deactivates staff profile
  - GET `/api/staff/{id}`: Retrieves staff profile details
  - GET `/api/staff/filter`: Searches staff profiles

### Service: StaffService
- Located in `Domain/Staff/StaffService.cs`
- Implements business logic for staff management
- Key methods:
  - `DeactivateAsync`: Processes staff deactivation
  - `GetByIdAsync`: Retrieves staff profile
  - `ValidateDeactivation`: Checks for pending operations/appointments

### Data Transfer Objects (DTOs)
- `StaffDto`: Represents staff profile data
- `StaffFilterDto`: Contains search criteria

### Entity Configuration
- Located in `Infrastructure/Staff/StaffEntityTypeConfiguration.cs`
- Defines staff profile schema including:
  - Active status flag
  - Historical data relationships
  - Audit fields

### Security Features
- JWT token-based authentication
- Admin role verification
- Audit trail logging
- Data access controls

### Logging Service
- Interface: `Domain/Shared/ILoggingService.cs`
- Implementation: `Infrastructure/LoggingService.cs`
- Logs:
  - Deactivation attempts
  - Successful deactivations
  - Admin actions
  - Status changes

## Sequence Diagram
The sequence diagram (`docs/Sprint1/US 5.1.14/02.design/puml/sequenceDiagram.puml`) illustrates:
1. Admin searches for staff profile
2. System displays staff details
3. Admin initiates deactivation
4. System validates request
5. Profile status updated
6. Confirmation sent

## Class Diagram
The class diagram (`docs/Sprint1/US 5.1.14/02.design/puml/class_diagram.puml`) shows relationships between:
- StaffController
- StaffService
- IStaffRepository
- ILoggingService
- Staff entity
- Data transfer objects

## Testing
- Unit tests in `Tests/Unit/Controllers/StaffControllerTests.cs`
- Unit tests in `Tests/Unit/Domain/StaffServiceTests.cs`
- Tests cover:
  - Admin authorization
  - Deactivation process
  - Historical data preservation
  - Audit logging

## Future Improvements

1. Process Enhancement
   - Automated deactivation scheduling
   - Batch deactivation capabilities
   - Configurable data retention policies
   - Historical data archiving options
   - Automated notifications
