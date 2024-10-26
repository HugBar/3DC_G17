# User Story 5.1.15: List/Search Staff Profiles

## Description
As an Admin, I want to list/search staff profiles, so that I can see the details, edit, and remove staff profiles.

## Dependencies

### User Story Dependencies
1. **US 5.1.12** - Staff Profile Creation
   - Required for staff profiles to exist in the system
   - Provides the base staff profile structure

2. **US 5.1.6** - Admin Authentication
   - Required for admin access validation
   - Provides JWT authentication

### System Dependencies
1. **Authentication System**
   - JWT token-based authentication
   - Admin role verification
   - Authorization policies

2. **Staff Management System**
   - Staff repository and data access
   - Staff profile data structure
   - CRUD operations

## Acceptence Criteria
- Admins can search staff profiles by attributes such as name, email, or specialization.
- The system displays search results in a list view with key staff information (name, email,
specialization).
- Admins can select a profile from the list to view, edit, or deactivate.
- The search results are paginated, and filters are available for refining the search results.

## Implementation Details

### Controller: StaffController
- Located in `Controllers/StaffController.cs`
- Endpoint: `GET /api/staff/filter`

### Services
- StaffService handles business logic:
- getStaffFilteredAsync: Processes search filters

### Data Transfer Objects
- StaffFilterDto
- StaffDto

### Repository Layer
**IStaffRepository defines:
- GetFilteredStaffAsync
- GetByIdAsync
- UpdateAsync
- Pagination support

### Error Handling
1. **Authentication Errors

- Unauthorized access (401)
- Insufficient permissions (403)

2. **Business Logic Errors

- Not Found (404)
- Invalid filters (400)
- Server errors (500)

## Sequence Diagram
- Same as the user story 5.1.11

## Class Diagram
- Same as the user story 5.1.11

## Testing

- Unit tests are located in `Tests/Unit/Controllers/StaffControllerTests.cs` and `Tests/Unit/Domain/StaffServiceTests.cs`
- Integrations tests are located in `Tests/Integration/Domain/StaffServiceTests.cs`
- E2E tests are located in `cypress/e2e/Staff.cy.js` 

## Security

1. **Authentication

- JWT token validation
- Role-based authorization

2. **Authorization

- Admin role requirement
- Operation permissions

3. **Data Protection

- Sensitive data filtering
- Response data sanitization

Future Improvements
