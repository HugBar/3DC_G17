# User Story 5.1.11: List/Search Patient Profiles

## Description
As an Admin, I want to list/search patient profiles by different attributes, so that I can view the details, edit, and remove patient profiles.

## Dependencies

### User Story Dependencies
1. **US 5.1.1** - User Registration System
   - Required for patient accounts to exist in the system
   - Provides basic user authentication framework

2. **US 5.1.8** - Patient Profile Creation 
   - Provides the base patient profile structure
   - Required for profile editing functionality



### System Dependencies
1. **Authentication System**
   - JWT token-based authentication
   - Admin role verification
   - Login functionality (AuthController.cs)

2. **Patient Management System**
   - Patient repository and data access
   - Patient profile data structure
   - CRUD operations for patient profiles

## Acceptance Criteria
- Admins can search patient profiles by various attributes, including name, email, date of birth,
or medical record number.
- The system displays search results in a list view with key patient information (name, email, date
of birth).
- Admins can select a profile from the list to view, edit, or delete the patient record.
- The search results are paginated, and filters are available to refine the search results.

## Implementation Details

### Controllers
- Located in `Controllers/PatientController.cs`
- Requires admin authentication via JWT
- Endpoints:
  - GET `/api/patient/search`: Search with filters
  - GET `/api/patient/{id}`: Get specific profile
  - DELETE `/api/patient/{id}`: Remove profile

### Services
- PatientService for business logic
- AuthService for authentication
- Logging service for audit trail
- Email service for notifications

### Data Transfer Objects
- `PatientFilterDTO`: Search criteria
- `PatientDTO`: Patient data representation

### Security
- JWT authentication required
- Admin role verification
- Audit logging for all operations

## Sequence Diagram 
![Sequence Diagram](docs/Sprint1/US%205.1.11/02.design/svg/sequence_diagram.svg)

## Class Diagram
![Class Diagram](docs/Sprint1/US%205.1.11/02.design/svg/class_diagram.svg)

## Testing
- Unit tests are located in `Tests/Unit/Controllers/PatientControllerTests.cs` and `Tests/Unit/Domain/PatientServiceTests.cs`
- Integrations tests are located in `Tests/Integration/Domain/PatientServiceTests.cs`
- E2E tests are located in `cypress/e2e/Patient.cy.js` 

## Future Improvements
1. Advanced search capabilities
   - Fuzzy matching
   - Phonetic search
   - Complex filter combinations

2. Enhanced reporting
   - Export search results
   - Batch operations
   - Statistical analysis

3. Performance optimizations
   - Caching frequently searched data
   - Search query optimization
   - Index optimization
