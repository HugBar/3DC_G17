describe('Operation Request List', () => {
  const baseUrl = 'https://localhost:5001/api/operationrequest';
  const frontendUrl = 'http://localhost:3000';
  let authToken;

  before(() => {
    // Login to get the token before tests
    cy.request({
      method: 'POST',
      url: 'https://localhost:5001/api/auth/login',
      body: {
        email: 'doctor@doctor.com',
        password: 'Doctor123!'
      }
    }).then((response) => {
      authToken = response.body.token;
    });
  });

  beforeEach(() => {
    // Set up authentication before each test
    cy.window().then((win) => {
      win.localStorage.setItem('authToken', authToken);
      win.localStorage.setItem('userRole', 'Doctor');
    });

    // Visit the operation request list page
    cy.visit(`${frontendUrl}/operationrequest/filter`, {
      onBeforeLoad: (win) => {
        win.localStorage.setItem('authToken', authToken);
      }
    });

    // Wait for the page to load
    cy.get('.operation-request-list-container', { timeout: 10000 }).should('be.visible');
  });

  it('displays operation request list and filters correctly', () => {
    // Check if filter inputs exist
    cy.get('input[name="doctorLicenseNumber"]').should('exist');
    cy.get('input[name="patientMedicalNumber"]').should('exist');
    cy.get('input[name="priority"]').should('exist');

    // Test filtering
    cy.get('input[name="doctorLicenseNumber"]').type('LIC-123');
    cy.url().should('include', 'doctorLicenseNumber=LIC-123');

    // Clear filters
    cy.get('.clear-filters-button').click();
    cy.url().should('not.include', 'doctorLicenseNumber=LIC-123');
  });

  it('shows operation request details when clicking on a request', () => {
    // Wait for list to load and click first request
    cy.get('.operation-request-card').first().click();

    // Verify modal content
    cy.get('.operation-request-details-modal').should('be.visible');
    cy.get('.modal-content').within(() => {
      cy.contains('Operation Request Details').should('be.visible');
      cy.contains('Back to List').should('be.visible');
      cy.contains('Delete').should('be.visible');
    });

    // Close modal
    cy.get('.back-button').click();
    cy.get('.operation-request-details-modal').should('not.exist');
  });

  it('persists filters after page reload', () => {
    // Apply filters
    cy.get('input[name="doctorLicenseNumber"]').type('LIC-123');
    cy.get('input[name="priority"]').type('urgent');

    // Reload page
    cy.reload();

    // Verify filters persist
    cy.get('input[name="doctorLicenseNumber"]').should('have.value', 'LIC-123');
    cy.get('input[name="priority"]').should('have.value', 'urgent');
  });

  it('displays no results message when no requests found', () => {
    // Intercept API call and return empty array
    cy.intercept('GET', `${baseUrl}/filter*`, {
      statusCode: 200,
      body: []
    }).as('getEmptyRequests');

    cy.reload();
    cy.get('.no-results').should('contain', 'No operation requests found');
  });

  it('updates URL with multiple filters', () => {
    // Apply multiple filters
    cy.get('input[name="doctorLicenseNumber"]').type('LIC-123');
    cy.get('input[name="patientMedicalNumber"]').type('PAT-456');
    cy.get('input[name="priority"]').type('urgent');

    // Verify URL contains all filters
    cy.url().should('include', 'doctorLicenseNumber=LIC-123')
      .and('include', 'patientMedicalNumber=PAT-456')
      .and('include', 'priority=urgent');
  });
}); 