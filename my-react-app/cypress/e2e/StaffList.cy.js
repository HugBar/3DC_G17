describe('Staff List', () => {
    const baseUrl = 'https://localhost:5001/api/staff';
    const frontendUrl = 'http://localhost:3000';
    let authToken;
  
    before(() => {
      // Login to get the token before tests
      cy.request({
        method: 'POST',
        url: 'https://localhost:5001/api/auth/login',
        body: {
          email: 'admin@admin.com',
          password: 'Admin123!'
        }
      }).then((response) => {
        authToken = response.body.token;
      });
    });
  
    beforeEach(() => {
      // Set up authentication before each test
      cy.window().then((win) => {
        win.localStorage.setItem('authToken', authToken);
        win.localStorage.setItem('userRole', 'Admin');
      });
  
      // Visit the staff list page
      cy.visit(`${frontendUrl}/staff/filter`, {
        onBeforeLoad: (win) => {
          win.localStorage.setItem('authToken', authToken);
        }
      });
  
      // Wait for the page to load
      cy.get('.staff-list-container', { timeout: 10000 }).should('be.visible');
    });
  
    it('displays staff list and filters correctly', () => {
      // Check if the filter inputs exist
      cy.get('input[name="firstName"]').should('exist');
      cy.get('input[name="lastName"]').should('exist');
      cy.get('input[name="email"]').should('exist');
      cy.get('input[name="specialization"]').should('exist');
  
      // Test filtering
      cy.get('input[name="firstName"]').type('John');
      cy.url().should('include', 'firstName=John');
      
      // Clear filters
      cy.get('.clear-filters-button').click();
      cy.url().should('not.include', 'firstName=John');
    });
  
    it('shows staff details when clicking on a staff member', () => {
      // Wait for staff list to load
      cy.get('.staff-card').first().click();
  
      // Check if modal appears with staff details
      cy.get('.staff-details-modal').should('be.visible');
      cy.get('.modal-content').within(() => {
        cy.contains('Staff Details').should('be.visible');
        cy.contains('Update Staff').should('be.visible');
        cy.contains('Deactivate Staff').should('be.visible');
        cy.contains('Close').should('be.visible');
      });
  
      // Close modal
      cy.get('.close-button').click();
      cy.get('.staff-details-modal').should('not.exist');
    });
  
    it('navigates to update staff page when clicking update button', () => {
      cy.get('.staff-card').first().click();
      cy.get('.update-button').click();
      cy.url().should('include', '/staff/update/');
    });
  
    it('handles deactivation of staff member', () => {
      cy.get('.staff-card').first().click();
      cy.get('.deactivate-button').click();
      cy.url().should('include', '/staff/deactivate/');
    });
  
    it('displays error message when API fails', () => {
      // Intercept API call and force it to fail
      cy.intercept('GET', `${baseUrl}/filter*`, {
        statusCode: 500,
        body: 'Server error'
      }).as('getStaffError');
  
      // Reload page to trigger API call
      cy.reload();
  
      // Check if error message is displayed
      cy.get('.error-message').should('contain', 'Error fetching staff list.');
    });
  
    it('displays no results message when no staff found', () => {
      // Intercept API call and return empty array
      cy.intercept('GET', `${baseUrl}/filter*`, {
        statusCode: 200,
        body: []
      }).as('getEmptyStaff');
  
      // Reload page to trigger API call
      cy.reload();
  
      // Check if no results message is displayed
      cy.get('.no-results').should('contain', 'No staff members found');
    });
  
    it('updates URL when applying filters', () => {
      // Test multiple filters
      cy.get('input[name="firstName"]').type('John');
      cy.get('input[name="lastName"]').type('Doe');
      cy.get('input[name="specialization"]').type('Doctor');
  
      // Check URL contains all filters
      cy.url().should('include', 'firstName=John')
        .and('include', 'lastName=Doe')
        .and('include', 'specialization=Doctor');
    });
  
    it('persists filters after page reload', () => {
      // Apply filters
      cy.get('input[name="firstName"]').type('John');
      cy.get('input[name="specialization"]').type('Doctor');
  
      // Reload page
      cy.reload();
  
      // Check if filters are still applied
      cy.get('input[name="firstName"]').should('have.value', 'John');
      cy.get('input[name="specialization"]').should('have.value', 'Doctor');
    });
  
    it('handles multiple staff selections', () => {
      // Click first staff member
      cy.get('.staff-card').first().click();
      cy.get('.close-button').click();
  
      // Click second staff member
      cy.get('.staff-card').eq(1).click();
      cy.get('.staff-details-modal').should('be.visible');
    });
  });