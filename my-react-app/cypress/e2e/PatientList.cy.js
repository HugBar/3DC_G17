describe('Patient List', () => {
    const baseUrl = 'https://localhost:5001/api/patient';
    const frontendUrl = 'http://localhost:3000';
    let authToken;
  
    before(() => {
      // Login to get the token
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
  
      // Visit the patient list page
      cy.visit(`${frontendUrl}/patient/list`, {
        onBeforeLoad: (win) => {
          win.localStorage.setItem('authToken', authToken);
        }
      });
  
      // Wait for the page to load
      cy.get('.patient-list-container', { timeout: 10000 }).should('be.visible');
  
      // Ensure no modal is open by clicking outside or pressing ESC
      cy.get('body').type('{esc}');
    });
  
    it('displays patient list and filters correctly', () => {
      // Ensure modal is closed before interacting with filters
      cy.get('.patient-details-modal').should('not.exist');
      
      // Check if filter inputs exist
      cy.get('input[name="firstName"]').should('exist');
      cy.get('input[name="lastName"]').should('exist');
      cy.get('input[name="email"]').should('exist');
      cy.get('input[name="medicalNr"]').should('exist');
  
      // Test filtering
      cy.get('input[name="firstName"]').should('be.visible').type('John');
      cy.url().should('include', 'firstName=John');
      cy.url().should('include', 'page=1');
  
      // Clear filters
      cy.get('.clear-filters-button').click();
      cy.url().should('not.include', 'firstName=John');
    });
  
    it('shows patient details when clicking on a patient', () => {
      // Wait for patient list to load and click first patient
      cy.get('.patient-card').first().click();
  
      // Check if modal appears with patient details
      cy.get('.patient-details-modal').should('be.visible');
      cy.get('.modal-content').within(() => {
        cy.contains('Patient Details').should('be.visible');
        cy.contains('Update Patient').should('be.visible');
        cy.get('button.delete-button')
          .scrollIntoView() // Scrolls the button into view
          .should('be.visible') // Ensures the button is now visible
        cy.contains('Close').should('be.visible');
      });
  
      // Close modal
      cy.get('.close-button').click();
      cy.get('.patient-details-modal').should('not.exist');
    });
  
    it('handles pagination correctly', () => {
      // Check if pagination controls exist
      cy.get('.pagination-controls').should('exist');
      
      // Test next page
      cy.get('.pagination-button')
  .contains('Next')
        .should('not.be.disabled')
        .click();
      cy.url().should('include', 'page=2');
  
      // Test previous page
      cy.get('.pagination-button').contains('Previous').click();
      cy.url().should('include', 'page=1');
    });
  
    it('navigates to update patient when clicking update button', () => {
      cy.get('.patient-card').first().click();
      cy.get('.update-button').click();
      cy.url().should('include', '/admin/edit-patient-profile/');
    });
  
    it('handles delete patient flow', () => {
      cy.get('.patient-card').first().click();
      cy.get('.delete-button').click();
      cy.url().should('include', '/patient/delete/');
    });
  
    it('displays error message when API fails', () => {
      // Intercept API call and force it to fail
      cy.intercept('GET', `${baseUrl}/filter*`, {
        statusCode: 500,
        body: 'Server error'
      }).as('getPatientError');
  
      // Reload page to trigger API call
      cy.reload();
  
      // Check if error message is displayed
      cy.get('.error-message').should('contain', 'Error fetching patient list.');
    });
  
    it('updates URL with multiple filters', () => {
      // Ensure modal is closed
      cy.get('.patient-details-modal').should('not.exist');
      
      // Apply multiple filters
      cy.get('input[name="firstName"]').should('be.visible').type('John');
      cy.get('input[name="lastName"]').should('be.visible').type('Doe');
      cy.get('input[name="email"]').should('be.visible').type('john@example.com');
      cy.get('input[name="medicalNr"]').should('be.visible').type('MED-12345678');
  
      // Check URL contains all filters
      cy.url().should('include', 'firstName=John')
        .and('include', 'lastName=Doe')
        .and('include', 'email=john%40example.com')
        .and('include', 'medicalNr=MED-12345678')
        .and('include', 'page=1');
    });
  
    it('persists filters and pagination after page reload', () => {
      // Apply filters and go to next page
      cy.get('input[name="firstName"]').type('John');
      cy.get('.pagination-button').contains('Next').click();
  
      // Reload page
      cy.reload();
  
      // Check if filters and page number are maintained
      cy.get('input[name="firstName"]').should('have.value', 'John');
      cy.url().should('include', 'page=2');
    });
  
    it('displays correct patient information in cards', () => {
      cy.get('.patient-card').first().within(() => {
        cy.get('h3').should('exist'); // Name
        cy.contains('Medical Record #:').should('exist');
        cy.contains('Email:').should('exist');
        cy.contains('Phone:').should('exist');
        cy.contains('Date of Birth:').should('exist');
      });
    });
  
    it('displays detailed patient information in modal', () => {
      cy.get('.patient-card').first().click();
      
      cy.get('.patient-details-modal').within(() => {
        cy.contains('Name:').should('exist');
        cy.contains('Email:').should('exist');
        cy.contains('Medical Record #:').should('exist');
        cy.contains('Phone:').should('exist');
        cy.contains('Date of Birth:').should('exist');
        cy.contains('Gender:').should('exist');
        cy.contains('Emergency Contact:').should('exist');
      });
    });
  });