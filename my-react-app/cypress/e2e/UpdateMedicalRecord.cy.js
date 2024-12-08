describe('Update Medical Record', () => {
  let authToken;

  before(() => {
    // Login to get the token before all tests
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
    // Clear localStorage before each test
    cy.clearLocalStorage();

    // Visit the update medical record page with auth token
    cy.visit('http://localhost:3000/medical-records/update', {
      onBeforeLoad: (win) => {
        win.localStorage.setItem('authToken', authToken);
      }
    });

    // Verify we're on the correct page and not redirected to login
    cy.url().should('include', '/medical-records/update');
  });

  it('should display initial form elements', () => {
    cy.get('h2').should('contain', 'Update Medical Record');
    cy.get('#patientId').should('exist');
    cy.get('button').contains('Search Patient').should('exist');
  });

  it('should handle patient search and display record', () => {
    cy.get('#patientId').type('TestPatient1');
    cy.get('button').contains('Search Patient').click();

    // Verify medical conditions section
    cy.get('h3').contains('Medical Conditions').should('be.visible');
    cy.get('#conditionSelect').should('exist');
    
    // Verify allergies section
    cy.get('h3').contains('Allergies').should('be.visible');
    cy.get('select').last().should('exist');
  });

  it('should add and remove conditions and allergies', () => {
    cy.get('#patientId').type('TestPatient1');
    cy.get('button').contains('Search Patient').click();

    // Add condition
    cy.get('#conditionSelect')
      .select('Asthma - High');
    cy.get('.current-list').contains('Asthma').should('be.visible');

    // Remove the specific condition we just added
    cy.get('.current-list')
      .contains('Asthma')
      .closest('.list-item')
      .find('.remove-button')
      .click({ force: true });
    
    cy.wait(1000); // Add small wait for removal animation/state update
    cy.get('.current-list').contains('Asthma').should('not.exist');

    // Add allergy (using actual value from dropdown)
    cy.get('select').last()
      .select('peanut - High');
    cy.get('.current-list').contains('peanut').should('be.visible');
  });

  it('should handle successful record update', () => {
    cy.get('#patientId').type('TestPatient1');
    cy.get('button').contains('Search Patient').click();

    // Add condition and allergy
    cy.get('#conditionSelect')
      .select('Asthma - High');
    cy.get('select').last()
      .select('peanut - High');

    // Update record
    cy.get('.update-button').click();

    // Verify success message
    cy.get('[role="alert"]')
      .should('have.class', 'success-message')
      .and('contain', 'Medical record updated successfully');
  });

  it('should handle update error gracefully', () => {
    // Intercept the update request and force it to fail
    cy.intercept('PUT', '**/medical-records/update/*', {
      statusCode: 500,
      body: { message: 'Server error' }
    }).as('updateRecord');

    // Search for patient
    cy.get('#patientId').type('TestPatient1');
    cy.get('button').contains('Search Patient').click();

    // Try to update
    cy.get('.update-button').click();

    // Wait for the failed request
    cy.wait('@updateRecord');

    // Verify error message
    cy.get('[role="alert"]')
      .should('have.class', 'error-message')
      .and('contain', 'Failed to update medical record');
  });
});
