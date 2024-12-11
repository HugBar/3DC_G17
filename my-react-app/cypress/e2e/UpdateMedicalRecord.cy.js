describe('Update Medical Record', () => {
  let authToken;
  const baseUrl = 'http://localhost:3001/medical-records';
  const frontendUrl = 'http://localhost:3000';
  let testPatientId;

  before(() => {
    // Login to get the token
    cy.request({
      method: 'POST',
      url: 'https://localhost:5001/api/auth/login',
      body: {
        email: 'doctor@doctor.com',
        password: 'Doctor123!'
      }
    }).then((response) => {
      authToken = response.body.token;

      // Create a test patient record
      testPatientId = `updatetest_${Date.now()}`;
      return cy.request({
        method: 'POST',
        url: `${baseUrl}/create/${testPatientId}`,
        headers: { Authorization: `Bearer ${authToken}` }
      });
    });
  });

  beforeEach(() => {
    cy.clearLocalStorage();

    // Visit the update page with the test patient ID
    cy.visit(`${frontendUrl}/medical-records/update/${testPatientId}`, {
      onBeforeLoad: (win) => {
        win.localStorage.setItem('authToken', authToken);
      }
    });

    cy.url().should('include', `/medical-records/update/${testPatientId}`);
  });

  it('should display initial form elements', () => {
    cy.get('h2').should('contain', 'Update Medical Record');
    cy.get('h3').contains('Medical Conditions').should('be.visible');
    cy.get('#conditionSelect').should('exist');
    cy.get('h3').contains('Allergies').should('be.visible');
    cy.get('select').last().should('exist');
  });

  it('should add and remove conditions and allergies', () => {
    // Add condition
    cy.get('#conditionSelect')
      .select('Asthma - High');
    cy.get('.current-list').contains('Asthma').should('be.visible');

    // Remove condition
    cy.get('.current-list')
      .contains('Asthma')
      .closest('.list-item')
      .find('.remove-button')
      .click({ force: true });
    
    cy.wait(1000); // Wait for removal animation/state update
    cy.get('.current-list').contains('Asthma').should('not.exist');

    // Add allergy
    cy.get('select').last()
      .select('peanut - High');
    cy.get('.current-list').contains('peanut').should('be.visible');
  });

  it('should handle successful record update', () => {
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
    cy.intercept('PUT', `**/medical-records/update/${testPatientId}`, {
      statusCode: 500,
      body: { message: 'Internal server error' }
    }).as('updateRecord');

    // Add a condition and try to update
    cy.get('#conditionSelect')
      .select('Asthma - High');
    cy.get('.update-button').click();

    // Wait for the failed request
    cy.wait('@updateRecord');

    // Verify error message
    cy.get('[role="alert"]')
      .should('have.class', 'error-message')
      .and('contain', 'Failed to update medical record');
  });

  after(() => {
    // Clean up test data
    if (testPatientId) {
      cy.request({
        method: 'DELETE',
        url: `${baseUrl}/${testPatientId}`,
        headers: { Authorization: `Bearer ${authToken}` },
        failOnStatusCode: false
      });
    }
  });
});
