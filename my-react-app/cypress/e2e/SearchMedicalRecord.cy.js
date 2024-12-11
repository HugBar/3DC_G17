describe('Search Medical Record', () => {
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

      // Create a test medical record
      testPatientId = `searchtest_${Date.now()}`;
      return cy.request({
        method: 'POST',
        url: `${baseUrl}/create/${testPatientId}`,
        headers: { Authorization: `Bearer ${authToken}` }
      });
    }).then(() => {
      // Add test conditions and allergies
      return cy.request({
        method: 'PUT',
        url: `${baseUrl}/update/${testPatientId}`,
        headers: { Authorization: `Bearer ${authToken}` },
        body: {
          conditions: [
            { name: 'Asthma', severity: 'High' },
            { name: 'Diabetes', severity: 'Medium' }
          ],
          allergies: [
            { name: 'Peanuts', severity: 'High' },
            { name: 'Shellfish', severity: 'Low' }
          ]
        }
      });
    });
  });

  beforeEach(() => {
    cy.clearLocalStorage();
    cy.visit(`${frontendUrl}/medical-records/search`, {
      onBeforeLoad: (win) => {
        win.localStorage.setItem('authToken', authToken);
      }
    });
  });

  it('should search by patient ID only', () => {
    cy.get('#patientId').type(testPatientId);
    cy.get('button').contains('Search').click();

    cy.get('.medical-record-details', { timeout: 10000 }).should('be.visible');
    cy.contains('Asthma - Severity: High').should('be.visible');
    cy.contains('Diabetes - Severity: Medium').should('be.visible');
    cy.contains('Peanuts - Severity: High').should('be.visible');
    cy.contains('Shellfish - Severity: Low').should('be.visible');
  });

  it('should search by patient ID and condition', () => {
    cy.get('#patientId').type(testPatientId);
    cy.get('#conditionName').type('Asthma');
    cy.get('button').contains('Search').click();

    cy.get('.medical-record-details').should('be.visible');
    cy.contains('Asthma - Severity: High').should('be.visible');
    cy.contains('Diabetes').should('not.exist');
  });

  it('should search by patient ID and allergy', () => {
    cy.get('#patientId').type(testPatientId);
    cy.get('#allergyName').type('Peanuts');
    cy.get('button').contains('Search').click();

    cy.get('.medical-record-details').should('be.visible');
    cy.contains('Peanuts - Severity: High').should('be.visible');
    cy.contains('Shellfish').should('not.exist');
  });

  it('should search with all filters', () => {
    cy.get('#patientId').type(testPatientId);
    cy.get('#conditionName').type('Asthma');
    cy.get('#allergyName').type('Peanuts');
    cy.get('button').contains('Search').click();

    cy.get('.medical-record-details').should('be.visible');
    cy.contains('Asthma - Severity: High').should('be.visible');
    cy.contains('Peanuts - Severity: High').should('be.visible');
    cy.contains('Diabetes').should('not.exist');
    cy.contains('Shellfish').should('not.exist');
  });

  it('should handle non-existent patient ID', () => {
    cy.get('#patientId').type('nonexistent');
    cy.get('button').contains('Search').click();

    cy.get('[role="alert"]')
      .should('have.class', 'error-message')
      .and('contain', 'Medical record not found');
  });

  it('should handle server errors gracefully', () => {
    cy.intercept('GET', '**/medical-records/search*', {
      statusCode: 500,
      body: { message: 'Server error' }
    }).as('searchError');

    cy.get('#patientId').type(testPatientId);
    cy.get('button').contains('Search').click();

    cy.wait('@searchError');
    cy.get('[role="alert"]')
      .should('have.class', 'error-message')
      .and('contain', 'Medical record not found');
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