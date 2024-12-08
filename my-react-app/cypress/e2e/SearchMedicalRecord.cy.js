describe('Search Medical Record', () => {
  let authToken;

  before(() => {
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
    cy.clearLocalStorage();

    cy.visit('http://localhost:3000/medical-records/search', {
      onBeforeLoad: (win) => {
        win.localStorage.setItem('authToken', authToken);
      }
    });

    cy.url().should('include', '/medical-records/search');
  });

  it('should display initial form elements', () => {
    cy.get('h2').should('contain', 'Search Medical Record');
    cy.get('#patientId').should('exist');
    cy.get('#conditionName').should('exist');
    cy.get('#allergyName').should('exist');
    cy.get('button').contains('Search').should('be.disabled');
  });

  it('should search medical records with all filters', () => {
    const patientId = `searchtest_${Date.now()}`;
    const updateData = {
      conditions: [
        { name: 'Asthma', severity: 'High' },
        { name: 'Diabetes', severity: 'Medium' }
      ],
      allergies: [
        { name: 'Peanuts', severity: 'High' },
        { name: 'Shellfish', severity: 'Low' }
      ]
    };

    // Create test record
    cy.request({
      method: 'PUT',
      url: `http://localhost:3001/medical-records/update/${patientId}`,
      headers: { Authorization: `Bearer ${authToken}` },
      body: updateData
    }).then(() => {
      // Search for the record
      cy.get('#patientId').type(patientId);
      cy.get('#conditionName').type('Asthma');
      cy.get('#allergyName').type('Peanuts');
      cy.get('button').contains('Search').click();

      // Verify results
      cy.get('.medical-record-details').should('be.visible');
      cy.contains('Asthma - Severity: High').should('be.visible');
      cy.contains('Peanuts - Severity: High').should('be.visible');
      cy.contains('Diabetes').should('not.exist');
      cy.contains('Shellfish').should('not.exist');
    });
  });

  it('should handle search with only condition filter', () => {
    const patientId = `searchtest_${Date.now()}`;
    const updateData = {
      conditions: [
        { name: 'Asthma', severity: 'High' },
        { name: 'Diabetes', severity: 'Medium' }
      ],
      allergies: [
        { name: 'Peanuts', severity: 'High' }
      ]
    };

    cy.request({
      method: 'PUT',
      url: `http://localhost:3001/medical-records/update/${patientId}`,
      headers: { Authorization: `Bearer ${authToken}` },
      body: updateData
    }).then(() => {
      cy.get('#patientId').type(patientId);
      cy.get('#conditionName').type('Asthma');
      cy.get('button').contains('Search').click();

      cy.get('.medical-record-details').should('be.visible');
      cy.contains('Asthma - Severity: High').should('be.visible');
      cy.contains('Diabetes').should('not.exist');
      cy.get('.allergies').should('not.exist');
    });
  });

  it('should handle non-existent patient gracefully', () => {
    cy.intercept('GET', '**/medical-records/search*', {
      statusCode: 404,
      body: { message: 'Medical record not found' }
    }).as('searchRecord');

    cy.get('#patientId').type('nonexistent');
    cy.get('button').contains('Search').click();

    cy.wait('@searchRecord');

    cy.get('[role="alert"]')
      .should('have.class', 'error-message')
      .and('contain', 'Medical record not found');
  });

  it('should handle server error gracefully', () => {
    cy.intercept('GET', '**/medical-records/search*', {
      statusCode: 500,
      body: { message: 'Server error' }
    }).as('searchRecord');

    cy.get('#patientId').type('TEST123');
    cy.get('button').contains('Search').click();

    cy.wait('@searchRecord');

    cy.get('[role="alert"]')
      .should('have.class', 'error-message')
      .and('contain', 'Medical record not found');
  });
}); 