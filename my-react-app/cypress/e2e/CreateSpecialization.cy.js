describe('Create Specialization', () => {
    const baseUrl = 'http://localhost:3001/api';
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
      cy.clearLocalStorage();
      
      cy.window().then((win) => {
        win.localStorage.setItem('authToken', authToken);
      });
  
      // Visit the create specialization page
      cy.visit(`${frontendUrl}/specialization/create`, {
        onBeforeLoad: (win) => {
          win.localStorage.setItem('authToken', authToken);
        }
      });
  
      cy.reload();
      cy.get('.create-specialization-container', { timeout: 10000 }).should('be.visible');
    });
  
    it('displays create specialization form correctly', () => {
      cy.get('h2').should('contain', 'Add New Specialization');
      cy.get('#name').should('exist');
      cy.get('#description').should('exist');
      cy.get('button[type="submit"]').should('contain', 'Add Specialization');
    });
  
    it('successfully creates a new specialization', () => {
      cy.intercept('POST', `${baseUrl}/specializations`).as('createSpecialization');
  
      cy.get('#name').type('Cypress Test');
      cy.get('#description').type('Cypress Test Description', { force: true });
      cy.get('button[type="submit"]').click();
  
      cy.wait('@createSpecialization').then((interception) => {
        expect(interception.response.statusCode).to.be.oneOf([200, 201]);
      });
  
      cy.get('.success-message')
        .should('be.visible')
        .and('contain', 'Specialization added successfully!');
    });
  
    it('handles network errors gracefully', () => {
      cy.intercept('POST', `${baseUrl}/specializations`, {
        statusCode: 500,
        body: { message: 'Internal Server Error' }
      }).as('createSpecializationError');
  
      cy.get('#name').type('Test Specialization');
      cy.get('#description').type('Test Description', { force: true });
      cy.get('button[type="submit"]').click();
  
      cy.wait('@createSpecializationError');
  
      cy.get('.error-message').should('contain', 'Internal Server Error');
    });
  
    it('validates required fields', () => {
      cy.get('button[type="submit"]').click();
  
      cy.get('#name')
        .invoke('prop', 'validationMessage')
        .should('not.be.empty');
  
      cy.get('#description')
        .invoke('prop', 'validationMessage')
        .should('not.be.empty');
    });
  
    it('persists form data after failed submission', () => {
      cy.intercept('POST', `${baseUrl}/specializations`, {
        statusCode: 400,
        body: { message: 'Validation Error' }
      }).as('validationError');
  
      const testData = {
        name: 'Test Specialization',
        description: 'Test Description'
      };
  
      cy.get('#name').type(testData.name);
      cy.get('#description').type(testData.description, { force: true });
      cy.get('button[type="submit"]').click();
  
      cy.wait('@validationError');
  
      cy.get('#name').should('have.value', testData.name);
      cy.get('#description').should('have.value', testData.description);
    });
  
    afterEach(() => {
      cy.clearLocalStorage();
    });
  });