describe('Login Page', () => {
    const baseUrl = 'https://localhost:5001/api/auth';
    const frontendUrl = 'http://localhost:3000';
  
    beforeEach(() => {
      // Clear localStorage and visit login page before each test
      cy.clearLocalStorage();
      cy.visit(`${frontendUrl}/login`);
    });
  
    it('displays login form correctly', () => {
      cy.get('h2').should('contain', 'Login');
      cy.get('input[type="email"]').should('exist');
      cy.get('input[type="password"]').should('exist');
      cy.get('button[type="submit"]').should('contain', 'Login');
    });
  
    it('successfully logs in with valid credentials', () => {
      cy.intercept('POST', `${baseUrl}/login`).as('loginRequest');
  
      cy.get('input[type="email"]').type('admin@admin.com');
      cy.get('input[type="password"]').type('Admin123!');
      cy.get('button[type="submit"]').click();
  
      cy.wait('@loginRequest').then((interception) => {
        expect(interception.response.statusCode).to.equal(200);
      });
  
      // Verify redirect to home page
      cy.url().should('eq', `${frontendUrl}/`);
  
      // Verify token is stored in localStorage
      cy.window().its('localStorage')
        .invoke('getItem', 'authToken')
        .should('exist');
    });
  
    it('shows error message with invalid credentials', () => {
      cy.intercept('POST', `${baseUrl}/login`, {
        statusCode: 401,
        body: 'Invalid credentials'
      }).as('failedLogin');
  
      cy.get('input[type="email"]').type('wrong@email.com');
      cy.get('input[type="password"]').type('WrongPassword123!');
      cy.get('button[type="submit"]').click();
  
      cy.wait('@failedLogin');
      cy.get('.login-error').should('contain', 'Error logging in.');
    });
  
    it('validates email format', () => {
      cy.get('input[type="email"]').type('invalid-email');
      cy.get('input[type="password"]').type('Password123!');
      cy.get('button[type="submit"]').click();
  
      // Check HTML5 validation message
      cy.get('input[type="email"]').then($input => {
        expect($input[0].validationMessage).to.not.be.empty;
      });
    });
  
    it('validates required fields', () => {
      cy.get('button[type="submit"]').click();
  
      // Check HTML5 validation for empty email
      cy.get('input[type="email"]').then($input => {
        expect($input[0].validationMessage).to.not.be.empty;
      });
    });
  
    it('handles server error gracefully', () => {
      cy.intercept('POST', `${baseUrl}/login`, {
        statusCode: 500,
        body: 'Internal Server Error'
      }).as('serverError');
  
      cy.get('input[type="email"]').type('test@example.com');
      cy.get('input[type="password"]').type('Password123!');
      cy.get('button[type="submit"]').click();
  
      cy.wait('@serverError');
      cy.get('.login-error').should('contain', 'Error logging in.');
    });
  
    it('maintains form data after failed submission', () => {
      cy.intercept('POST', `${baseUrl}/login`, {
        statusCode: 401,
        body: 'Invalid credentials'
      }).as('failedLogin');
  
      const testEmail = 'test@example.com';
      cy.get('input[type="email"]').type(testEmail);
      cy.get('input[type="password"]').type('Password123!');
      cy.get('button[type="submit"]').click();
  
      cy.wait('@failedLogin');
      cy.get('input[type="email"]').should('have.value', testEmail);
    });
  
    it('clears form after successful login', () => {
      cy.intercept('POST', `${baseUrl}/login`, {
        statusCode: 200,
        body: { token: 'fake-jwt-token' }
      }).as('successfulLogin');
  
      cy.get('input[type="email"]').type('admin@admin.com');
      cy.get('input[type="password"]').type('Admin123!');
      cy.get('button[type="submit"]').click();
  
      cy.wait('@successfulLogin');
      cy.get('input[type="email"]').should('have.value', '');
      cy.get('input[type="password"]').should('have.value', '');
    });
  
    afterEach(() => {
      // Clean up after each test
      cy.clearLocalStorage();
    });
  });