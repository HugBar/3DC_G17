/*describe('Delete Account', () => {
  const baseUrl = 'https://localhost:5001/api';
  const authUrl = 'https://localhost:5001/api/auth';
  const userUrl = 'https://localhost:5001/api/user';
  const frontendUrl = 'http://localhost:3000';
    let authToken;
    let userId;
  
    const generateUniqueEmail = () => {
      const timestamp = new Date().getTime();
      return `deletetest_${timestamp}@deletetest.com`;
    };
  
    const generateUniquePhone = () => {
      const timestamp = new Date().getTime().toString().slice(-9);
      return timestamp.padStart(9, '9');
    };
  
    const patientEmail = generateUniqueEmail();
    const patientPhone = generateUniquePhone();
  
    before(() => {
      // Login como admin para criar o usuário inicial
      cy.request({
        method: 'POST',
        url: `${authUrl}/login`,
        body: {
          email: 'admin@admin.com',
          password: 'Admin123!'
        }
      }).then((response) => {
        const adminToken = response.body.token;
  
        // Criar novo usuário
        return cy.request({
          method: 'POST',
          url: `${userUrl}/register`,
          headers: {
            'Authorization': `Bearer ${adminToken}`,
            'Content-Type': 'application/json'
          },
          body: {
            email: patientEmail,
            password: 'Patient123!',
            userName: `patient_${new Date().getTime()}`,
            role: "Patient"
          }
        });
      }).then((userResponse) => {
        userId = userResponse.body.id;
  
        // Login com o usuário paciente
        return cy.request({
          method: 'POST',
          url: `${authUrl}/login`,
          body: {
            email: patientEmail,
            password: 'Patient123!'
          }
        });
      }).then((loginResponse) => {
        authToken = loginResponse.body.token;
      });
    });
  
    beforeEach(() => {
      cy.clearLocalStorage();
      
      cy.window().then((win) => {
        win.localStorage.setItem('authToken', authToken);
        win.localStorage.setItem('userEmail', patientEmail);
        win.localStorage.setItem('userRole', 'Patient');
      });
  
      // Visitar a página inicial primeiro
      cy.visit(frontendUrl);
  
      // Clicar no botão Patient no menu
      cy.get('button.nav-button').contains('Patient').click();
  
      // Clicar no botão Delete Account na barra de ações
      cy.get('.action-button').contains('Delete Account').click();
  
      // Verificar se chegamos à página correta
      cy.url().should('include', '/patient/delete-account');
    });
  
    it('should display delete account page correctly', () => {
      cy.get('.delete-account-container').should('be.visible');
      cy.get('.delete-account-title').should('contain', 'Delete Account');
      cy.get('.warning-text').should('contain', 'This action cannot be undone');
      cy.get('.delete-button').should('exist');
    });

    it('should handle verification code request process', () => {
      // Update interceptor to match actual API endpoint
      cy.intercept('POST', `${baseUrl}/Patient/account-deletion-request`, {
        statusCode: 200,
        body: { message: 'Verification code sent' }
      }).as('requestDeletion');

      cy.get('.delete-button').click();

      cy.wait('@requestDeletion', { timeout: 10000 });
      
      cy.get('.delete-account-modal-overlay').should('be.visible');
      cy.get('.delete-account-modal').should('be.visible');
    });

    it('should validate 6-digit verification code format', () => {
      // Click delete button and wait for modal
      cy.get('.delete-button').click();
      
      // Wait for API request to complete
      cy.intercept('POST', `${baseUrl}/account-deletion-request`).as('requestDeletion');
      cy.wait('@requestDeletion');

      // Wait for modal to be visible
      cy.get('.delete-account-modal-overlay').should('be.visible');
      cy.get('.delete-account-modal').should('be.visible');

      // Test invalid input (less than 6 digits)
      cy.get('input.token-input').should('be.visible').type('12345');
      cy.get('.modal-buttons button').first().click();
      cy.get('.modal-error-message').should('be.visible')
        .and('contain', 'Please enter the 6-digit verification code');
      
      // Test non-numeric input
      cy.get('input.token-input').clear().type('abc123');
      cy.get('.modal-buttons button').first().click();
      cy.get('.modal-error-message').should('be.visible');
    });

    it('should handle successful account deletion', () => {
      // Update interceptors with correct endpoints
      cy.intercept('POST', `${baseUrl}/Patient/account-deletion-request`, {
        statusCode: 200,
        body: { message: 'Verification code sent' }
      }).as('requestDeletion');

      cy.intercept('DELETE', `${baseUrl}/Patient/confirm-account-deletion`, {
        statusCode: 200,
        body: { message: 'Account deleted successfully' }
      }).as('confirmDeletion');

      cy.get('.delete-button').click();
      cy.wait('@requestDeletion', { timeout: 10000 });

      cy.get('input.token-input').should('be.visible').type('123456');
      cy.get('.modal-buttons button').first().click();
      
      cy.wait('@confirmDeletion', { timeout: 10000 });
    });


    it('should allow canceling the deletion process', () => {
      cy.get('.delete-button').click();
      cy.get('.modal-buttons button').contains('Cancel').click();
      cy.get('.delete-account-modal-overlay').should('not.exist');
    });
  
    afterEach(() => {
      cy.clearLocalStorage();
    });
  
    after(() => {
      // Limpar dados de teste usando o token de admin
      cy.request({
        method: 'POST',
        url: `${authUrl}/login`,
        body: {
          email: 'admin@admin.com',
          password: 'Admin123!'
        }
      }).then((response) => {
        const adminToken = response.body.token;
  
        if (userId) {
          cy.request({
            method: 'DELETE',
            url: `${userUrl}/delete`,
            headers: {
              'Authorization': `Bearer ${adminToken}`
            },
            body: {
              email: patientEmail
            },
            failOnStatusCode: false
          });
        }
      });
    });
  });*/