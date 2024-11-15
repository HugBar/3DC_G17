describe('Delete Account', () => {
    const baseUrl = 'https://localhost:5001/api/patient';
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
  
    it('deve exibir a página de exclusão de conta corretamente', () => {
      cy.get('.delete-account-title').should('contain', 'Delete Account');
      cy.get('.warning-text').should('contain', 'Warning: This action cannot be undone');
      cy.get('.delete-button').should('exist');
    });
  
    it('deve exibir erro quando token não é fornecido', () => {
      cy.intercept('POST', `${baseUrl}/account-deletion-request`, {
        statusCode: 200,
        body: { message: 'Confirmation email sent' }
      }).as('requestDeletion');

      cy.get('.delete-button').click();
      cy.wait('@requestDeletion');

      cy.get('.modal').should('be.visible');
      cy.get('.modal-title').should('contain', 'Enter Confirmation Token');

      cy.get('form.token-form button[type="submit"]').click();
      
      cy.get('.modal-error-message')
        .should('be.visible')
        .and('contain', 'Please enter the confirmation token from your email.');
    });
  
    it('deve exibir erro quando token é inválido', () => {
      cy.intercept('POST', `${baseUrl}/account-deletion-request`, {
        statusCode: 200,
        body: { message: 'Confirmation email sent' }
      }).as('requestDeletion');

      cy.intercept('DELETE', `${baseUrl}/confirm-account-deletion`, {
        statusCode: 400,
        body: { message: 'Invalid token' }
      }).as('invalidToken');

      cy.get('.delete-button').click();
      cy.wait('@requestDeletion');

      // Verificar se o modal aparece
      cy.get('.modal').should('be.visible');
      cy.get('.modal-title').should('contain', 'Enter Confirmation Token');

      // Tentar com token inválido
      cy.get('.token-input').should('be.visible').type('ajaa');
      cy.get('form.token-form button[type="submit"]').click();

      // Verificar mensagem de erro
      cy.get('.modal-error-message')
        .should('be.visible')
        .and('contain', 'Invalid token or deletion failed. Please try again.');
    });
  
    it('deve permitir cancelar o processo de exclusão', () => {
      cy.intercept('POST', `${baseUrl}/account-deletion-request`, {
        statusCode: 200,
        body: { message: 'Confirmation email sent' }
      }).as('requestDeletion');

      cy.get('.delete-button').click();
      cy.wait('@requestDeletion');

      cy.get('.modal').should('be.visible');
      cy.get('.modal-title').should('contain', 'Enter Confirmation Token');

      cy.get('.modal-buttons button').contains('Cancel').click();
      
      // Verificar se voltou para a página de delete account
      cy.url().should('include', '/patient/delete-account');
      cy.get('.delete-account-title').should('be.visible');
      cy.get('.delete-button').should('be.visible');
      cy.get('.modal').should('not.exist');
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
  });