describe('Operation Request Form', () => {
    const baseUrl = 'https://localhost:5001/api/OperationRequest';
    const frontendUrl = 'http://localhost:3000';
    let authToken;
    let createdRequestId;
  
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
      
      cy.window().then((win) => {
        win.localStorage.clear();
        win.localStorage.setItem('authToken', authToken);
      });
  
      cy.visit(`${frontendUrl}/operation/request`, {
        onBeforeLoad: (win) => {
          win.localStorage.setItem('authToken', authToken);
        }
      });
  
      cy.reload();
      cy.get('form', { timeout: 10000 }).should('be.visible');
    });
  
    it('should display the create operation request form', () => {
      cy.get('h2').should('contain', 'Create Operation Request');
      cy.get('input[name="patientId"]').should('exist');
      cy.get('input[name="doctorId"]').should('exist');
      cy.get('input[name="operationTypeId"]').should('exist');
      cy.get('input[name="deadline"]').should('exist');
      cy.get('select[name="priority"]').should('exist');
    });
  
    it('should successfully create a new operation request', () => {
      const requestData = {
        patientId: 'b99fd2b1-20d1-4aeb-879e-e2d5f4debeda',
        doctorId: '0c40019b-1e02-42d3-9da8-0138c9825c17',
        operationTypeId: 'c86233e0-82ca-499b-af0b-854ed601fe5a',
        deadline: '2024-12-01T10:00',
        priority: 'urgent'
      };
  
      cy.intercept('POST', `${baseUrl}/create-operation-request`, {
        headers: {
          'Authorization': `Bearer ${authToken}`
        }
      }).as('createRequestRequest');
  
      cy.get('input[name="patientId"]').type(requestData.patientId);
      cy.get('input[name="doctorId"]').type(requestData.doctorId);
      cy.get('input[name="operationTypeId"]').type(requestData.operationTypeId);
      cy.get('input[name="deadline"]')
        .invoke('removeAttr', 'disabled')
        .type(requestData.deadline);
      cy.get('select[name="priority"]').select(requestData.priority);
  
      cy.get('button[type="submit"]').click();
  
      cy.wait('@createRequestRequest').then((interception) => {
        createdRequestId = interception.response.body.id;
      });
  
      cy.get('.success-message').should('contain', 'Operation request created successfully!');
  
      // Verify form is cleared
      cy.get('input[name="patientId"]').should('have.value', '');
      cy.get('input[name="doctorId"]').should('have.value', '');
      cy.get('input[name="operationTypeId"]').should('have.value', '');
    });
  
    it('should handle network errors gracefully', () => {
      cy.intercept('POST', `${baseUrl}/create-operation-request`, {
        statusCode: 500,
        body: { message: 'Internal Server Error' }
      }).as('createRequestError');
  
      cy.get('input[name="patientId"]').type('P123');
      cy.get('input[name="doctorId"]').type('D456');
      cy.get('input[name="operationTypeId"]').type('OT789');
      cy.get('input[name="deadline"]')
        .invoke('removeAttr', 'disabled')
        .type('2024-12-01T10:00');
      cy.get('select[name="priority"]').select('urgent');
  
      cy.get('button[type="submit"]').click();
  
      cy.get('.error-message').should('contain', 'Error creating operation request');
    });
  
    it('should handle validation errors', () => {
      cy.intercept('POST', `${baseUrl}/create-operation-request`, {
        statusCode: 400,
        body: { message: 'Invalid operation type ID' }
      }).as('createRequestValidationError');
  
      cy.get('input[name="patientId"]').type('P123');
      cy.get('input[name="doctorId"]').type('D456');
      cy.get('input[name="operationTypeId"]').type('invalid-id');
      cy.get('input[name="deadline"]').type('2024-12-01T10:00');
      cy.get('select[name="priority"]').select('urgent');
  
      cy.get('button[type="submit"]').click();
  
      cy.get('.error-message')
        .should('be.visible')
        .and('contain', 'Error creating operation request');
  
      // Verify the form data is preserved
      cy.get('input[name="operationTypeId"]').should('have.value', 'invalid-id');
    });
  
    afterEach(() => {
      cy.clearLocalStorage();
    });
  
    after(() => {
        // Clean up created operation request after all tests
        if (createdRequestId) {
          cy.request({
            method: 'DELETE',
            url: `${baseUrl}/delete-operation-request/${createdRequestId}`,
            headers: {
              'Authorization': `Bearer ${authToken}`
            }
          }).then((response) => {
            expect(response.status).to.eq(200);
          });
        }
      });
  });