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
      cy.get('input[name="patientMRN"]').should('exist');
      cy.get('input[name="doctorLicenseNumber"]').should('exist');
      cy.get('label').contains('Operation Type:').should('exist');
      cy.get('input[name="deadline"]').should('exist');
      cy.get('select[name="priority"]').should('exist');
    });
  
    it('should successfully create a new operation request', () => {
      const requestData = {
        patientMRN: 'MRN123456',
        doctorLicenseNumber: 'LIC789012',
        operationType: 'Brain Surgery (Version 1)',
        deadline: '2024-12-01T10:00',
        priority: 'urgent'
      };
  
      // Wait for operation types to load
      cy.contains('label', 'Operation Type:')
        .parent()
        .find('select')
        .should('exist')
        .and('not.be.disabled');
  
      cy.intercept('POST', `${baseUrl}/create-operation-request`, {
        headers: {
          'Authorization': `Bearer ${authToken}`
        }
      }).as('createRequestRequest');
  
      cy.get('input[name="patientMRN"]').type(requestData.patientMRN);
      cy.get('input[name="doctorLicenseNumber"]').type(requestData.doctorLicenseNumber);
      cy.contains('label', 'Operation Type:')
        .parent()
        .find('select')
        .select(requestData.operationType);
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
      cy.get('input[name="patientMRN"]').should('have.value', '');
      cy.get('input[name="doctorLicenseNumber"]').should('have.value', '');
      cy.get('select[name="priority"]').should('have.value', 'elective');
    });
    
    it('should handle network errors gracefully', () => {
      cy.contains('label', 'Operation Type:')
        .parent()
        .find('select')
        .should('exist')
        .and('not.be.disabled');
      
      cy.intercept('POST', `${baseUrl}/create-operation-request`, {
        statusCode: 500,
        body: { message: 'Internal Server Error' }
      }).as('createRequestError');
  
      cy.get('input[name="patientMRN"]').type('P123');
      cy.get('input[name="doctorLicenseNumber"]').type('D456');
      cy.contains('label', 'Operation Type:')
        .parent()
        .find('select')
        .select('Brain Surgery (Version 1)');
      cy.get('input[name="deadline"]')
        .invoke('removeAttr', 'disabled')
        .type('2024-12-01T10:00');
      cy.get('select[name="priority"]').select('urgent');
  
      cy.get('button[type="submit"]').click();
  
      cy.get('.error-message').should('contain', 'Error creating operation request');
    });
  
    it('should handle validation errors', () => {
      cy.contains('label', 'Operation Type:')
        .parent()
        .find('select')
        .should('exist')
        .and('not.be.disabled');
      
      cy.intercept('POST', `${baseUrl}/create-operation-request`, {
        statusCode: 400,
        body: { message: 'Invalid patient medical record number' }
      }).as('createRequestValidationError');
  
      cy.get('input[name="patientMRN"]').type('invalid-mrn');
      cy.get('input[name="doctorLicenseNumber"]').type('LIC789012');
      cy.contains('label', 'Operation Type:')
        .parent()
        .find('select')
        .select('Brain Surgery (Version 1)');
      cy.get('input[name="deadline"]').type('2024-12-01T10:00');
      cy.get('select[name="priority"]').select('urgent');
  
      cy.get('button[type="submit"]').click();
  
      cy.get('.error-message')
        .should('be.visible')
        .and('contain', 'Error creating operation request');
  
      cy.get('input[name="patientMRN"]').should('have.value', 'invalid-mrn');
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