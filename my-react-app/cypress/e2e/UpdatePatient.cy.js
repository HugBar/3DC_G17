/*describe('Update Patient Form', () => {
    const baseUrl = 'https://localhost:5001/api/patient';
    const frontendUrl = 'http://localhost:3000';
    let authToken;
    let patientId;
  
    // Função para gerar email único
    const generateUniqueEmail = () => {
      const timestamp = new Date().getTime();
      return `patienttest_${timestamp}@testupdate.com`;
    };
  
    // Função para gerar número de telefone único
    const generateUniquePhone = () => {
      const timestamp = new Date().getTime().toString().slice(-9);
      return timestamp.padStart(9, '9');
    };
  
    const patientEmail = generateUniqueEmail();
    const patientPhone = generateUniquePhone();
  
    before(() => {
      // Login como admin
      cy.request({
        method: 'POST',
        url: 'https://localhost:5001/api/auth/login',
        body: {
          email: 'admin@admin.com',
          password: 'Admin123!'
        }
      }).then((response) => {
        authToken = response.body.token;
  
        // Criar paciente de teste
        return cy.request({
          method: 'POST',
          url: baseUrl,
          headers: {
            'Authorization': `Bearer ${authToken}`,
            'Content-Type': 'application/json'
          },
          body: {
            "firstName": "PatientUpdateTest",
            "lastName": "User",
            "email": patientEmail,
            "phoneNumber": patientPhone,
            "dateOfBirth": "1990-01-01",
            "gender": "Male",
            "password": "Patient123!"
          }
        });
      }).then((patientResponse) => {
        patientId = patientResponse.body.id;
        cy.log(`Created patient with email: ${patientEmail} and phone: ${patientPhone}`);
  
        // Login como o paciente criado
        return cy.request({
          method: 'POST',
          url: 'https://localhost:5001/api/auth/login',
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
      });
  
      cy.visit(`${frontendUrl}/patient/update-profile`, {
        onBeforeLoad: (win) => {
          win.localStorage.setItem('authToken', authToken);
        }
      });
  
      cy.reload();
      cy.get('form', { timeout: 10000 }).should('be.visible');
    });
  
    it('should display update form with existing data', () => {
      cy.get('h2').should('contain', 'Update Profile');
      cy.get('input[name="email"]').should('have.value', patientEmail);
      cy.get('input[name="phoneNumber"]').should('have.value', patientPhone);
    });
  
    it('should successfully update patient phone number', () => {
      const updatedPhone = '987654321';
  
      cy.intercept('PATCH', `${baseUrl}/edit-patient-profile`).as('updatePatientRequest');
  
      cy.get('input[name="phoneNumber"]')
        .clear()
        .type(updatedPhone);
  
      cy.get('button[type="submit"]').click();
  
      cy.wait('@updatePatientRequest').then((interception) => {
        expect(interception.response.statusCode).to.equal(200);
      });
  
      cy.get('.success-message').should('contain', 'Profile updated successfully');
    });
  
    it('should handle network errors gracefully', () => {
      cy.intercept('PATCH', `${baseUrl}/edit-patient-profile`, {
        statusCode: 500,
        body: { message: 'Internal Server Error' }
      }).as('updatePatientError');
  
      cy.get('input[name="phoneNumber"]')
        .clear()
        .type('987654321');
        
      cy.get('button[type="submit"]').click();
  
      cy.get('.error-message').should('contain', 'Failed to update profile');
    });
  
    it('should validate email format', () => {
      cy.get('input[name="email"]')
        .clear()
        .type('invalid-email');
  
      cy.get('button[type="submit"]').click();
  
      cy.get('.field-error-message')
        .should('be.visible')
        .and('contain', 'Please enter a valid email');
    });
  
    it('should validate phone number format', () => {
      cy.get('input[name="phoneNumber"]')
        .clear()
        .type('12345');
  
      cy.get('button[type="submit"]').click();
  
      cy.get('.field-error-message')
        .should('be.visible')
        .and('contain', 'Phone number must be exactly 9 digits');
    });
  
    it('should show email change confirmation modal', () => {
      const newEmail = 'newemail@test.com';
  
      cy.get('input[name="email"]')
        .clear()
        .type(newEmail);
  
      cy.get('button[type="submit"]').click();
  
      cy.get('.modal')
        .should('be.visible')
        .within(() => {
          cy.contains('Email Change').should('be.visible');
          cy.contains('You will need to login again').should('be.visible');
        });
    });
  
    afterEach(() => {
      cy.clearLocalStorage();
    });
  
    after(() => {
      if (patientId) {
        cy.request({
          method: 'POST',
          url: 'https://localhost:5001/api/auth/login',
          body: {
            email: 'admin@admin.com',
            password: 'Admin123!'
          }
        }).then((response) => {
          const adminToken = response.body.token;
          
          cy.request({
            method: 'DELETE',
            url: `${baseUrl}/${patientId}`,
            headers: {
              'Authorization': `Bearer ${adminToken}`
            }
          }).then((response) => {
            expect(response.status).to.eq(200);
          });
        });
      }
    });
  });*/