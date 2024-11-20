describe('Update Patient Profile', () => {
  const baseUrl = 'https://localhost:5001/api/patient';
  const authUrl = 'https://localhost:5001/api/auth';
  const userUrl = 'https://localhost:5001/api/user';
  const frontendUrl = 'http://localhost:3000';
  let authToken;
  let adminToken;
  let patientId;
  let userId;

  // Funções auxiliares para gerar dados únicos
  const generateUniqueEmail = () => {
    const timestamp = new Date().getTime();
    return `patienttest_${timestamp}@patienttestupdate.com`;
  };

  const generateUniquePhone = () => {
    const timestamp = new Date().getTime().toString().slice(-9);
    return timestamp.padStart(9, '9');
  };

  const testPatient = {
    email: generateUniqueEmail(),
    password: "TestPatient123!",
    firstName: "PatientUpdateTest",
    lastName: "User",
    phoneNumber: generateUniquePhone(),
    dateOfBirth: "1990-01-01",
    gender: "Male",
    medicalNr: `MED${new Date().getTime()}`,
    contactInfo: "Test Contact Info",
    emergencyContact: "Emergency Contact Test",
    appointmentHistory: "",
    medicalHistory: ""
  };

  before(() => {
    // Login como admin
    cy.request({
      method: 'POST',
      url: `${authUrl}/login`,
      body: {
        email: 'admin@admin.com',
        password: 'Admin123!'
      }
    }).then((response) => {
      adminToken = response.body.token;
      authToken = adminToken;

      // Criar novo usuário
      return cy.request({
        method: 'POST',
        url: `${userUrl}/register`,
        headers: {
          'Authorization': `Bearer ${adminToken}`,
          'Content-Type': 'application/json'
        },
        body: {
          email: testPatient.email,
          password: testPatient.password,
          userName: `patient_${new Date().getTime()}`,
          role: "Patient"
        }
      });
    }).then((userResponse) => {
      userId = userResponse.body.id;
      testPatient.userId = userId;

      // Criar o perfil do paciente
      return cy.request({
        method: 'POST',
        url: `${baseUrl}`,
        headers: {
          'Authorization': `Bearer ${adminToken}`,
          'Content-Type': 'application/json'
        },
        body: testPatient
      });
    }).then((patientResponse) => {
      patientId = patientResponse.body.id;

      // Login como o paciente
      return cy.request({
        method: 'POST',
        url: `${authUrl}/login`,
        body: {
          email: testPatient.email,
          password: testPatient.password
        }
      });
    }).then((response) => {
      authToken = response.body.token;
    });
  });

  beforeEach(() => {
    cy.wrap(patientId).should('not.be.undefined');
    cy.clearLocalStorage();
    
    // Adicionar email do paciente no localStorage
    cy.window().then((win) => {
      win.localStorage.setItem('authToken', authToken);
      win.localStorage.setItem('userRole', 'Patient');
      win.localStorage.setItem('userEmail', testPatient.email);
    });

    // Interceptar a chamada GET do perfil
    cy.intercept('GET', `${baseUrl}/get-patient-profile*`, {
      statusCode: 200,
      body: testPatient
    }).as('getProfile');

    cy.visit(`${frontendUrl}/patient/update`, {
      onBeforeLoad: (win) => {
        win.localStorage.setItem('authToken', authToken);
        win.localStorage.setItem('userRole', 'Patient');
        win.localStorage.setItem('userEmail', testPatient.email);
      }
    });

    cy.wait('@getProfile');
    cy.get('form', { timeout: 10000 }).should('be.visible');
  });

  it('deve exibir o formulário de atualização com dados existentes', () => {
    cy.get('h2').should('contain', 'Update Profile');
    cy.get('input[name="email"]').should('have.value', testPatient.email);
    cy.get('input[name="phoneNumber"]').should('have.value', testPatient.phoneNumber);
  });

  it('deve atualizar o número de telefone com sucesso', () => {
    const newPhoneNumber = '967654321';

    cy.intercept('PATCH', `${baseUrl}/edit-patient-profile*`).as('updatePatientRequest');

    cy.get('input[name="phoneNumber"]')
      .clear()
      .type(newPhoneNumber);

    cy.get('button[type="submit"]').click();

    cy.wait('@updatePatientRequest', { timeout: 10000 }).then((interception) => {
      expect(interception.response.statusCode).to.equal(200);
      expect(interception.request.body).to.deep.equal([
        { op: 'replace', path: '/phoneNumber', value: newPhoneNumber }
      ]);
    });

    cy.get('.success-message', { timeout: 10000 })
      .should('be.visible')
      .should('contain', 'Profile updated successfully');
  });

  it('deve validar formato inválido de número de telefone', () => {
    const invalidPhone = '12345'; // Menos de 9 dígitos

    cy.get('input[name="phoneNumber"]')
      .clear()
      .type(invalidPhone);

    cy.get('button[type="submit"]').click();

    cy.get('.field-error-message').should('contain', 'Phone number must be exactly 9 digits');
  });

  it('deve atualizar perfil via requisição PATCH direta', () => {
    const patchDoc = [
      { op: 'replace', path: '/phoneNumber', value: '963214567' }
    ];

    cy.request({
      method: 'PATCH',
      url: `${baseUrl}/edit-patient-profile`,
      headers: {
        Authorization: `Bearer ${authToken}`,
        'Content-Type': 'application/json-patch+json'
      },
      body: patchDoc
    }).then((response) => {
      expect(response.status).to.eq(200);
      expect(response.body).to.have.property('phoneNumber', '963214567');
    });
  });

  it('deve mostrar modal quando email é alterado', () => {
    const newEmail = 'newemail@test.com';

    // Configurar o interceptor com a resposta mockada
    cy.intercept('PATCH', `${baseUrl}/edit-patient-profile*`, {
      statusCode: 200,
      body: {
        email: newEmail,
        // outros campos necessários
      }
    }).as('updatePatientRequest');

    cy.get('input[name="email"]')
      .clear()
      .type(newEmail);

    cy.get('button[type="submit"]').click();

    // Aumentar o timeout e verificar a resposta
    cy.wait('@updatePatientRequest', { timeout: 10000 }).then((interception) => {
      expect(interception.response.statusCode).to.equal(200);
    });

    cy.get('.modal', { timeout: 10000 }).should('be.visible');
    cy.get('.modal-content').should('contain', 'Email Change');
  });

  afterEach(() => {
    cy.clearLocalStorage();
  });

  after(() => {
    if (patientId) {
      // Primeiro deletar o paciente
      cy.request({
        method: 'DELETE',
        url: `${baseUrl}/delete/${patientId}`,
        headers: {
          'Authorization': `Bearer ${adminToken}`,
          'Content-Type': 'application/json'
        },
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.be.oneOf([200, 404]);

        // Depois deletar o usuário
        return cy.request({
          method: 'DELETE',
          url: `${authUrl}/delete-user`,
          headers: {
            'Authorization': `Bearer ${adminToken}`,
            'Content-Type': 'application/json'
          },
          body: {
            email: testPatient.email,
            confirmDeletion: true,
            userId: userId
          },
          failOnStatusCode: false
        });
      }).then((response) => {
        expect(response.status).to.be.oneOf([200, 404]);
      });
    }
  });
});
