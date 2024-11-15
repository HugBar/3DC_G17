/*describe('Update Patient Form', () => {
  const baseUrl = 'https://localhost:5001/api/patient';
  const authUrl = 'https://localhost:5001/api/auth';
  const userUrl = 'https://localhost:5001/api/user';
  const frontendUrl = 'http://localhost:3000';
  let authToken;
  let adminToken;
  let patientId;
  let userId;

  // Função para gerar email único
  const generateUniqueEmail = () => {
    const timestamp = new Date().getTime();
    return `patienttest_${timestamp}@patienttest.com`;
  };

  // Função para gerar telefone único
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
      adminToken = response.body.token;

      // Criar novo usuário usando o endpoint correto
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

      // Criar perfil de paciente usando o token do admin
      return cy.request({
        method: 'POST',
        url: `${baseUrl}`,
        headers: {
          'Authorization': `Bearer ${adminToken}`,
          'Content-Type': 'application/json'
        },
        body: {
          userId: userId,
          firstName: "PatientUpdateTest",
          lastName: "User",
          email: patientEmail,
          phoneNumber: patientPhone,
          dateOfBirth: "1990-01-01",
          gender: "Male",
          contactInfo: "Test Address",
          emergencyContact: "Test Emergency Contact",
          medicalHistory: "Test Medical History"
        }
      });
    }).then((patientResponse) => {
      patientId = patientResponse.body.id;

      // Login com o usuário paciente para obter o token para os testes
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
    });

    // Interceptar a chamada GET com o email como query parameter
    cy.intercept('GET', `${baseUrl}/get-patient-profile?email=${patientEmail}`).as('getProfile');

    cy.visit(`${frontendUrl}/patient/update`);
    cy.url().should('include', `/patient/update`);

    // Esperar pela chamada GET inicial
    cy.wait('@getProfile').then((interception) => {
      expect(interception.response.statusCode).to.equal(200);
    });
    
    cy.get('form').should('exist');
  });

  it('deve atualizar apenas campos preenchidos', () => {
    const newPhone = generateUniquePhone();

    // Interceptar com o ID correto
    cy.intercept('PATCH', `${baseUrl}/edit-patient-profile/${patientId}`).as('updateProfile');

    cy.get('input[name="phoneNumber"]').type(newPhone);
    cy.get('button[type="submit"]').click();

    cy.wait('@updateProfile').then((interception) => {
      expect(interception.request.body).to.deep.equal({
        phoneNumber: newPhone
      });
      expect(interception.response.statusCode).to.equal(200);
    });

    cy.get('.success-message').should('contain', 'Profile updated successfully');
  });

  it('deve exibir erro quando email é inválido', () => {
    cy.get('input[name="email"]')
      .clear()
      .type('invalid-email');

    cy.get('button[type="submit"]').click();
    cy.get('.field-error-message').should('contain', 'Please enter a valid email.');
  });

  it('deve exibir erro quando número de telefone é inválido', () => {
    cy.get('input[name="phoneNumber"]')
      .clear()
      .type('12345');

    cy.get('button[type="submit"]').click();
    cy.get('.field-error-message').should('contain', 'Phone number must be exactly 9 digits.');
  });

  it('deve mostrar modal quando email é alterado', () => {
    const newEmail = generateUniqueEmail();
    
    cy.intercept('PATCH', `${baseUrl}/edit-patient-profile/${patientId}`).as('updateProfile');
    
    cy.get('input[name="email"]')
      .clear()
      .type(newEmail);
      
    cy.get('button[type="submit"]').click();
    
    cy.wait('@updateProfile');
    cy.get('.modal').should('be.visible');
  });

  it('deve redirecionar para login após confirmação de mudança de email', () => {
    const newEmail = generateUniqueEmail();

    cy.intercept('PATCH', `${baseUrl}/edit-patient-profile/${patientId}`).as('updateProfile');

    cy.get('input[name="email"]')
      .clear()
      .type(newEmail);

    cy.get('button[type="submit"]').click();
    
    cy.wait('@updateProfile');
    cy.get('.modal button').click();

    cy.url().should('include', '/login');
  });

  it('deve tratar erros de rede adequadamente', () => {
    cy.intercept('PATCH', `${baseUrl}/edit-patient-profile/${patientId}`, {
      statusCode: 500,
      body: { message: 'Internal Server Error' }
    }).as('updatePatientError');

    cy.get('input[name="phoneNumber"]')
      .clear()
      .type(generateUniquePhone());

    cy.get('button[type="submit"]').click();

    cy.get('.error-message').should('contain', 'Failed to update profile');
  });

  it('deve exibir mensagem quando nenhuma alteração é detectada', () => {
    cy.get('button[type="submit"]').click();
    cy.get('.error-message').should('contain', 'No changes detected');
  });

  afterEach(() => {
    cy.clearLocalStorage();
  });

  after(() => {
    // Garantir que estamos usando o token de admin para limpeza
    cy.request({
      method: 'POST',
      url: `${authUrl}/login`,
      body: {
        email: 'admin@admin.com',
        password: 'Admin123!'
      }
    }).then((response) => {
      const adminToken = response.body.token;

      // Deletar o perfil do paciente
      if (patientId) {
        cy.request({
          method: 'DELETE',
          url: `${baseUrl}/${patientId}`,
          headers: {
            'Authorization': `Bearer ${adminToken}`
          },
          failOnStatusCode: false
        });
      }

      // Deletar o usuário
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