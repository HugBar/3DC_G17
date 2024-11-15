const baseUrl = 'https://localhost:5001/api/patient';
const authUrl = 'https://localhost:5001/api/auth';
const userUrl = 'https://localhost:5001/api/user';
const frontendUrl = 'http://localhost:3000';
let authToken;
let patientId;
let userId;

const generateUniqueEmail = () => {
  const timestamp = new Date().getTime();
  return `patienttest_${timestamp}@patienttestdelete.com`;
};

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
    url: `${authUrl}/login`,
    body: {
      email: 'admin@admin.com',
      password: 'Admin123!'
    }
  }).then((response) => {
    const adminToken = response.body.token;
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
        email: patientEmail,
        password: 'Patient123!',
        userName: `patient_${new Date().getTime()}`,
        role: "Patient"
      }
    });
  }).then((userResponse) => {
    userId = userResponse.body.id;

    // Criar o perfil do paciente
    return cy.request({
      method: 'POST',
      url: `${baseUrl}`,
      headers: {
        'Authorization': `Bearer ${authToken}`,
        'Content-Type': 'application/json'
      },
      body: {
        firstName: "PatientDeleteTest",
        lastName: "User",
        email: patientEmail,
        phoneNumber: patientPhone,
        dateOfBirth: "1990-01-01",
        gender: "Male",
        medicalNr: `MED${new Date().getTime()}`,
        contactInfo: "Test Contact Info",
        emergencyContact: "Emergency Contact Test",
        userId: userId,
        appointmentHistory: "",
        medicalHistory: ""
      }
    });
  }).then((patientResponse) => {
    patientId = patientResponse.body.id;
  });
});

beforeEach(() => {
  cy.wrap(patientId).should('not.be.undefined');
  cy.clearLocalStorage();
  cy.window().then((win) => {
    win.localStorage.setItem('authToken', authToken);
  });

  // Navegar para a lista de pacientes
  cy.visit(`${frontendUrl}/patient/list`, { timeout: 25000 });
  cy.intercept('GET', `${baseUrl}/filter*`).as('getPatients');
  cy.wait('@getPatients', { timeout: 25000 });
});

// Primeiro teste - verificar detalhes
it('should display patient details and delete option', () => {
  cy.log('Starting test with patientId:', patientId);

  cy.contains('.patient-card', patientEmail, { timeout: 25000 })
    .should('exist')
    .and('be.visible')
    .click();

  cy.get('.patient-details-modal', { timeout: 25000 })
    .should('be.visible');

  cy.get('.delete-button', { timeout: 25000 })
    .should('exist')
    .and('be.visible');
});

// Segundo teste - cancelar exclusão
it('should return to list when canceling deletion', () => {
  cy.contains('.patient-card', patientEmail, { timeout: 25000 })
    .should('be.visible')
    .click();

  cy.get('.delete-button', { timeout: 25000 })
    .should('be.visible')
    .click();

  cy.get('.cancel-button', { timeout: 25000 })
    .should('be.visible')
    .click();

  cy.url().should('include', '/patient/list');
});

// Último teste - excluir paciente
it('should successfully delete patient when confirmed', () => {
  cy.intercept('DELETE', `${baseUrl}/${patientId}`).as('deleteRequest');

  cy.contains('.patient-card', patientEmail, { timeout: 25000 })
    .should('be.visible')
    .click();

  cy.get('.delete-button', { timeout: 25000 })
    .should('be.visible')
    .click();

  cy.get('.confirm-button', { timeout: 25000 })
    .should('be.visible')
    .click();

});

afterEach(() => {
  cy.clearLocalStorage();
});

after(() => {
  if (patientId) {
    // Limpar dados de teste
    cy.request({
      method: 'DELETE',
      url: `${userUrl}/delete`,
      headers: {
        'Authorization': `Bearer ${authToken}`
      },
      body: {
        email: patientEmail
      },
      failOnStatusCode: false
    });
  }
});