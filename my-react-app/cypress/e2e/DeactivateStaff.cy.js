const baseUrl = 'https://localhost:5001/api/staff';
const frontendUrl = 'http://localhost:3000';
let authToken;
let staffId;

// Função para gerar email único
const generateUniqueEmail = () => {
    const timestamp = new Date().getTime();
    return `stafftest_${timestamp}@stafftestdeactivate.com`;
};

const generateUniquePhone = () => {
    const timestamp = new Date().getTime().toString().slice(-9);
    return timestamp.padStart(9, '9');
};

const staffEmail = generateUniqueEmail();
const staffPhone = generateUniquePhone();

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

        // Criar o usuário
        return cy.request({
            method: 'POST',
            url: 'https://localhost:5001/api/user/register',
            headers: {
                'Authorization': `Bearer ${authToken}`,
                'Content-Type': 'application/json'
            },
            body: {
                username: `stafftest_${new Date().getTime()}`,
                email: staffEmail,
                password: "StaffTest123!",
                role: "Nurse"
            }
        });
    }).then(() => {
        // Criar o perfil do funcionário
        return cy.request({
            method: 'POST',
            url: `${baseUrl}/create-staff-profile`,
            headers: {
                'Authorization': `Bearer ${authToken}`,
                'Content-Type': 'application/json'
            },
            body: {
                "firstName": "StaffUpdateTest",
                "lastName": "User",
                "email": staffEmail,
                "phoneNumber": staffPhone,
                "specialization": "Nurse",
                "availabilitySlots": [
                    {
                        "startTime": "2024-11-23T08:00:00",
                        "endTime": "2024-11-23T11:00:00"
                    }
                ]
            }
        });
    }).then((staffResponse) => {
        staffId = staffResponse.body.id;
    });
});

beforeEach(() => {
    cy.wrap(staffId).should('not.be.undefined');
    cy.clearLocalStorage();
    cy.window().then((win) => {
        win.localStorage.setItem('authToken', authToken);
    });

    cy.visit(`${frontendUrl}/staff/deactivate/${staffId}`);
    cy.get('.deactivate-staff-container', { timeout: 10000 }).should('be.visible');
});

it('should display the deactivate staff page correctly', () => {
    cy.get('h2').should('contain', 'Deactivate Staff');
    cy.get('.deactivate-button').should('exist').and('be.visible');
    cy.get('.back-button').should('exist').and('be.visible');
});

it('should successfully deactivate staff member', () => {
    // Configurar interceptação antes de qualquer ação
    cy.intercept('PATCH', `${baseUrl}/${staffId}/deactivate`).as('deactivateRequest');
    
    // Verificar se o staffId está presente
    cy.wrap(staffId).should('not.be.undefined');
    
    // Clicar no botão de desativar
    cy.get('.deactivate-button').click();
    
    // Esperar pela requisição e verificar a resposta
    cy.wait('@deactivateRequest', { timeout: 15000 })
        .its('response.statusCode')
        .should('equal', 200);

    // Verificar mensagem de sucesso
    cy.get('.success-message')
        .should('be.visible')
        .and('contain', 'Staff member successfully deactivated');
});

it('should handle network errors gracefully', () => {
    // Configurar interceptação com erro
    cy.intercept('PATCH', `${baseUrl}/${staffId}/deactivate`, {
        statusCode: 500,
        body: { message: 'Internal Server Error' },
        delay: 100
    }).as('deactivateError');

    // Clicar no botão de desativar
    cy.get('.deactivate-button').click();

    // Verificar mensagem de erro
    cy.get('.error-message')
        .should('be.visible')
        .and('contain', 'Error deactivating staff member');
});

afterEach(() => {
    cy.clearLocalStorage();
});

after(() => {
    if (staffId) {
        cy.request({
            method: 'DELETE',
            url: `${baseUrl}/${staffId}`,
            headers: {
                'Authorization': `Bearer ${authToken}`
            },
            failOnStatusCode: false
        }).then((response) => {
            expect(response.status).to.be.oneOf([200, 404]);
        });

        if (staffEmail) {
            cy.request({
                method: 'DELETE',
                url: 'https://localhost:5001/api/user/delete',
                headers: {
                    'Authorization': `Bearer ${authToken}`
                },
                body: {
                    email: staffEmail
                },
                failOnStatusCode: false
            });
        }
    }
});