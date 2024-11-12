describe('Deactivate Staff', () => {
    const baseUrl = 'https://localhost:5001/api/staff';
    const frontendUrl = 'http://localhost:3000';
    let authToken;
    let staffId;

    // Gerar um email dinâmico para evitar conflitos
    const uniqueEmail = `stafftest${Date.now()}@test.com`;
    const staffPhone = "961234567";

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

            // Criar staff de teste para desativar
            return cy.request({
                method: 'POST',
                url: `${baseUrl}/create-staff-profile`,
                headers: {
                    'Authorization': `Bearer ${authToken}`,
                    'Content-Type': 'application/json'
                },
                body: {
                    "firstName": "StaffDeactivateTest",
                    "lastName": "User",
                    "email": uniqueEmail, // Usar o email dinâmico
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
        cy.clearLocalStorage();
        cy.window().then((win) => {
            win.localStorage.setItem('authToken', authToken);
        });

        cy.visit(`${frontendUrl}/staff/deactivate/${staffId}`, {
            onBeforeLoad: (win) => {
                win.localStorage.setItem('authToken', authToken);
            }
        });

        cy.reload();
        cy.get('.deactivate-staff-container', { timeout: 10000 }).should('be.visible');
    });

    it('should display the deactivate staff page', () => {
        cy.get('h2').should('contain', 'Deactivate Staff');
        cy.get('.deactivate-button').should('exist');
        cy.get('.back-button').should('exist');
    });

    it('should successfully deactivate staff member', () => {
        cy.intercept('PATCH', `${baseUrl}/deactivate/${staffId}`).as('deactivateRequest');

        cy.get('.deactivate-button').click();

        cy.wait('@deactivateRequest').then((interception) => {
            expect(interception.response.statusCode).to.equal(200);
        });

        cy.get('.success-message').should('contain', 'Staff member successfully deactivated');
    });

    it('should handle network errors gracefully', () => {
        cy.intercept('PATCH', `${baseUrl}/deactivate/${staffId}`, {
            statusCode: 500,
            body: { message: 'Internal Server Error' }
        }).as('deactivateError');

        cy.get('.deactivate-button').click();

        cy.get('.error-message').should('contain', 'Error deactivating staff member');
    });

    it('should navigate back when clicking back button', () => {
        cy.get('.back-button').click();
        cy.url().should('not.include', '/staff/deactivate');
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
                }
            }).then((response) => {
                expect(response.status).to.eq(200);
            });
        }
    });
});
