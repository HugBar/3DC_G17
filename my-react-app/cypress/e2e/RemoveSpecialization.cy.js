describe('Remove Specialization', () => {
    let specializationId;
    let authToken;
    const authUrl = 'https://localhost:5001/api'; 
    const baseUrl = 'http://localhost:3001/api'; 
    const frontendUrl = 'http://localhost:3000';

    before(() => {
        cy.request({
            method: 'POST',
            url: `${authUrl}/auth/login`,
            body: {
                email: 'admin@admin.com',
                password: 'Admin123!'
            }
        }).then((response) => {
            authToken = response.body.token;
            
            return cy.request({
                method: 'POST',
                url: `${baseUrl}/specializations`,
                headers: {
                    'Authorization': `Bearer ${authToken}`,
                    'Content-Type': 'application/json'
                },
                body: {
                    name: 'Test Specialization',
                    description: 'Test Description for Deletion'
                }
            });
        }).then((response) => {
            specializationId = response.body._id;
            cy.log('Created specialization with ID:', specializationId);
        });
    });

    beforeEach(() => {
        cy.clearLocalStorage();
        cy.window().then((win) => {
            win.localStorage.setItem('authToken', authToken);
        });
        cy.visit(`${frontendUrl}/specializations/search`, {
            onBeforeLoad: (win) => {
                win.localStorage.setItem('authToken', authToken);
            }
        });
    });

    it('displays delete confirmation modal', () => {
        // Click first specialization card
        cy.get('.specialization-card')
            .first()
            .should('be.visible')
            .click();

        cy.get('.specialization-details-modal')
            .should('be.visible')
            .find('.delete-button')
            .click();

        cy.get('.delete-specialization-container')
            .should('be.visible');

        cy.get('.modal-content')
            .should('contain', 'Delete Specialization')
            .and('contain', 'Are you sure you want to delete this specialization?');

        cy.get('.confirm-button')
            .should('be.visible')
            .and('contain', 'Yes, Delete');

        cy.get('.cancel-button')
            .should('be.visible')
            .and('contain', 'Cancel');
    });

    it('successfully deletes specialization when confirmed', () => {
        cy.get('.specialization-card')
            .first()
            .should('be.visible')
            .click();

        cy.get('.specialization-details-modal')
            .should('be.visible')
            .find('.delete-button')
            .click();

        cy.get('.confirm-button').click();

        cy.get('.success-message', { timeout: 5000 })
            .should('be.visible')
            .and('contain', 'Specialization successfully deleted');

        cy.url().should('include', '/specializations/search');
    });

    it('returns to search page when canceling deletion', () => {
        cy.get('.specialization-card')
            .first()
            .should('be.visible')
            .click();

        cy.get('.specialization-details-modal')
            .should('be.visible')
            .find('.delete-button')
            .click();

        cy.get('.cancel-button').click();

        cy.url().should('include', '/specializations/search');
    });

    afterEach(() => {
        cy.clearLocalStorage();
    });
});