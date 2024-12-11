describe('Update Specialization', () => {
    const baseUrl = 'http://localhost:3001/api';
    const frontendUrl = 'http://localhost:3000';
    let authToken;
    let testSpecialization;

    before(() => {
        // Login to get the token
        cy.request({
            method: 'POST',
            url: 'https://localhost:5001/api/auth/login',
            body: {
                email: 'admin@admin.com',
                password: 'Admin123!'
            }
        }).then((response) => {
            authToken = response.body.token;
            
            // Create test specialization after getting token
            return cy.request({
                method: 'POST',
                url: `${baseUrl}/specializations`,
                headers: { Authorization: `Bearer ${authToken}` },
                body: {
                    name: 'TEST CYPRESS',
                    description: 'Test Description'
                }
            });
        }).then((createResponse) => {
            testSpecialization = createResponse.body.specialization;
        });
    });

    beforeEach(() => {
        cy.clearLocalStorage();
        
        cy.window().then((win) => {
            win.localStorage.setItem('authToken', authToken);
        });

        // Visit the update specialization page
        cy.visit(`${frontendUrl}/specialization/update/${encodeURIComponent(testSpecialization.name)}`, {
            onBeforeLoad: (win) => {
                win.localStorage.setItem('authToken', authToken);
            }
        });

        cy.reload();
        cy.get('.update-specialization-container', { timeout: 10000 }).should('be.visible');
    });

    it('displays update specialization form with existing data', () => {
        cy.get('h2').should('contain', 'Update Specialization');
        cy.get('#name').should('have.value', testSpecialization.name);
        cy.get('#description').should('have.value', testSpecialization.description);
        cy.get('button[type="submit"]').should('contain', 'Update Specialization');
    });

    it('successfully updates a specialization', () => {
        cy.intercept('PUT', `${baseUrl}/specializations/${testSpecialization._id}`).as('updateSpecialization');

        const updatedData = {
            name: 'Updated Test Specialization',
            description: 'Updated Test Description'
        };

        cy.get('#name').clear().type(updatedData.name);
        cy.get('#description').clear().type(updatedData.description, { force: true });
        cy.get('button[type="submit"]').click();

        cy.wait('@updateSpecialization').then((interception) => {
            expect(interception.response.statusCode).to.equal(200);
        });

        cy.get('.success-message')
            .should('be.visible')
            .and('contain', 'Specialization updated successfully!');
    });


    it('validates required fields', () => {
        cy.get('#name').clear();
        cy.get('#description').clear();
        cy.get('button[type="submit"]').click();

        cy.get('#name')
            .invoke('prop', 'validationMessage')
            .should('not.be.empty');

        cy.get('#description')
            .invoke('prop', 'validationMessage')
            .should('not.be.empty');
    });

    it('handles non-existent specialization', () => {
        cy.visit(`${frontendUrl}/specialization/update/${encodeURIComponent('NonExistentSpecialization')}`, {
            onBeforeLoad: (win) => {
                win.localStorage.setItem('authToken', authToken);
            }
        });

        cy.get('.error-message')
            .should('be.visible')
            .and('contain', 'Specialization not found');
    });

    afterEach(() => {
        cy.clearLocalStorage();
    });

    after(() => {
        // Clean up - delete test specialization
        if (testSpecialization?._id) {
            cy.request({
                method: 'DELETE',
                url: `${baseUrl}/specializations/${testSpecialization._id}`,
                headers: { Authorization: `Bearer ${authToken}` }
            });
        }
    });
}); 