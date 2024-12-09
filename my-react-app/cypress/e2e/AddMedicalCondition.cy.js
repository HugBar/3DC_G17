describe('Add Medical Condition', () => {
    const baseUrl = 'http://localhost:3001';
    const frontendUrl = 'http://localhost:3000';
    let authToken;

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
        });
    });

    beforeEach(() => {
        cy.clearLocalStorage();

        cy.window().then((win) => {
            win.localStorage.setItem('authToken', authToken);
        });

        // Visit the add medical condition page
        cy.visit(`${frontendUrl}/medical-condition/add`, {
            onBeforeLoad: (win) => {
                win.localStorage.setItem('authToken', authToken);
            }
        });

        cy.reload();
        cy.get('.add-medical-condition-container', { timeout: 10000 }).should('be.visible');
    });

    it('displays add medical condition form correctly', () => {
        cy.get('h2').should('contain', 'Add New Medical Condition');
        cy.get('#name').should('exist');
        cy.get('#severity').should('exist');
        cy.get('#description').should('exist');
        cy.get('button[type="submit"]').should('contain', 'Add Medical Condition');
    });

    it('successfully adds a new medical condition', () => {
        cy.intercept('POST', `${baseUrl}/medical-conditions/add-medical-condition`).as('addMedicalCondition');

        cy.get('#name').type('Test Condition');
        cy.get('#severity').select('Medium');
        cy.get('#description').type('Test Description');
        cy.get('button[type="submit"]').click();

        cy.wait('@addMedicalCondition').then((interception) => {
            expect(interception.response.statusCode).to.be.oneOf([200, 201]);
        });

        cy.get('.success-message')
            .should('be.visible')
            .and('contain', 'Medical condition added successfully!');
    });

    it('handles network errors gracefully', () => {
        cy.intercept('POST', `${baseUrl}/medical-conditions/add-medical-condition`, {
            statusCode: 500,
            body: { message: 'Internal Server Error' }
        }).as('addMedicalConditionError');

        cy.get('#name').type('Test Condition');
        cy.get('#severity').select('High');
        cy.get('#description').type('Test Description');
        cy.get('button[type="submit"]').click();

        cy.wait('@addMedicalConditionError');

        cy.get('.error-message').should('be.visible');
    });

    it('validates required fields', () => {
        cy.get('button[type="submit"]').click();

        cy.get('#name')
            .invoke('prop', 'validationMessage')
            .should('not.be.empty');

        cy.get('#description')
            .invoke('prop', 'validationMessage')
            .should('not.be.empty');
    });

    it('persists form data after failed submission', () => {
        cy.intercept('POST', `${baseUrl}/medical-conditions/add-medical-condition`, {
            statusCode: 400,
            body: { message: 'Validation Error' }
        }).as('validationError');

        const testData = {
            name: 'Test Condition',
            severity: 'High',
            description: 'Test Description'
        };

        cy.get('#name').type(testData.name);
        cy.get('#severity').select(testData.severity);
        cy.get('#description').type(testData.description);
        cy.get('button[type="submit"]').click();

        cy.wait('@validationError');

        cy.get('#name').should('have.value', testData.name);
        cy.get('#severity').should('have.value', testData.severity);
        cy.get('#description').should('have.value', testData.description);
    });

    it('allows selecting different severity levels', () => {
        cy.get('#severity')
            .should('contain', 'Low')
            .and('contain', 'Medium')
            .and('contain', 'High');

        cy.get('#severity').select('Low').should('have.value', 'Low');
        cy.get('#severity').select('Medium').should('have.value', 'Medium');
        cy.get('#severity').select('High').should('have.value', 'High');
    });

    afterEach(() => {
        cy.clearLocalStorage();
    });
});
