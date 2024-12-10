describe('Search Specialization', () => {
    let authToken;

    before(() => {
        // Get auth token for admin user
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
        // Clear storage and setup auth
        cy.clearLocalStorage();
        cy.visit('http://localhost:3000/specializations/search', {
            onBeforeLoad: (win) => {
                win.localStorage.setItem('authToken', authToken);
            }
        });

        // Verify page load
        cy.url().should('include', '/specializations/search');
    });

    it('displays initial search form and loads specializations', () => {
        cy.get('h2').should('contain', 'Search Specializations');
        cy.get('input[name="name"]').should('exist');
        cy.get('input[name="description"]').should('exist');
        cy.get('.search-button').should('exist');
        cy.get('.clear-filters-button').should('exist');
        cy.get('.specializations-grid').should('exist');
    });

    it('performs search with name filter', () => {
        const searchTerm = 'Cardio';
        
        cy.get('input[name="name"]').type(searchTerm);
        cy.get('.search-button').click();

        cy.url().should('include', `name=${searchTerm}`);
        cy.get('.specializations-grid').should('exist');
        cy.contains('.specialization-card', searchTerm).should('be.visible');
    });

    it('performs search with description filter', () => {
        const searchTerm = 'brain';
        
        cy.get('input[name="description"]').type(searchTerm);
        cy.get('.search-button').click();

        cy.url().should('include', `description=${searchTerm}`);
        cy.get('.specialization-card').should('exist');
    });

    it('clears filters and resets search', () => {
        // Set filters first
        cy.get('input[name="name"]').type('Test');
        cy.get('input[name="description"]').type('Description');
        
        // Clear filters
        cy.get('.clear-filters-button').click();

        // Verify filters are cleared
        cy.get('input[name="name"]').should('have.value', '');
        cy.get('input[name="description"]').should('have.value', '');
        cy.url().should('not.include', 'name=');
        cy.url().should('not.include', 'description=');
    });

    it('shows specialization details in modal', () => {
        // Wait for specializations to load
        cy.get('.specializations-grid').should('exist');
        
        // Click first specialization
        cy.get('.specialization-card').first().click();

        // Verify modal appears
        cy.get('.modal-content').should('be.visible');
        cy.get('.modal-content h3').should('contain', 'Specialization Details');
        
        // Close modal
        // click close button 

        cy.get('.close-button').click();
       
        cy.get('.modal').should('not.exist');
    });

    it('handles no results scenario', () => {
        // Search with unlikely term
        cy.get('input[name="name"]').type('NonExistentSpecialization123');
        cy.get('.search-button').click();

        cy.get('.no-results').should('be.visible')
            .and('contain', 'No specializations found');
    });

    it('handles server errors gracefully', () => {
        // Intercept API call and force error
        cy.intercept('GET', '**/specializations/search*', {
            statusCode: 500,
            body: { message: 'Internal Server Error' }
        });

        cy.get('.search-button').click();

        cy.get('.error-message')
            .should('be.visible')
            .and('contain', 'Error searching specializations');
    });

    it('preserves search filters on page reload', () => {
        const searchName = 'Cardio';
        const searchDesc = 'Heart';

        // Set filters
        cy.get('input[name="name"]').type(searchName);
        cy.get('input[name="description"]').type(searchDesc);
        cy.get('.search-button').click();

        // Reload page
        cy.reload();

        // Verify filters are preserved
        cy.get('input[name="name"]').should('have.value', searchName);
        cy.get('input[name="description"]').should('have.value', searchDesc);
        cy.url().should('include', `name=${searchName}`);
        cy.url().should('include', `description=${searchDesc}`);
    });
});