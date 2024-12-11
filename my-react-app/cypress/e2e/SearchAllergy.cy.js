describe('Search Allergy', () => {
    let authToken;

    before(() => {
        // Login and get auth token
        cy.request({
            method: 'POST',
            url: 'https://localhost:5001/api/auth/login',
            body: {
                email: 'doctor@doctor.com',
                password: 'Doctor123!'
            }
        }).then((response) => {
            authToken = response.body.token;
        });
    });

    beforeEach(() => {
        // Clear storage and set up auth token
        cy.clearLocalStorage();
        cy.visit('http://localhost:3000/allergies/search', {
            onBeforeLoad: (win) => {
                win.localStorage.setItem('authToken', authToken);
            }
        });
        cy.url().should('include', '/allergies/search');
    });

    it('should display initial form elements', () => {
        cy.get('h2').should('contain', 'Search Allergies');
        cy.get('[placeholder="Allergen Name"]').should('exist');
        cy.get('[data-testid="severity-select"]').should('exist');
        cy.get('button').contains('Search').should('exist');
        cy.get('button').contains('Clear Filters').should('exist');
    });

    it('should apply search filters', () => {
        const testAllergen = 'Peanuts';
        const testSeverity = 'High';
    
        // First verify select exists
        cy.get('[data-testid="severity-select"]').should('exist');
        
        // Then proceed with test
        cy.get('[placeholder="Allergen Name"]').type(testAllergen);
        cy.get('[data-testid="severity-select"]').select(testSeverity);
        cy.get('button').contains('Search').click();
    
        // Verify URL parameters
        cy.url().should('include', `allergen=${testAllergen}`);
        cy.url().should('include', `severity=${testSeverity}`);
    });

    it('should clear filters when clicking clear button', () => {
        // Fill in filters first
        cy.get('[placeholder="Allergen Name"]').type('Test');
        cy.get('[data-testid="severity-select"]').select('High');

        // Clear filters
        cy.get('button').contains('Clear Filters').click();

        // Verify filters are cleared
        cy.get('[placeholder="Allergen Name"]').should('have.value', '');
        cy.get('[data-testid="severity-select"]').should('have.value', '');
        cy.url().should('not.include', 'allergen');
        cy.url().should('not.include', 'severity');
    });

    it('should handle no results found', () => {
        // Search with non-existent allergen
        cy.get('[placeholder="Allergen Name"]').type('NonExistentAllergen123');
        cy.get('button').contains('Search').click();

        // Verify no results message
        cy.contains('No allergies found').should('be.visible');
    });

    it('should update URL with search parameters', () => {
        const testAllergen = 'Peanuts';
        const testSeverity = 'High';

        // Fill in search filters
        cy.get('[placeholder="Allergen Name"]').type(testAllergen);
        cy.get('[data-testid="severity-select"]').select(testSeverity);
        cy.get('button').contains('Search').click();

        // Verify URL parameters
        cy.url().should('include', `allergen=${testAllergen}`);
        cy.url().should('include', `severity=${testSeverity}`);
    });

    it('should handle server errors gracefully', () => {
        // Intercept API call and force error
        cy.intercept('GET', '**/allergies/search*', {
            statusCode: 500,
            body: { message: 'Internal Server Error' }
        }).as('searchRequest');

        // Perform search
        cy.get('[placeholder="Allergen Name"]').type('Test');
        cy.get('button').contains('Search').click();

        // Verify error handling
        cy.wait('@searchRequest');
        cy.contains('Error searching allergies').should('be.visible');
    });

    it('should handle click card', () => {
        // Intercept API call and return a list of allergies
        cy.intercept('GET', '**/allergies/search*', {
            statusCode: 200,
            body: [
                {
                    id: 1,
                    allergen: 'Peanuts',
                    severity: 'High'
                }
            ]
        }).as('searchRequest');

        // Perform search
        cy.get('[placeholder="Allergen Name"]').type('Test');
        cy.get('button').contains('Search').click();

        // Verify card click
        cy.wait('@searchRequest');
        cy.get('.allergy-card').click();


        //Verify contains text in the card
        cy.contains('Allergy Details').should('be.visible');
    });
});