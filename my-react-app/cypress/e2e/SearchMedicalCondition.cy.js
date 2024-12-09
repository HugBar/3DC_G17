describe('Search Medical Condition', () => {
    let authToken;
  
    before(() => {
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
      cy.clearLocalStorage();
  
      cy.visit('http://localhost:3000/medical-conditions/search', {
        onBeforeLoad: (win) => {
          win.localStorage.setItem('authToken', authToken);
        }
      });
  
      cy.url().should('include', '/medical-conditions/search');
    });
  
    it('should display initial search form elements', () => {
      cy.get('h2').should('contain', 'Search Medical Conditions');
      cy.get('input[name="name"]').should('exist');
      cy.get('select[name="severity"]').should('exist');
      cy.get('.search-button').should('exist');
      cy.get('.clear-filters-button').should('exist');
    });
  
    it('should search medical conditions with name filter', () => {
      cy.intercept('GET', '**/medical-conditions/search*').as('searchConditions');
  
      cy.get('input[name="name"]').type('Asthma');
      cy.get('.search-button').click();
  
      cy.wait('@searchConditions').then((interception) => {
        expect(interception.response.statusCode).to.equal(200);
      });
  
      cy.get('.conditions-grid').should('be.visible');
      cy.get('.condition-card').should('exist');
    });
  
    it('should search medical conditions with severity filter', () => {
      cy.intercept('GET', '**/medical-conditions/search*').as('searchConditions');
  
      cy.get('select[name="severity"]').select('High');
      cy.get('.search-button').click();
  
      cy.wait('@searchConditions').then((interception) => {
        expect(interception.response.statusCode).to.equal(200);
      });
  
      cy.get('.conditions-grid').should('be.visible');
      cy.get('.condition-card').should('exist');
    });
  
    it('should search with both name and severity filters', () => {
      cy.intercept('GET', '**/medical-conditions/search*').as('searchConditions');
  
      cy.get('input[name="name"]').type('Asthma');
      cy.get('select[name="severity"]').select('High');
      cy.get('.search-button').click();
  
      cy.wait('@searchConditions').then((interception) => {
        expect(interception.response.statusCode).to.equal(200);
      });
  
      cy.get('.conditions-grid').should('be.visible');
      cy.get('.condition-card').should('exist');
    });
  
    it('should clear filters when clicking clear button', () => {
      cy.get('input[name="name"]').type('Test');
      cy.get('select[name="severity"]').select('High');
      
      cy.get('.clear-filters-button').click();
  
      cy.get('input[name="name"]').should('have.value', '');
      cy.get('select[name="severity"]').should('have.value', '');
    });
  
    it('should show condition details when clicking on a condition card', () => {
      cy.intercept('GET', '**/medical-conditions/search*', {
        statusCode: 200,
        body: [{
          name: 'Test Condition',
          severity: 'High',
          description: 'Test Description'
        }]
      }).as('searchConditions');
  
      cy.get('.search-button').click();
      cy.wait('@searchConditions');
  
      cy.get('.condition-card').first().click();
      cy.get('.condition-details-modal').should('be.visible');
      cy.get('.modal-content').within(() => {
        cy.contains('Test Condition').should('be.visible');
        cy.contains('High').should('be.visible');
        cy.contains('Test Description').should('be.visible');
      });
    });
  
    it('should handle no results gracefully', () => {
      cy.intercept('GET', '**/medical-conditions/search*', {
        statusCode: 404,
        body: []
      }).as('noResults');
  
      cy.get('input[name="name"]').type('NonexistentCondition');
      cy.get('.search-button').click();
  
      cy.wait('@noResults');
      cy.get('.error-message').should('contain', 'Nenhuma condição médica encontrada.');
    });
  
    it('should handle server errors gracefully', () => {
      cy.intercept('GET', '**/medical-conditions/search*', {
        statusCode: 500,
        body: { message: 'Server error' }
      }).as('serverError');
  
      cy.get('input[name="name"]').type('Test');
      cy.get('.search-button').click();
  
      cy.wait('@serverError');
      cy.get('.error-message').should('contain', 'Erro ao buscar condições médicas.');
    });
  
    it('should update URL with search parameters', () => {
      cy.get('input[name="name"]').type('TestCondition');
      cy.get('select[name="severity"]').select('High');
      cy.get('.search-button').click();
  
      cy.url().should('include', 'name=TestCondition')
        .and('include', 'severity=High');
    });
  
    afterEach(() => {
      cy.clearLocalStorage();
    });
  });
  