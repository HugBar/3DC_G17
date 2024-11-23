describe('Create Staff Form', () => {
    const baseUrl = 'https://localhost:5001/api/staff';
    const frontendUrl = 'http://localhost:3000';
    let authToken;
    let createdStaffId;
  
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
      // Clear localStorage first
      cy.clearLocalStorage();
      
      // Set up authentication before visiting the page
      cy.window().then((win) => {
        win.localStorage.clear();
        win.localStorage.setItem('authToken', authToken);
      });
  
      // Visit the create staff page
      cy.visit(`${frontendUrl}/staff/create`, {
        onBeforeLoad: (win) => {
          win.localStorage.setItem('authToken', authToken);
        }
      });
  
      // Force reload to ensure new auth state is picked up
      cy.reload();
  
      // Wait for the form to be visible
      cy.get('form', { timeout: 10000 }).should('be.visible');
    });
  
    it('should display the create staff form', () => {
      cy.get('h2').should('contain', 'Create Staff');
      cy.get('input[name="firstName"]').should('exist');
      cy.get('input[name="lastName"]').should('exist');
      cy.get('input[name="email"]').should('exist');
      cy.get('input[name="phoneNumber"]').should('exist');
      cy.get('input[name="specialization"]').should('exist');
    });
  
    it('should successfully create a new staff member', () => {
      const staffData = {
        firstName: 'John',
        lastName: 'Doe',
        email: 'stafftest@stafftest.com',
        phoneNumber: '962041130',
        specialization: 'Cardiology'
      };
  
      // Intercept the create request to capture the staff ID
      cy.intercept('POST', `${baseUrl}/create-staff-profile`).as('createStaffRequest');
  
      // Fill in the form
      cy.get('input[name="firstName"]').type(staffData.firstName);
      cy.get('input[name="lastName"]').type(staffData.lastName);
      cy.get('input[name="email"]').type(staffData.email);
      cy.get('input[name="phoneNumber"]').type(staffData.phoneNumber);
      cy.get('input[name="specialization"]').type(staffData.specialization);
  
      // Add availability slot
      cy.get('input[name="startTime"]').first().type('2024-12-01T09:00');
      cy.get('input[name="endTime"]').first().type('2024-12-01T17:00');
  
      // Submit the form
      cy.get('button[type="submit"]').click();
  
      // Wait for the create request and store the staff ID
      cy.wait('@createStaffRequest').then((interception) => {
        createdStaffId = interception.response.body.id;
      });
  
      // Check for success message
      cy.get('.success-message').should('contain', 'Staff John Doe created successfully!');
  
      // Verify form is cleared after successful submission
      cy.get('input[name="firstName"]').should('have.value', '');
      cy.get('input[name="lastName"]').should('have.value', '');
      cy.get('input[name="email"]').should('have.value', '');
    });
  
    it('should allow adding and removing multiple availability slots', () => {
      // Add a new slot
      cy.get('.add-slot-button').click();
      cy.get('.slot-container').should('have.length', 2);
  
      // Fill in both slots
      cy.get('input[name="startTime"]').first().type('2024-12-01T09:00');
      cy.get('input[name="endTime"]').first().type('2024-12-01T12:00');
      cy.get('input[name="startTime"]').last().type('2024-12-01T13:00');
      cy.get('input[name="endTime"]').last().type('2024-12-01T17:00');
  
      // Remove the second slot
      cy.get('.remove-slot-button').last().click();
      cy.get('.slot-container').should('have.length', 1);
    });
  
    it('should handle network errors gracefully', () => {
      // Intercept the create staff request and simulate a failure
      cy.intercept('POST', `${baseUrl}/create-staff-profile`, {
        statusCode: 500,
        body: { message: 'Internal Server Error' }
      }).as('createStaffError');
  
      // Fill in the form
      cy.get('input[name="firstName"]').type('John');
      cy.get('input[name="lastName"]').type('Doe');
      cy.get('input[name="email"]').type('stafftest@stafftest.com');
      cy.get('input[name="phoneNumber"]').type('962041130');
      cy.get('input[name="specialization"]').type('Cardiology');
      cy.get('input[name="startTime"]').first().type('2024-12-01T09:00');
      cy.get('input[name="endTime"]').first().type('2024-12-01T17:00');
  
      // Submit the form
      cy.get('button[type="submit"]').click();
  
      // Check for error message
      cy.get('.error-message').should('contain', 'Internal Server Error');
    });
  
    it('should handle duplicate email validation error', () => {
      // Fill in the form with an existing email
      cy.get('input[name="firstName"]').type('John');
      cy.get('input[name="lastName"]').type('Doe');
      cy.get('input[name="email"]').type('admin@admin.com'); // Using existing email
      cy.get('input[name="phoneNumber"]').type('123456789');
      cy.get('input[name="specialization"]').type('Cardiology');
      
      // Add availability slot
      cy.get('input[name="startTime"]').first().type('2024-12-01T09:00');
      cy.get('input[name="endTime"]').first().type('2024-12-01T17:00');

      // Intercept the create staff request
      cy.intercept('POST', `${baseUrl}/create-staff-profile`, {
        statusCode: 400,
        body: {
          message: 'Email already exists'
        }
      }).as('createStaffRequest');

      // Submit the form
      cy.get('button[type="submit"]').click();

      // Wait for the request
      cy.wait('@createStaffRequest');

      // Check for error message
      cy.get('.error-message')
        .should('be.visible')
        .and('contain', 'Email already exists');

      // Verify the form data is preserved
      cy.get('input[name="email"]').should('have.value', 'admin@admin.com');
    });
  
    afterEach(() => {
      // Clean up localStorage after each test
      cy.clearLocalStorage();
    });
  
    after(() => {
      // Clean up created staff profile after all tests
      if (createdStaffId) {
        cy.request({
          method: 'DELETE',
          url: `${baseUrl}/${createdStaffId}`,
          headers: {
            'Authorization': `Bearer ${authToken}`
          }
        }).then((response) => {
          expect(response.status).to.eq(200);
        });
      }

    });
  });