describe('Update Staff Form', () => {
  const baseUrl = 'https://localhost:5001/api/staff';
  const frontendUrl = 'http://localhost:3000';
  let authToken;
  let staffId;
  const staffEmail = "stafftest@stafftest.com";
  const staffPhone = "961234567";

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

      // Create a test staff profile
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
      staffId = staffResponse.body.id; // Save the created staff ID
    });
  });

  beforeEach(() => {
    // Clear localStorage first
    cy.clearLocalStorage();
    
    // Set up authentication before visiting the page
    cy.window().then((win) => {
      win.localStorage.clear();
      win.localStorage.setItem('authToken', authToken);
      win.localStorage.setItem('userRole', 'Admin'); // Add role if your app uses it
    });

    // Visit the update page with auth headers
    cy.visit(`${frontendUrl}/staff/update/${staffId}`, {
      onBeforeLoad: (win) => {
        // Set up localStorage before page load
        win.localStorage.setItem('authToken', authToken);
        win.localStorage.setItem('userRole', 'Admin');
      },
      headers: {
        'Authorization': `Bearer ${authToken}`
      }
    });

    // Force reload to ensure new auth state is picked up
    cy.reload();

    // Wait for the form to be visible
    cy.get('form', { timeout: 10000 }).should('be.visible');
  });

  it('should load the update staff form with existing data', () => {
    // Check if the form fields are populated with the existing data
    cy.get('#email').should('have.value', staffEmail);
    cy.get('#phoneNumber').should('have.value', staffPhone);
    cy.get('#specialization').should('have.value', 'Nurse');
    cy.get('input[name="startTime"]').first().should('have.value', '2024-11-23T08:00:00');
    cy.get('input[name="endTime"]').first().should('have.value', '2024-11-23T11:00:00');
  });

  it('should successfully update staff information', () => {
    // Update form fields
    cy.get('#phoneNumber').clear().type('999111222');
    cy.get('#specialization').clear().type('Surgeon');
    cy.get('input[name="startTime"]').first().clear().type('2024-11-24T08:00:00');
    cy.get('input[name="endTime"]').first().clear().type('2024-11-24T12:00:00');

    // Submit the form
    cy.get('button[type="submit"]').click();

    // Verify the success message
    cy.get('.success-message').should('be.visible').and('contain', 'Staff updated successfully!');
  });

  it('should display validation errors when fields are invalid', () => {
    // Enter an invalid phone number
    cy.get('#phoneNumber').clear().type('123');

    // Submit the form
    cy.get('button[type="submit"]').click();

    // Check for validation error message
    cy.get('.error-message').should('be.visible').and('contain', 'Please fix the errors before submitting');
  });

  after(() => {
    // Cleanup: Delete the test staff profile created for this test
    if (staffId) {
      cy.request({
        method: 'DELETE',
        url: `${baseUrl}/${staffId}`,
        headers: {
          'Authorization': `Bearer ${authToken}`
        }
      });
    }
  });
});
