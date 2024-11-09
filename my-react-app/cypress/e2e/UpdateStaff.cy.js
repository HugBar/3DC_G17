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

      // Create a test staff profile to update
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
    cy.clearLocalStorage();
    cy.window().then((win) => {
      win.localStorage.setItem('authToken', authToken);
    });

    // Visit the update staff page with the created staff ID
    cy.visit(`${frontendUrl}/staff/update/${staffId}`, {
      onBeforeLoad: (win) => {
        win.localStorage.setItem('authToken', authToken);
      }
    });

    cy.reload();
    cy.get('form', { timeout: 10000 }).should('be.visible');
  });

  it('should display the update staff form with existing data', () => {
    cy.get('h2').should('contain', 'Update Staff');
    cy.get('input[name="specialization"]').should('have.value', 'Nurse');
  });

  it('should successfully update staff specialization', () => {
    const updatedSpecialization = 'Surgeon';

    cy.intercept('PATCH', `${baseUrl}/edit-staff-profile/${staffId}`).as('updateStaffRequest');

    // Clear specialization field properly before typing a new value
    cy.get('input[name="specialization"]')
      .should('have.value', 'Nurse')
      .clear()
      .should('have.value', '')
      .type(updatedSpecialization);

    cy.get('button[type="submit"]').click();

    cy.wait('@updateStaffRequest').then((interception) => {
      expect(interception.response.statusCode).to.equal(200);
    });

    cy.get('.success-message').should('contain', 'Staff updated successfully!');
  });

  it('should handle network errors gracefully', () => {
    // Wait for initial load and get current value
    cy.get('input[name="specialization"]').then(($input) => {
      const currentValue = $input.val();
      
      cy.intercept('PATCH', `${baseUrl}/edit-staff-profile/${staffId}`, {
        statusCode: 500,
        body: { message: 'Internal Server Error' }
      }).as('updateStaffError');

      // Use the current value to verify field before modifying
      cy.get('input[name="specialization"]')
        .should('have.value', currentValue)
        .clear()
        .type('ErrorTestSpecialization');
        
      cy.get('button[type="submit"]').click();

      cy.get('.error-message').should('contain', 'Error updating staff');
    });
  });


  it('should update staff specialization via direct PATCH request', () => {
    const patchDoc = [
      { op: 'replace', path: '/specialization', value: 'Cardiologist' },
    ];

    cy.request({
      method: 'PATCH',
      url: `${baseUrl}/edit-staff-profile/${staffId}`,
      headers: {
        Authorization: `Bearer ${authToken}`,
        'Content-Type': 'application/json-patch+json'
      },
      body: patchDoc
    }).then((response) => {
      expect(response.status).to.eq(200);
      expect(response.body).to.have.property('specialization', 'Cardiologist');
    });
  });
  
  afterEach(() => {
    cy.clearLocalStorage();
  });

  after(() => {
    // Clean up - delete the test staff profile
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
