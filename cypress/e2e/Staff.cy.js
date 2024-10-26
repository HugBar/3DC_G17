describe('Staff API E2E Tests', () => {

    const baseUrl = 'https://localhost:5001/api/staff';
    let authToken;  // This will store the authentication token
    let staffId;    // This will store the staffId for later use in tests
  
    // Before all tests, authenticate and get a token
    before(() => {
      cy.request('POST', 'https://localhost:5001/api/auth/login', {
        email: 'admin@admin.com',  // Replace with valid credentials
        password: 'Admin123!'
      }).then((response) => {
        expect(response.status).to.eq(200);
        authToken = response.body.token;  // Assume the token is in the response body
      });
    });
  
    // 1. Test creating a new staff profile
    it('should create a new staff profile', () => {
      cy.request({
        method: 'POST',
        url: `${baseUrl}/create-staff-profile`,
        headers: {
          Authorization: `Bearer ${authToken}`  // Include the token in the request headers
        },
        body: {
            "firstName": "StaffTest",
            "lastName": "1234",
            "email": "stafftest@stafftest.com",
            "phoneNumber": "862345689",
            "specialization": "Doctor",
            "availabilitySlots": [
                  {
                "startTime": "2024-11-23T09:00:00",
                "endTime": "2024-11-23T12:00:00"
              },
              {
                "startTime": "2024-11-26T14:00:00",
                "endTime": "2024-11-26T18:00:00"
              }
            ]
          }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.have.property('id');
        expect(response.body).to.have.property('firstName', 'StaffTest');
        expect(response.body).to.have.property('lastName', '1234');
        expect(response.body).to.have.property('email', 'stafftest@stafftest.com');
        expect(response.body).to.have.property('phoneNumber', '862345689');
        expect(response.body).to.have.property('specialization', 'Doctor');
        expect(response.body).to.have.property('availabilitySlots').and.to.be.an('array');
        expect(response.body.availabilitySlots).to.have.length(2);
        expect(response.body.availabilitySlots[0]).to.have.property('startTime', '2024-11-23T09:00:00');
        expect(response.body.availabilitySlots[0]).to.have.property('endTime', '2024-11-23T12:00:00');
        expect(response.body.availabilitySlots[1]).to.have.property('startTime', '2024-11-26T14:00:00');
        expect(response.body.availabilitySlots[1]).to.have.property('endTime', '2024-11-26T18:00:00');
  
        staffId = response.body.id;
      });
    });
  
    // 2. Test retrieving the newly created staff by ID
    it('should retrieve the staff by ID', () => {
      cy.request({
        method: 'GET',
        url: `${baseUrl}/get-staff-profile/${staffId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.have.property('id', staffId);
        expect(response.body).to.have.property('firstName', 'StaffTest');
        expect(response.body).to.have.property('lastName', '1234');
        expect(response.body).to.have.property('email', 'stafftest@stafftest.com');
        expect(response.body).to.have.property('phoneNumber', '862345689');
        expect(response.body).to.have.property('specialization', 'Doctor');
        expect(response.body).to.have.property('availabilitySlots').and.to.be.an('array');
        expect(response.body.availabilitySlots).to.have.length(2);
        expect(response.body.availabilitySlots[0]).to.have.property('startTime', '2024-11-23T09:00:00');
        expect(response.body.availabilitySlots[0]).to.have.property('endTime', '2024-11-23T12:00:00');
        expect(response.body.availabilitySlots[1]).to.have.property('startTime', '2024-11-26T14:00:00');
        expect(response.body.availabilitySlots[1]).to.have.property('endTime', '2024-11-26T18:00:00');
      });
    });
  
    // 3. Test for staff not found
    it('should return 404 for a non-existent staff', () => {
      const nonExistentStaffId = 'nonexistent-staff-id';
      cy.request({
        method: 'GET',
        url: `${baseUrl}/get-staff-profile/${nonExistentStaffId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(404);
      });
    });

    it('should filter staff profiles based on criteria', () => {
      cy.request({
        method: 'GET',
        url: `${baseUrl}/filter`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        qs: {
          firstName: 'StaffTest',  // Example filter criteria
          // Add more query parameters as needed
        }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.be.an('array');
        response.body.forEach(staff => {
          expect(staff).to.have.property('firstName', 'StaffTest');
          // Add more assertions based on expected filter results
        });
      });
    });

    it('should deactivate a staff member by ID', () => {
      cy.request({
        method: 'PATCH',
        url: `${baseUrl}/${staffId}/deactivate`,
        headers: {
          Authorization: `Bearer ${authToken}`
        }
      }).then((response) => {
        expect(response.status).to.eq(200); // No Content
      });
    });

    it('should update a staff profile', () => {
      const patchDoc = [
        { op: 'replace', path: '/phoneNumber', value: '911999999' },
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
        expect(response.body).to.have.property('phoneNumber', '911999999');
      });
    });

  
    // 4. Test deleting the staff
    it('should delete the staff by ID', () => {
      cy.request({
        method: 'DELETE',
        url: `${baseUrl}/${staffId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        }
      }).then((response) => {
        expect(response.status).to.eq(200);
      });
    });
  
    // 5. Verify that the deleted staff no longer exists
    it('should return 404 for the deleted staff', () => {
      cy.request({
        method: 'GET',
        url: `${baseUrl}/get-staff-profile/${staffId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(404);
      });
    });
  
  });