describe('Patient API E2E Tests', () => {

    const baseUrl = 'https://localhost:5001/api/patient';
    let authToken;  // This will store the authentication token
    let authTokenPatient;
    let patientId;  // This will store the patientId for later use in tests

    // Before all tests, authenticate and get a token
    before(() => {
      cy.request('POST', 'https://localhost:5001/api/auth/login', {
        email: 'admin@admin.com',  // Replace with valid credentials
        password: 'Admin123!'
      }).then((response) => {
        expect(response.status).to.eq(200);
        authToken = response.body.token;  // Assume the token is in the response body
      });

      cy.request('POST', 'https://localhost:5001/api/auth/login', {
        email: 'patienttest@patienttest.com',  // Replace with valid credentials
        password: 'Patient123!'
      }).then((response) => {
        expect(response.status).to.eq(200);
        authTokenPatient = response.body.token;  // Assume the token is in the response body
      });



    });

    // 1. Test creating a new patient profile
    it('should create a new patient profile', () => {
      cy.request({
        method: 'POST',
        url: `${baseUrl}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        body: {
          "firstName": "PatientTest",
          "lastName": "1234",
          "email": "patienttest@patienttest.com",
          "phoneNumber": "862345689",
          "dateofBirth": "1990-01-01",
          "gender": "Male",
          "contactInfo": "123 Main St",
          "emergencyContact": "Emergency Contact",
          "appointmentHistory": "",
          "medicalHistory": ""
        }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.have.property('id');
        patientId = response.body.id;
      });
    });

    // 2. Test retrieving the newly created patient by ID
    it('should retrieve the patient by ID', () => {
      cy.request({
        method: 'GET',
        url: `${baseUrl}/get-patient-profile/${patientId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.have.property('id', patientId);
      });
    });


    // 3. Test updating the patient profile
    it('should update the patient profile', () => {
        const patchDoc = [
          { op: 'replace', path: '/email', value: 'CypressTesting2@gmail.com' },
          { op: 'replace', path: '/phoneNumber', value: '777858999' }
        ];
  
        cy.request({
          method: 'PATCH',
          url: `${baseUrl}/edit-patient-profile`,
          headers: {
            Authorization: `Bearer ${authTokenPatient}`,
            'Content-Type': 'application/json-patch+json'
          },
          body: patchDoc
        }).then((response) => {
          expect(response.status).to.eq(200);
          expect(response.body).to.have.property('email', 'CypressTesting2@gmail.com');
          expect(response.body).to.have.property('phoneNumber', '777858999');
        });
      });


          // 3. Test updating the patient profile
    it('should update the patient profile', () => {
        const patchDoc = [
          { op: 'replace', path: '/email', value: 'CypressTesting1@gmail.com' },
          { op: 'replace', path: '/phoneNumber', value: '777888999' },
        ];
  
        cy.request({
          method: 'PATCH',
          url: `${baseUrl}/admin/edit-patient-profile/${patientId}`,
          headers: {
            Authorization: `Bearer ${authToken}`,
            'Content-Type': 'application/json-patch+json'
          },
          body: patchDoc
        }).then((response) => {
          expect(response.status).to.eq(200);
          expect(response.body).to.have.property('email', 'CypressTesting1@gmail.com');
          expect(response.body).to.have.property('phoneNumber', '777888999');
        });
      });

    // 4. Test deleting the patient
    it('should delete the patient by ID', () => {
      cy.request({
        method: 'DELETE',
        url: `${baseUrl}/delete-patient/${patientId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        body: {
          confirmDeletion: true
        }
      }).then((response) => {
        expect(response.status).to.eq(200);
      });
    });

    // 5. Verify that the deleted patient no longer exists
    it('should return 404 for the deleted patient', () => {
      cy.request({
        method: 'GET',
        url: `${baseUrl}/get-patient-profile/${patientId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(404);
      });
    });

});