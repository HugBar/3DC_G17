describe('Medical Record API E2E Tests', () => {
  const baseUrl = 'http://localhost:3001/medical-records';
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
      expect(response.status).to.eq(200);
      authToken = response.body.token;
    });
  });

  it('should update conditions and allergies for existing record', () => {
    const patientId = 'TestPatient12';
    const updateData = {
      conditions: [
        { 
          name: 'Hypertension',
          severity: 'High'
        }
      ],
      allergies: [
        {
          name: 'Peanuts',
          severity: 'High'
        }
      ]
    };

    cy.request({
      method: 'PUT',
      url: `${baseUrl}/update/${patientId}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: updateData,
      failOnStatusCode: false
    }).then((response) => {
      // Log response for debugging
      cy.log(JSON.stringify(response.body));
      
      expect([200, 201]).to.include(response.status);
      expect(response.body.record.patientId).to.eq(patientId);
      
      // Compare only the relevant properties
      const receivedCondition = response.body.record.conditions[0];
      const expectedCondition = updateData.conditions[0];
      expect(receivedCondition.name).to.eq(expectedCondition.name);
      expect(receivedCondition.severity).to.eq(expectedCondition.severity);

      const receivedAllergy = response.body.record.allergies[0];
      const expectedAllergy = updateData.allergies[0];
      expect(receivedAllergy.name).to.eq(expectedAllergy.name);
      expect(receivedAllergy.severity).to.eq(expectedAllergy.severity);
    });
  });

  it('should get medical record by patient ID', () => {
    const patientId = `patient_${Date.now()}`;
    const updateData = {
      conditions: [{
        name: 'Diabetes',
        severity: 'High'
      }],
      allergies: [{
        name: 'Peanuts',
        severity: 'High'
      }]
    };

    cy.request({
      method: 'PUT',
      url: `${baseUrl}/update/${patientId}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: updateData,
      failOnStatusCode: false
    }).then(() => {
      cy.wait(1000);
      
      cy.request({
        method: 'GET',
        url: `${baseUrl}/${patientId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body.patientId).to.eq(patientId);
        
        // Compare only the relevant properties
        const receivedCondition = response.body.conditions[0];
        const expectedCondition = updateData.conditions[0];
        expect(receivedCondition.name).to.eq(expectedCondition.name);
        expect(receivedCondition.severity).to.eq(expectedCondition.severity);

        const receivedAllergy = response.body.allergies[0];
        const expectedAllergy = updateData.allergies[0];
        expect(receivedAllergy.name).to.eq(expectedAllergy.name);
        expect(receivedAllergy.severity).to.eq(expectedAllergy.severity);
      });
    });
  });

  it('should create new record when updating non-existent patient record', () => {
    const patientId = `newpatient_${Date.now()}`;
    const updateData = {
      conditions: [
        { 
          name: 'Asthma',
          severity: 'Medium'
        }
      ],
      allergies: [
        {
          name: 'Dust',
          severity: 'Low'
        }
      ]
    };

    // First attempt should create a new record
    cy.request({
      method: 'PUT',
      url: `${baseUrl}/update/${patientId}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: updateData,
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(201);
      expect(response.body.message).to.eq('Medical record created successfully');
      expect(response.body.record.patientId).to.eq(patientId);
      
      const receivedCondition = response.body.record.conditions[0];
      const expectedCondition = updateData.conditions[0];
      expect(receivedCondition.name).to.eq(expectedCondition.name);
      expect(receivedCondition.severity).to.eq(expectedCondition.severity);

      const receivedAllergy = response.body.record.allergies[0];
      const expectedAllergy = updateData.allergies[0];
      expect(receivedAllergy.name).to.eq(expectedAllergy.name);
      expect(receivedAllergy.severity).to.eq(expectedAllergy.severity);

      // Now update the newly created record
      const newUpdateData = {
        conditions: [
          { 
            name: 'Asthma',
            severity: 'High'
          }
        ],
        allergies: [
          {
            name: 'Dust',
            severity: 'Medium'
          }
        ]
      };

      cy.request({
        method: 'PUT',
        url: `${baseUrl}/update/${patientId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        body: newUpdateData
      }).then((updateResponse) => {
        expect(updateResponse.status).to.eq(200);
        expect(updateResponse.body.message).to.eq('Medical record updated successfully');
        
        const updatedCondition = updateResponse.body.record.conditions[0];
        expect(updatedCondition.name).to.eq(newUpdateData.conditions[0].name);
        expect(updatedCondition.severity).to.eq(newUpdateData.conditions[0].severity);

        const updatedAllergy = updateResponse.body.record.allergies[0];
        expect(updatedAllergy.name).to.eq(newUpdateData.allergies[0].name);
        expect(updatedAllergy.severity).to.eq(newUpdateData.allergies[0].severity);
      });
    });
  });

  it('should search medical records with various filters', () => {
    const patientId = `searchtest_${Date.now()}`;
    const updateData = {
      conditions: [
        { name: 'Asthma', severity: 'High' },
        { name: 'Diabetes', severity: 'Medium' }
      ],
      allergies: [
        { name: 'Peanuts', severity: 'High' },
        { name: 'Shellfish', severity: 'Low' }
      ]
    };

    // First create a test record
    cy.request({
      method: 'PUT',
      url: `${baseUrl}/update/${patientId}`,
      headers: { Authorization: `Bearer ${authToken}` },
      body: updateData
    }).then((response) => {
      expect(response.status).to.eq(201);
      
      // Test search with patient ID only
      cy.request({
        method: 'GET',
        url: `${baseUrl}/search?patientId=${patientId}`,
        headers: { Authorization: `Bearer ${authToken}` }
      }).then((searchResponse) => {
        expect(searchResponse.status).to.eq(200);
        expect(searchResponse.body.conditions).to.have.length(2);
        expect(searchResponse.body.allergies).to.have.length(2);
      });

      // Test search with condition filter
      cy.request({
        method: 'GET',
        url: `${baseUrl}/search?patientId=${patientId}&conditionName=Asthma`,
        headers: { Authorization: `Bearer ${authToken}` }
      }).then((searchResponse) => {
        expect(searchResponse.status).to.eq(200);
        expect(searchResponse.body.conditions).to.have.length(1);
        expect(searchResponse.body.conditions[0].name).to.eq('Asthma');
      });

      // Test search with allergy filter
      cy.request({
        method: 'GET',
        url: `${baseUrl}/search?patientId=${patientId}&allergyName=Peanuts`,
        headers: { Authorization: `Bearer ${authToken}` }
      }).then((searchResponse) => {
        expect(searchResponse.status).to.eq(200);
        expect(searchResponse.body.allergies).to.have.length(1);
        expect(searchResponse.body.allergies[0].name).to.eq('Peanuts');
      });

      // Test search with both condition and allergy filters
      cy.request({
        method: 'GET',
        url: `${baseUrl}/search?patientId=${patientId}&conditionName=Asthma&allergyName=Peanuts`,
        headers: { Authorization: `Bearer ${authToken}` }
      }).then((searchResponse) => {
        expect(searchResponse.status).to.eq(200);
        expect(searchResponse.body.conditions).to.have.length(1);
        expect(searchResponse.body.allergies).to.have.length(1);
        expect(searchResponse.body.conditions[0].name).to.eq('Asthma');
        expect(searchResponse.body.allergies[0].name).to.eq('Peanuts');
      });
    });
  });

  it('should handle non-existent patient ID in search', () => {
    cy.request({
      method: 'GET',
      url: `${baseUrl}/search?patientId=nonexistent`,
      headers: { Authorization: `Bearer ${authToken}` },
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(404);
      expect(response.body.message).to.eq('Medical record not found');
    });
  });
}); 