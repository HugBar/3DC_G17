describe('Medical Record API E2E Tests', () => {
  const baseUrl = 'http://localhost:3001/medical-records';
  let authToken;
  let testPatientId;

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
    const patientId = '202412000001';
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

  it('should create new patient record', () => {
    testPatientId = `newpatient_${Date.now()}`;
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

    // First create a blank medical record
    cy.request({
      method: 'POST',
      url: `${baseUrl}/create/${testPatientId}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      }
    }).then((createResponse) => {
      expect(createResponse.status).to.eq(201);
      expect(createResponse.body.message).to.eq('Blank medical record created successfully');
      expect(createResponse.body.record.patientId).to.eq(testPatientId);
      expect(createResponse.body.record.conditions).to.be.an('array').that.is.empty;
      expect(createResponse.body.record.allergies).to.be.an('array').that.is.empty;

      // Then update the newly created record
      cy.request({
        method: 'PUT',
        url: `${baseUrl}/update/${testPatientId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        body: updateData
      }).then((updateResponse) => {
        expect(updateResponse.status).to.eq(200);
        expect(updateResponse.body.message).to.eq('Medical record updated successfully');
        
        const receivedCondition = updateResponse.body.record.conditions[0];
        const expectedCondition = updateData.conditions[0];
        expect(receivedCondition.name).to.eq(expectedCondition.name);
        expect(receivedCondition.severity).to.eq(expectedCondition.severity);

        const receivedAllergy = updateResponse.body.record.allergies[0];
        const expectedAllergy = updateData.allergies[0];
        expect(receivedAllergy.name).to.eq(expectedAllergy.name);
        expect(receivedAllergy.severity).to.eq(expectedAllergy.severity);

        // Update the record again with new data
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
          url: `${baseUrl}/update/${testPatientId}`,
          headers: {
            Authorization: `Bearer ${authToken}`
          },
          body: newUpdateData
        }).then((secondUpdateResponse) => {
          expect(secondUpdateResponse.status).to.eq(200);
          expect(secondUpdateResponse.body.message).to.eq('Medical record updated successfully');
          
          const updatedCondition = secondUpdateResponse.body.record.conditions[0];
          expect(updatedCondition.name).to.eq(newUpdateData.conditions[0].name);
          expect(updatedCondition.severity).to.eq(newUpdateData.conditions[0].severity);

          const updatedAllergy = secondUpdateResponse.body.record.allergies[0];
          expect(updatedAllergy.name).to.eq(newUpdateData.allergies[0].name);
          expect(updatedAllergy.severity).to.eq(newUpdateData.allergies[0].severity);
        });
      });
    });
  });

  it('should get medical record by patient ID', () => {
    // Get the record created in the previous test
    cy.request({
      method: 'GET',
      url: `${baseUrl}/${testPatientId}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      }
    }).then((response) => {
      expect(response.status).to.eq(200);
      expect(response.body.patientId).to.eq(testPatientId);
      
      // Verify the final state from the previous test
      const expectedCondition = {
        name: 'Asthma',
        severity: 'High'
      };
      const expectedAllergy = {
        name: 'Dust',
        severity: 'Medium'
      };

      const receivedCondition = response.body.conditions[0];
      expect(receivedCondition.name).to.eq(expectedCondition.name);
      expect(receivedCondition.severity).to.eq(expectedCondition.severity);

      const receivedAllergy = response.body.allergies[0];
      expect(receivedAllergy.name).to.eq(expectedAllergy.name);
      expect(receivedAllergy.severity).to.eq(expectedAllergy.severity);
    });
  });

  it('should search medical records with various filters', () => {
    // Test search with patient ID only
    cy.request({
      method: 'GET',
      url: `${baseUrl}/search?patientId=${testPatientId}`,
      headers: { Authorization: `Bearer ${authToken}` }
    }).then((searchResponse) => {
      expect(searchResponse.status).to.eq(200);
      expect(searchResponse.body.conditions).to.have.length(1);
      expect(searchResponse.body.allergies).to.have.length(1);
    });

    // Test search with condition filter
    cy.request({
      method: 'GET',
      url: `${baseUrl}/search?patientId=${testPatientId}&conditionName=Asthma`,
      headers: { Authorization: `Bearer ${authToken}` }
    }).then((searchResponse) => {
      expect(searchResponse.status).to.eq(200);
      expect(searchResponse.body.conditions).to.have.length(1);
      expect(searchResponse.body.conditions[0].name).to.eq('Asthma');
      expect(searchResponse.body.conditions[0].severity).to.eq('High');
    });

    // Test search with allergy filter
    cy.request({
      method: 'GET',
      url: `${baseUrl}/search?patientId=${testPatientId}&allergyName=Dust`,
      headers: { Authorization: `Bearer ${authToken}` }
    }).then((searchResponse) => {
      expect(searchResponse.status).to.eq(200);
      expect(searchResponse.body.allergies).to.have.length(1);
      expect(searchResponse.body.allergies[0].name).to.eq('Dust');
      expect(searchResponse.body.allergies[0].severity).to.eq('Medium');
    });

    // Test search with both condition and allergy filters
    cy.request({
      method: 'GET',
      url: `${baseUrl}/search?patientId=${testPatientId}&conditionName=Asthma&allergyName=Dust`,
      headers: { Authorization: `Bearer ${authToken}` }
    }).then((searchResponse) => {
      expect(searchResponse.status).to.eq(200);
      expect(searchResponse.body.conditions).to.have.length(1);
      expect(searchResponse.body.allergies).to.have.length(1);
      expect(searchResponse.body.conditions[0].name).to.eq('Asthma');
      expect(searchResponse.body.conditions[0].severity).to.eq('High');
      expect(searchResponse.body.allergies[0].name).to.eq('Dust');
      expect(searchResponse.body.allergies[0].severity).to.eq('Medium');
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