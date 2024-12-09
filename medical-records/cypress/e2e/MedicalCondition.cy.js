// MedicalCondition.cy.js

describe('Medical Condition API E2E Tests', () => {
  const baseUrl = 'http://localhost:3001/medical-conditions';
  let authToken;

  before(() => {
    cy.request({
      method: 'POST',
      url: 'https://localhost:5001/api/auth/login',
      body: {
        email: 'admin@admin.com',
        password: 'Admin123!'
      }
    }).then((response) => {
      expect(response.status).to.eq(200);
      authToken = response.body.token;
    });
  });

  it('should add a new medical condition to the catalog', () => {
    const uniqueName = `TestCondition_${Date.now()}`;
    const condition = {
      name: uniqueName,
      severity: 'High',
      description: 'Test condition description'
    };

    cy.request({
      method: 'POST',
      url: `${baseUrl}/add-medical-condition`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: condition
    }).then((response) => {
      expect(response.status).to.eq(201);
      expect(response.body.message).to.eq('Medical condition added successfully');
      expect(response.body.medicalCondition.name).to.eq(uniqueName);
      expect(response.body.medicalCondition.severity).to.eq('High');
    });
  });

  it('should handle duplicate medical condition creation', () => {
    const condition = {
      name: 'Hypertension',
      severity: 'High',
      description: 'High blood pressure condition'
    };

    cy.request({
      method: 'POST',
      url: `${baseUrl}/add-medical-condition`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: condition,
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(409);
      expect(response.body.message).to.eq('Medical condition already exists');
    });
  });

  it('should search medical conditions by name', () => {
    const searchName = 'Hypertension';

    cy.request({
      method: 'GET',
      url: `${baseUrl}/search?name=${searchName}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      }
    }).then((response) => {
      expect(response.status).to.eq(200);
      expect(response.body).to.be.an('array');
      expect(response.body[0].name).to.eq(searchName);
    });
  });

  it('should search medical conditions by severity', () => {
    cy.request({
      method: 'GET',
      url: `${baseUrl}/search?severity=High`,
      headers: {
        Authorization: `Bearer ${authToken}`
      }
    }).then((response) => {
      expect(response.status).to.eq(200);
      expect(response.body).to.be.an('array');
      response.body.forEach(condition => {
        expect(condition.severity).to.eq('High');
      });
    });
  });

  it('should get all medical conditions', () => {
    cy.request({
      method: 'GET',
      url: `${baseUrl}/getConditionDetails`,
      headers: {
        Authorization: `Bearer ${authToken}`
      }
    }).then((response) => {
      expect(response.status).to.eq(200);
      expect(response.body).to.be.an('array');
      expect(response.body.length).to.be.at.least(1);
    });
  });

  it('should handle invalid condition data submission', () => {
    const invalidCondition = {
      name: '',
      severity: '',
      description: ''
    };

    cy.request({
      method: 'POST',
      url: `${baseUrl}/add-medical-condition`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: invalidCondition,
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(500);
      expect(response.body.message).to.eq('Internal server error');
    });
  });

  it('should handle search with non-existent condition name', () => {
    const nonExistentName = `NonExistent_${Date.now()}`;

    cy.request({
      method: 'GET',
      url: `${baseUrl}/search?name=${nonExistentName}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      }
    }).then((response) => {
      expect(response.status).to.eq(200);
      expect(response.body).to.be.an('array');
      expect(response.body.length).to.eq(0);
    });
  });

  it('should add medical condition to patient record', () => {
    const patientId = 'TestPatient123';
    const condition = {
      name: 'Diabetes',
      severity: 'Medium',
      description: 'Type 2 Diabetes'
    };

    cy.request({
      method: 'POST',
      url: `${baseUrl}/patient/${patientId}/conditions`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: condition,
      failOnStatusCode: false
    }).then((response) => {
      // Check either success or not found (depending on if patient exists)
      expect([201, 404]).to.include(response.status);
      if (response.status === 201) {
        expect(response.body.message).to.eq('Medical condition added successfully');
      }
    });
  });
});
