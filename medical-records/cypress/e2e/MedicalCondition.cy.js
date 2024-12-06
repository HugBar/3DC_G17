/*describe('Medical Conditions API E2E Tests', () => {
  const baseUrl = 'http://localhost:3001/api/medical-conditions';
  const authUrl = 'http://localhost:5001/api/auth';
  let adminToken;
  let doctorToken;

  before(() => {
    // Login como admin
    cy.request({
      method: 'POST',
      url: `${authUrl}/login`,
      body: {
        email: 'admin@admin.com',
        password: 'Admin123!'
      },
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(200);
      adminToken = response.body.token;
    });

    // Login como doctor
    cy.request({
      method: 'POST',
      url: `${authUrl}/login`,
      body: {
        email: 'doctor@doctor.com',
        password: 'Doctor123!'
      },
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(200);
      doctorToken = response.body.token;
    });
  });

  describe('Admin Operations', () => {
    it('should allow admin to create a new medical condition with unique name', () => {
      const uniqueName = `TestCondition_${Date.now()}`;
      const condition = {
        name: uniqueName,
        severity: 'High',
        description: 'Test condition description'
      };

      cy.request({
        method: 'POST',
        url: `${baseUrl}/add-model`,
        headers: {
          Authorization: `Bearer ${adminToken}`
        },
        body: condition
      }).then((response) => {
        expect(response.status).to.eq(201);
        expect(response.body.medicalCondition.name).to.eq(uniqueName);
      });
    });

    it('should not allow non-admin to create medical condition', () => {
      const condition = {
        name: `TestCondition_${Date.now()}`,
        severity: 'High',
        description: 'Test condition description'
      };

      cy.request({
        method: 'POST',
        url: `${baseUrl}/add-model`,
        headers: {
          Authorization: `Bearer ${doctorToken}`
        },
        body: condition,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(403);
        expect(response.body.message).to.eq('Unauthorized: Admin access required');
      });
    });

    it('should handle duplicate medical condition creation by admin', () => {
      const condition = {
        name: 'Diabetes',
        severity: 'High',
        description: 'Common metabolic disorder'
      };

      cy.request({
        method: 'POST',
        url: `${baseUrl}/add-model`,
        headers: {
          Authorization: `Bearer ${adminToken}`
        },
        body: condition,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(409);
        expect(response.body.message).to.eq('Medical condition already exists');
      });
    });
  });

  describe('Doctor Operations', () => {
    it('should allow doctor to search medical conditions with filters', () => {
      const searchParams = {
        name: 'Diabetes',
        severity: 'High'
      };

      cy.request({
        method: 'GET',
        url: `${baseUrl}/search`,
        headers: {
          Authorization: `Bearer ${doctorToken}`
        },
        qs: searchParams
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.be.an('array');
        if (response.body.length > 0) {
          expect(response.body[0]).to.have.property('name');
          expect(response.body[0]).to.have.property('severity');
        }
      });
    });

    it('should not allow non-doctor to search medical conditions', () => {
      const searchParams = {
        severity: 'High'
      };

      cy.request({
        method: 'GET',
        url: `${baseUrl}/search`,
        headers: {
          Authorization: `Bearer ${adminToken}`
        },
        qs: searchParams,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(403);
        expect(response.body.message).to.eq('Unauthorized: Doctor access required');
      });
    });

    it('should return empty array when doctor searches with no matching criteria', () => {
      const searchParams = {
        name: 'NonexistentCondition_' + Date.now()
      };

      cy.request({
        method: 'GET',
        url: `${baseUrl}/search`,
        headers: {
          Authorization: `Bearer ${doctorToken}`
        },
        qs: searchParams
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.be.an('array');
        expect(response.body.length).to.eq(0);
      });
    });
  });

  describe('Unauthorized Access', () => {
    it('should reject requests without authentication', () => {
      cy.request({
        method: 'GET',
        url: `${baseUrl}/search`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(401);
        expect(response.body.message).to.eq('Authentication required');
      });
    });
  });
});*/