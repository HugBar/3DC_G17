describe('Allergy API E2E Tests', () => {
    const baseUrl = 'http://localhost:3001/allergies';
    let authToken;
    let testAllergenName;
  
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
  
    it('should add a new allergy to the catalog', () => {
        testAllergenName = `TestAllergen_${Date.now()}`;
      const allergy = {
        allergen: testAllergenName,
        severity: 'High',
        diagnosedDate: '2024-03-20',
        notes: 'Test allergy notes'
      };
  
      cy.request({
        method: 'POST',
        url: `${baseUrl}/add-allergy`,
        headers: { Authorization: `Bearer ${authToken}` },
        body: allergy
      }).then((response) => {
        expect(response.status).to.eq(201);
        expect(response.body.message).to.eq('Allergy added successfully');
        expect(response.body.allergy.allergen).to.eq(allergy.allergen);
      });
    });
  
    it('should handle duplicate allergy creation', () => {
      const allergy = {
        allergen: 'Peanuts',
        severity: 'High',
        diagnosedDate: '2024-03-20',
        notes: 'Common allergy'
      };
  
      cy.request({
        method: 'POST',
        url: `${baseUrl}/add-allergy`,
        headers: { Authorization: `Bearer ${authToken}` },
        body: allergy,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(409);
        expect(response.body.message).to.eq('Allergy already exists');
      });
    });
  
    it('should search allergies by allergen', () => {
      const searchAllergen = 'Peanuts';
  
      cy.request({
        method: 'GET',
        url: `${baseUrl}/search?allergen=${searchAllergen}`,
        headers: { Authorization: `Bearer ${authToken}` }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.be.an('array');
        response.body.forEach(allergy => {
          expect(allergy.allergen).to.eq(searchAllergen);
        });
      });
    });
  
    it('should search allergies by severity', () => {
      cy.request({
        method: 'GET',
        url: `${baseUrl}/search?severity=High`,
        headers: { Authorization: `Bearer ${authToken}` }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.be.an('array');
        response.body.forEach(allergy => {
          expect(allergy.severity).to.eq('High');
        });
      });
    });
  
    it('should get all allergies', () => {
      cy.request({
        method: 'GET',
        url: `${baseUrl}/getAllergyDetails`,
        headers: { Authorization: `Bearer ${authToken}` }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.be.an('array');
        expect(response.body.length).to.be.at.least(1);
      });
    });
  
    it('should handle invalid allergy data submission', () => {
      const invalidAllergy = {
        allergen: '',
        severity: '',
        diagnosedDate: '',
        notes: ''
      };
  
      cy.request({
        method: 'POST',
        url: `${baseUrl}/add-allergy`,
        headers: { Authorization: `Bearer ${authToken}` },
        body: invalidAllergy,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(500);
        expect(response.body.message).to.eq('Internal server error');
      });
    });
  
    it('should handle search with non-existent allergen', () => {
      const nonExistentAllergen = `NonExistent_${Date.now()}`;
  
      cy.request({
        method: 'GET',
        url: `${baseUrl}/search?allergen=${nonExistentAllergen}`,
        headers: { Authorization: `Bearer ${authToken}` }
      }).then((response) => {
        expect(response.status).to.eq(200);
        expect(response.body).to.be.an('array');
        expect(response.body.length).to.eq(0);
      });
    });

    it('should delete the test allergy', () => {
        cy.request({
          method: 'DELETE',
          url: `${baseUrl}/delete-allergy/${testAllergenName}`,
          headers: { Authorization: `Bearer ${authToken}` }
        }).then((response) => {
          expect(response.status).to.eq(200);
          expect(response.body.message).to.eq('Allergy deleted successfully');
        });
      });

    
  });