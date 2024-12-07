describe('Specialization API E2E Tests', () => {
  const baseUrl = 'http://localhost:3001/api/specializations';
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

  it('should create a new specialization with unique name', () => {
    const uniqueName = `TestSpecialization_${Date.now()}`;
    const specialization = {
      name: uniqueName,
      description: 'Test specialization description'
    };

    cy.request({
      method: 'POST',
      url: `${baseUrl}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: specialization
    }).then((response) => {
      expect(response.status).to.eq(201);
      expect(response.body.specialization.name).to.eq(uniqueName);
    });
  });

  it('should handle duplicate specialization creation', () => {
    // Using an existing specialization from the database
    const specialization = {
      name: 'Cardiology',
      description: 'Deals with heart conditions'
    };

    cy.request({
      method: 'POST',
      url: `${baseUrl}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: specialization,
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(409);
      expect(response.body.message).to.eq('Specialization already exists');
    });
  });

  it('should get all specializations', () => {
    cy.request({
      method: 'GET',
      url: `${baseUrl}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      }
    }).then((response) => {
      expect(response.status).to.eq(200);
      expect(response.body).to.be.an('array');
      expect(response.body.length).to.be.at.least(3); // We know there are at least 3 specializations
      
      // Verify known specializations exist
      const specializations = response.body.map(spec => spec.name);
      expect(specializations).to.include('Cardiology');
      expect(specializations).to.include('Neurology');
    });
  });

  it('should handle validation errors', () => {
    const invalidSpecialization = {
      name: '',
      description: ''
    };

    cy.request({
      method: 'POST',
      url: `${baseUrl}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: invalidSpecialization,
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(500);
      expect(response.body).to.have.property('message', 'Internal server error');
    });
  });

  it('should get specialization by ID', () => {
    const uniqueName = `TestSpecialization_${Date.now()}`;
    
    // First create a specialization
    cy.request({
      method: 'POST',
      url: `${baseUrl}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: {
        name: uniqueName,
        description: 'Test description'
      }
    }).then((response) => {
      expect(response.status).to.eq(201);
      const specializationId = response.body.specialization._id;

      // Now get the specialization by ID
      cy.request({
        method: 'GET',
        url: `${baseUrl}/${specializationId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        }
      }).then((getResponse) => {
        expect(getResponse.status).to.eq(200);
        expect(getResponse.body.name).to.eq(uniqueName);
      });
    });
  });

  it('should handle non-existent specialization ID', () => {
    cy.request({
      method: 'GET',
      url: `${baseUrl}/507f1f77bcf86cd799439011`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(404);
      expect(response.body.message).to.eq('Specialization not found');
    });
  });

  it('should update a specialization successfully', () => {
    const uniqueName = `TestSpecialization_${Date.now()}`;
    
    // First create a specialization
    cy.request({
      method: 'POST',
      url: `${baseUrl}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: {
        name: uniqueName,
        description: 'Initial description'
      }
    }).then((createResponse) => {
      expect(createResponse.status).to.eq(201);
      const specializationId = createResponse.body.specialization._id;

      // Now update the specialization
      const updatedData = {
        name: `Updated_${uniqueName}`,
        description: 'Updated description'
      };

      cy.request({
        method: 'PUT',
        url: `${baseUrl}/${specializationId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        body: updatedData
      }).then((updateResponse) => {
        expect(updateResponse.status).to.eq(200);
        expect(updateResponse.body.message).to.eq('Specialization updated successfully');
        expect(updateResponse.body.specialization.name).to.eq(updatedData.name);
        expect(updateResponse.body.specialization.description).to.eq(updatedData.description);
      });
    });
  });

  it('should handle duplicate name during update', () => {
    const uniqueName = `TestSpecialization_${Date.now()}`;
    
    // First create a specialization
    cy.request({
      method: 'POST',
      url: `${baseUrl}`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: {
        name: uniqueName,
        description: 'Test description'
      }
    }).then((createResponse) => {
      const specializationId = createResponse.body.specialization._id;

      // Try to update with an existing name (Cardiology)
      cy.request({
        method: 'PUT',
        url: `${baseUrl}/${specializationId}`,
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        body: {
          name: 'Cardiology',
          description: 'Updated description'
        },
        failOnStatusCode: false
      }).then((updateResponse) => {
        expect(updateResponse.status).to.eq(409);
        expect(updateResponse.body.message).to.eq('Specialization with this name already exists');
      });
    });
  });

  it('should handle update of non-existent specialization', () => {
    cy.request({
      method: 'PUT',
      url: `${baseUrl}/507f1f77bcf86cd799439011`,
      headers: {
        Authorization: `Bearer ${authToken}`
      },
      body: {
        name: 'New Name',
        description: 'New description'
      },
      failOnStatusCode: false
    }).then((response) => {
      expect(response.status).to.eq(404);
      expect(response.body.message).to.eq('Specialization not found');
    });
  });

});