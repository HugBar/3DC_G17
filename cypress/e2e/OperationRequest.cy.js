describe('Operation Request API E2E Tests with Authentication', () => {

    const baseUrl = 'https://localhost:5001/api/operationrequest';
    let requestId;  // This will store the requestId for later use in tests
    let authToken;  // This will store the authentication token
  
    // Before all tests, authenticate and get a token
    before(() => {
      cy.request('POST', 'https://localhost:5001/api/auth/login', {
        email: 'doctor@doctor.com',  // Replace with valid credentials
        password: 'Doctor123!'
      }).then((response) => {
        expect(response.status).to.eq(200);  // Check that the login was successful
        authToken = `Bearer ${response.body.token}`;  // Store the token in a Bearer format
      });
    });
  
  // 1. Test creating a new operation request
  it('should create a new operation request (with authorization)', () => {
    cy.request({
      method: 'POST',
      url: `${baseUrl}/create-operation-request`,
      headers: {
        Authorization: authToken  // Using the token obtained from the login
      },
      body: {
        patientId: '9e6f3cfd-2219-4085-b83f-03eb397fcb71',  // Example Patient ID
        doctorId: '42a99ecd-85f9-4acc-b57c-9ad9a48a0c1a',  // Example Doctor ID
        operationTypeId: 'aa9c46a4-f6c4-4283-ae9d-d59bbc13171c',  // Example Operation Type ID
        deadline: '2024-12-01T00:00:00Z',  // Sample future deadline
        priority: 'urgent'  // Valid priority value
      }
    }).then((response) => {
      expect(response.status).to.eq(200);  // Check that creation is successful
      expect(response.body).to.have.property('id');  // Ensure the response contains an ID
      requestId = response.body.id;  // Store the requestId for later use
    });
  });
  
   // 2. Test updating the operation request (with authorization) using JSON Patch
it('should update the operation request by ID (with authorization)', () => {
    const patchOperations = [
      {
        op: 'replace',
        path: '/operationTypeId',
        value: 'b0490ea6-ea92-4a9e-abdd-7f78e9d2800f'  // New operation type ID
      },
      {
        op: 'replace',
        path: '/deadline',
        value: '2025-01-15T00:00:00Z'  // New future deadline
      },
      {
        op: 'replace',
        path: '/priority',
        value: 'elective'  // New valid priority value
      }
    ];
  
    cy.request({
      method: 'PATCH',
      url: `${baseUrl}/${requestId}`,
      headers: {
        Authorization: authToken,  // Using the token
        'Content-Type': 'application/json-patch+json'  // Specifying JSON Patch content type
      },
      body: patchOperations  // Using the JSON Patch structure
    }).then((response) => {
      expect(response.status).to.eq(200);  // Check that the update was successful
      expect(response.body).to.have.property('operationTypeId', 'b0490ea6-ea92-4a9e-abdd-7f78e9d2800f');  // Validate updated operation type
      expect(response.body).to.have.property('deadline', '2025-01-15T00:00:00Z');  // Validate updated deadline
      expect(response.body).to.have.property('priority', 'elective');  // Validate updated priority
    });
  });

  // Test searching for operation requests with filter criteria (with authorization)
it('should search for operation requests based on filter criteria (with authorization)', () => {
    const searchCriteria = {
      patientId: '9e6f3cfd-2219-4085-b83f-03eb397fcb71',  // Example patient ID to search by
      operationTypeId: 'b0490ea6-ea92-4a9e-abdd-7f78e9d2800f',  // Example operation type ID to search by
      priority: 'elective'  // Example priority to search by
    };
  
    cy.request({
      method: 'GET',
      url: `${baseUrl}/filter`,  // The filter endpoint
      headers: {
        Authorization: authToken  // Using the token obtained from the login
      },
      qs: searchCriteria  // Pass the search criteria as query parameters
    }).then((response) => {
      expect(response.status).to.eq(200);  // Ensure the request is successful
      expect(response.body).to.be.an('array');  // Validate that the response is an array
      expect(response.body.length).to.be.greaterThan(0);  // Ensure there are some results
  
      // Optionally, validate some specific properties of the first result
      const firstRequest = response.body[0];
      expect(firstRequest).to.have.property('patientId', searchCriteria.patientId);  // Validate patientId
      expect(firstRequest).to.have.property('operationTypeId', searchCriteria.operationTypeId);  // Validate operation type
      expect(firstRequest).to.have.property('priority', searchCriteria.priority);  // Validate priority
    });
  });
  
  
  
// 3. Test deleting the operation request (with authorization)
    it('should delete the operation request by ID (with authorization)', () => {
    cy.request({
      method: 'DELETE',
      url: `${baseUrl}/delete-operation-request/${requestId}`,  // Adjusted URL to match the route
      headers: {
        Authorization: authToken  // Using the token
      }
    }).then((response) => {
      expect(response.status).to.eq(200);  // Check that the deletion was successful
    });
    });
  
  });
  