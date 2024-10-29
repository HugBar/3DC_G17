describe('User API E2E Tests', () => {

  const baseUrl = 'https://localhost:5001/api/user';

  let userId; 

  // 1. Test creating a new user
  it('should create a new user', () => {
    cy.request('POST', `${baseUrl}/register`, {
      username: 'CypressUser1000034',  // Test data
      email: 'cypress1000034@example.com',
      password: 'Password123!',
      role: 'Admin'
    }).then((response) => {
      expect(response.status).to.eq(201); 
      expect(response.body).to.have.property('id');  
      expect(response.body).to.have.property('userName', 'CypressUser1000034');  
      expect(response.body).to.have.property('email', 'cypress1000034@example.com'); 
      expect(response.body).to.have.property('role', 'Admin');  

      userId = response.body.id;  
    });
  });

  // 2. Test retrieving the newly created user by ID
  it('should retrieve the user by ID', () => {
    cy.request('GET', `${baseUrl}/${userId}`).then((response) => {
      expect(response.status).to.eq(200);  
      expect(response.body).to.have.property('id', userId);  
      expect(response.body).to.have.property('userName', 'CypressUser1000034');  
      expect(response.body).to.have.property('email', 'cypress1000034@example.com');  
      expect(response.body).to.have.property('role', 'Admin');  
    });
  });

  // 3. Test updating the user successfully
  it('should update the user by ID', () => {
    const updatedUser = {
      userName: 'UpdatedCypressUser1000034',
      email: 'updatedcypress1000034@example.com',
      newRole: 'Doctor' 
    };
  
    cy.request('PUT', `${baseUrl}/${userId}`, updatedUser).then((response) => {
      expect(response.status).to.eq(200);  
      expect(response.body).to.have.property('userName', updatedUser.userName);  
      expect(response.body).to.have.property('email', updatedUser.email);  
      expect(response.body).to.have.property('role', updatedUser.newRole);  
    });
  });

    // 5. Test deleting the user 
    it('should delete the user by ID', () => {
      cy.request('DELETE', `${baseUrl}/${userId}`).then((response) => {
        expect(response.status).to.eq(200);  
      });
    });

});
