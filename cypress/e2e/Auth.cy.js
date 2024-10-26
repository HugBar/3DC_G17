describe('Auth API E2E Tests', () => {

    const baseUrl = 'https://localhost:5001/api/auth';
    let authToken;
    let userToken;

    // Teste de login
    it('should login successfully and return a token', () => {
        cy.request('POST', `${baseUrl}/login`, {
            email: 'admin@admin.com',  // Substitua por credenciais válidas
            password: 'Admin123!'
        }).then((response) => {
            expect(response.status).to.eq(200);
            expect(response.body).to.have.property('token');
            authToken = response.body.Token;
        });
    });

    // Teste de logout
    it('should logout successfully', () => {
        cy.request({
            method: 'POST',
            url: `${baseUrl}/logout`,
            headers: {
                Authorization: `Bearer ${authToken}`
            }
        }).then((response) => {
            expect(response.status).to.eq(200);
        });
    });

    it('should send password reset email', () => {
        cy.request('POST', `${baseUrl}/reset-password`, {
            email: 'admin@admin.com'
        }).then((response) => {
            expect(response.status).to.eq(200);
            expect(response.body).to.eq('Password reset email sent.');
            
        });
    });

    /*it('should confirm password reset successfully', () => {
        cy.request({
            method: 'POST',
            url: `${baseUrl}/confirm-reset-password`,
            body: {
                email: 'admin@admin.com',
                token: userToken,
                newPassword: 'NewAdmin123!'
            },
            failOnStatusCode: false  // Permite que o teste continue mesmo se o status não for 2xx
        }).then((response) => {
            cy.log('Response:', response);
            expect(response.status).to.eq(200);
            expect(response.body).to.eq('Password has been reset successfully.');
        });
    });*/

});




