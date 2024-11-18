// src/components/auth/Login.js
import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../../context/AuthContext';
import { login as loginApi, googleLogin } from '../../api/auth';

import './Login.css';

const Login = () => {
  const { login } = useAuth();
  const [credentials, setCredentials] = useState({ email: '', password: '' });
  const [errorMessage, setErrorMessage] = useState('');
  const navigate = useNavigate();

  const handleChange = (e) => {
    const { name, value } = e.target;
    setCredentials((prev) => ({ ...prev, [name]: value }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      const response = await loginApi(credentials);
      if (response) {
        setCredentials({ email: '', password: '' });
        login(response);
        navigate('/');
      }
    } catch (error) {
      setErrorMessage('Error logging in.');
      console.error('Login error:', error);
    }
  };

  const handleGoogleLogin = () => {
    try {
      googleLogin();
    } catch (error) {
      setErrorMessage('Error initiating Google login.');
      console.error('Google login error:', error);
    }
  };

  return (
    <div className="login-container">
      <h2 className="login-title">Login</h2>
      {errorMessage && <p className="login-error">{errorMessage}</p>}
      <form onSubmit={handleSubmit} className="login-form">
        <div className="form-group">
          <label htmlFor="email">Email:</label>
          <input
            id="email"
            type="email"
            name="email"
            value={credentials.email}
            onChange={handleChange}
            required
            className="login-input"
          />
        </div>
        <div className="form-group">
          <label htmlFor="password">Password:</label>
          <input
            id="password"
            type="password"
            name="password"
            value={credentials.password}
            onChange={handleChange}
            required
            className="login-input"
          />
        </div>
        <button type="submit" className="login-button">Login</button>
        
        <div className="separator">
          <span>OR</span>
        </div>

        <button 
          type="button" 
          onClick={handleGoogleLogin}
          className="google-login-button"
        >
          <img 
            src="https://upload.wikimedia.org/wikipedia/commons/5/53/Google_%22G%22_Logo.svg" 
            alt="Google logo" 
            className="google-icon"
          />
          Sign in with Google
        </button>
      </form>
    </div>
  );
};

export default Login;
