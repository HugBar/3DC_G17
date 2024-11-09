import React, { createContext, useContext, useState, useEffect } from 'react';
import { jwtDecode } from 'jwt-decode';

const AuthContext = createContext(null);

export const AuthProvider = ({ children }) => {
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [isAdmin, setIsAdmin] = useState(false);
  const [isDoctor, setIsDoctor] = useState(false);
  const [isPatient, setIsPatient] = useState(false);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    const checkAuth = () => {
      const token = localStorage.getItem('authToken');
      if (token) {
        try {
          const decoded = jwtDecode(token);
          const role = decoded["http://schemas.microsoft.com/ws/2008/06/identity/claims/role"];
          
          setIsAuthenticated(true);
          setIsAdmin(role === 'Admin');
          setIsDoctor(role === 'Doctor');
          setIsPatient(role === 'Patient');
        } catch (error) {
          localStorage.removeItem('authToken');
          setIsAuthenticated(false);
          setIsAdmin(false);
          setIsDoctor(false);
          setIsPatient(false);
        }
      }
      setIsLoading(false);
    };

    checkAuth();
  }, []);

  const login = (token) => {
    localStorage.setItem('authToken', token);
    const decoded = jwtDecode(token);
    const role = decoded["http://schemas.microsoft.com/ws/2008/06/identity/claims/role"];
    
    setIsAuthenticated(true);
    setIsAdmin(role === 'Admin');
    setIsDoctor(role === 'Doctor');
    setIsPatient(role === 'Patient');
  };

  const logout = () => {
    localStorage.removeItem('authToken');
    setIsAuthenticated(false);
    setIsAdmin(false);
    setIsDoctor(false);
    setIsPatient(false);
  };

  return (
    <AuthContext.Provider value={{ 
      isAuthenticated, 
      isAdmin, 
      isDoctor, 
      isPatient, 
      isLoading, 
      login, 
      logout 
    }}>
      {children}
    </AuthContext.Provider>
  );
};

export const useAuth = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
};