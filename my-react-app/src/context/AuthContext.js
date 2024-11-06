import { createContext, useState, useContext, useEffect } from 'react';
import { jwtDecode } from 'jwt-decode';

const AuthContext = createContext(null);

export const AuthProvider = ({ children }) => {
  const [authToken, setAuthToken] = useState(localStorage.getItem('authToken'));
  const [isAdmin, setIsAdmin] = useState(false);
  const [isDoctor, setIsDoctor] = useState(false);

  useEffect(() => {
    if (authToken) {
      const decodedToken = jwtDecode(authToken);
      const role = decodedToken["http://schemas.microsoft.com/ws/2008/06/identity/claims/role"];
      setIsAdmin(role === 'Admin');
      setIsDoctor(role === 'Doctor');
    } else {
      setIsAdmin(false);
      setIsDoctor(false);
    }
  }, [authToken]);

  const login = (token) => {
    setAuthToken(token);
    localStorage.setItem('authToken', token);
  };

  const logout = () => {
    setAuthToken(null);
    localStorage.removeItem('authToken');
  };

  const isAuthenticated = !!authToken;

  return (
    <AuthContext.Provider value={{
      authToken,
      isAdmin,
      isDoctor,
      login,
      logout,
      isAuthenticated
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