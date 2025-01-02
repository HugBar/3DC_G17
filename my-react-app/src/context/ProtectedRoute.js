import React from 'react';
import { Navigate } from 'react-router-dom';
import { useAuth } from '../context/AuthContext';

const ProtectedRoute = ({ children, requiredRole }) => {
  const { isAuthenticated, isAdmin, isDoctor, isNurse, isLoading } = useAuth();

  if (isLoading) {
    return <div>Loading...</div>;
  }

  if (!isAuthenticated) {
    return <Navigate to="/login" />;
  }

  const roleMap = {
    admin: isAdmin,
    doctor: isDoctor,
    nurse: isNurse,
  };

  const hasRequiredRole = Array.isArray(requiredRole)
    ? requiredRole.some(role => roleMap[role])
    : roleMap[requiredRole];

  if (!hasRequiredRole) {
    return <Navigate to="/" />;
  }

  return children;
};

export default ProtectedRoute;