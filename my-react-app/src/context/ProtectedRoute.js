import React from 'react';
import { Navigate } from 'react-router-dom';
import { useAuth } from '../context/AuthContext';

const ProtectedRoute = ({ children, requiredRole }) => {
  console.log('ProtectedRoute rendered with role:', requiredRole);
  const { isAuthenticated, isAdmin, isDoctor, isNurse, isPatient, isLoading } = useAuth();
  console.log('Auth state in ProtectedRoute:', isAuthenticated, isAdmin, isDoctor, isNurse, isPatient);

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
    patient: isPatient
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