import React from 'react';
import { Navigate } from 'react-router-dom';
import { useAuth } from '../context/AuthContext';

const ProtectedRoute = ({ children, requiredRole }) => {
  const { isAuthenticated, isAdmin, isDoctor, isPatient, isLoading } = useAuth();

  if (isLoading) {
    // You can replace this with a loading spinner component
    return <div>Loading...</div>;
  }

  if (!isAuthenticated) {
    return <Navigate to="/login" />;
  }

  const hasRequiredRole = {
    admin: isAdmin,
    doctor: isDoctor,
    patient: isPatient
  }[requiredRole];

  if (!hasRequiredRole) {
    return <Navigate to="/" />;
  }

  return children;
};

export default ProtectedRoute;