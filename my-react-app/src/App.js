import React, { useState } from 'react';
import { Routes, Route, Link, Navigate, useNavigate } from 'react-router-dom';
import { useAuth } from './context/AuthContext';
import CreateStaff from './components/createStaff/CreateStaff.js';
import UpdateStaff from './components/updateStaff/UpdateStaff.js';
import StaffList from './components/staffList/StaffList.js';
import Login from './components/auth/Login';
import CreateOperationRequest from './components/createOperationRequest/CreateOperationRequest.js';
import NotFound from './components/notFound/NotFound';
import logo from './assets/hospital.png';
import './App.css';

const App = () => {
  const navigate = useNavigate();
  const { isAdmin, isDoctor, isAuthenticated, logout } = useAuth();

  const [showStaffActions, setShowStaffActions] = useState(false);
  const [selectedStaffAction, setSelectedStaffAction] = useState(null);
  const [selectedStaffId, setSelectedStaffId] = useState(null);

  const handleHomeClick = () => {
    setShowStaffActions(false);
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
    navigate('/');
  };

  const handleSelectStaff = (staffId) => {
    setSelectedStaffId(staffId);
    setSelectedStaffAction('Update Staff');
    navigate(`/staff/update/${staffId}`);
  };

  const resetStaffAction = () => {
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
    setShowStaffActions(true);
    navigate('/staff/list');
  };

  const handleLogoutAndReset = () => {
    logout();
    setShowStaffActions(false);
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
    navigate('/');
  };

  return (
    <div>
      <header className="app-header">
        <div className="logo-title">
          <img src={logo} alt="Hospital Logo" className="app-logo" />
          <h1 className="app-title">Hospital Management App</h1>
        </div>
        <nav className="app-nav">
          <Link to="/" onClick={handleHomeClick} className="nav-link">Home</Link>
          {isAuthenticated && (
            <button 
              onClick={() => setShowStaffActions(!showStaffActions)} 
              className="nav-link"
            >
              Staff
            </button>
          )}
          {isAuthenticated ? (
            <button 
              onClick={handleLogoutAndReset} 
              className="nav-link logout-button"
            >
              Logout
            </button>
          ) : (
            <Link to="/login" className="nav-link">Login</Link>
          )}
        </nav>
      </header>

      {showStaffActions && (
        <div className="staff-action-bar">
          {isAdmin && (
            <>
              <button
                onClick={() => {
                  setSelectedStaffAction('Create Staff');
                  navigate('/staff/create');
                }}
                className={`action-button ${selectedStaffAction === 'Create Staff' ? 'active' : ''}`}
              >
                Create Staff
              </button>
              <button
                onClick={() => {
                  setSelectedStaffAction('Update Staff');
                  navigate('/staff/list');
                }}
                className={`action-button ${selectedStaffAction === 'Update Staff' ? 'active' : ''}`}
              >
                Update Staff
              </button>
            </>
          )}
          {isDoctor && (
            <button
              onClick={() => {
                setSelectedStaffAction('Request Operation');
                navigate('/operation/request');
              }}
              className={`action-button ${selectedStaffAction === 'Request Operation' ? 'active' : ''}`}
            >
              Request Operation
            </button>
          )}
        </div>
      )}

      <Routes>
        <Route path="/" element={
          <div className="main-content">
            <h1 className="welcome-title">Welcome to<br/>Hospital Management</h1>
            <p className="welcome-subtitle">Your Trusted Healthcare Management Solution</p>
            {!isAuthenticated && (
              <Link to="/login" className="nav-link">
                Login to Access the System
              </Link>
            )}
          </div>
        } />
        <Route path="/login" element={<Login />} />
        <Route path="/staff/create" element={
          isAdmin ? <CreateStaff /> : <Navigate to="/" />
        } />
        <Route path="/staff/list" element={
          isAdmin ? <StaffList onSelectStaff={handleSelectStaff} /> : <Navigate to="/" />
        } />
        <Route path="/staff/update/:id" element={
          isAdmin ? <UpdateStaff staffId={selectedStaffId} onBack={resetStaffAction} /> : <Navigate to="/" />
        } />
        <Route path="/operation/request" element={
          isDoctor ? <CreateOperationRequest /> : <Navigate to="/" />
        } />
        <Route path="*" element={<NotFound />} />
      </Routes>
    </div>
  );
};

export default App;
