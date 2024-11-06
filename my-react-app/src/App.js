import React, { useState } from 'react';
import { Routes, Route, Link, Navigate, useNavigate } from 'react-router-dom';
import { useAuth } from './context/AuthContext';
import CreateStaff from './components/createStaff/CreateStaff';
import UpdateStaff from './components/updateStaff/UpdateStaff';
import StaffList from './components/staffList/StaffList';
import Login from './components/auth/Login';
import CreateOperationRequest from './components/createOperationRequest/CreateOperationRequest';
import OperationRequestList from './components/listOperationRequest/OperationRequestList';
import OperationRequestDeleteConfirmation from './components/deleteOperationRequest/OperationRequestDeleteConfirmation';
import OperationRequestDetails from './components/consultOperationRequest/OperationRequestDetails';

import NotFound from './components/notFound/NotFound';
import logo from './assets/hospital.png';
import './App.css';

const App = () => {
  const navigate = useNavigate();
  const { isAdmin, isDoctor, isAuthenticated, logout } = useAuth();

  const [showStaffActions, setShowStaffActions] = useState(false);
  const [selectedStaffAction, setSelectedStaffAction] = useState(null);
  const [selectedStaffId, setSelectedStaffId] = useState(null);
  const [selectedOperationRequest, setSelectedOperationRequest] = useState(null);
  const [selectedOperationRequestId, setSelectedOperationRequestId] = useState(null);
  const [selectedOperationRequestIdForDetails, setSelectedOperationRequestIdForDetails] = useState(null);
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

  const handleLogin = (token) => {
    setAuthToken(token);
    localStorage.setItem('authToken', token);
  };

  const handleLogout = () => {
    setAuthToken(null);
    localStorage.removeItem('authToken');
    setShowStaffActions(false);
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
    setSelectedOperationRequest(null);
    setSelectedOperationRequestId(null);
    setIsAdmin(false);
    setIsDoctor(false);
  };

  const isAuthenticated = () => !!authToken;

  const handleHomeClick = () => {
    setShowStaffActions(false);
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
    setSelectedOperationRequest(null);
    setSelectedOperationRequestId(null);
  };

  const handleSelectStaff = (staffId) => {
    setSelectedStaffId(staffId);
    setSelectedStaffAction('Update Staff');
    navigate(`/staff/update/${staffId}`);
  };

  const handleSelectOperationRequestForDeletion = (requestId) => {
    setSelectedOperationRequestId(requestId);
    setSelectedOperationRequest('Delete Operation Requests');
  };

  const resetOperationRequestAction = () => {
    setSelectedOperationRequest(null);
    setSelectedOperationRequestId(null);
    setShowStaffActions(true);
  };

  const handleSelectOperationRequestForDeletion = (requestId) => {
    setSelectedOperationRequestId(requestId);
    setSelectedOperationRequest('Delete Operation Requests');
  };

  const resetOperationRequestAction = () => {
    setSelectedOperationRequest(null);
    setSelectedOperationRequestId(null);
    setShowStaffActions(true);
  };

  const resetStaffAction = () => {
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
    setShowStaffActions(true); // Show the action bar again if needed
  };

  const handleSelectOperationRequestForDetails = (requestId) => {
    setSelectedOperationRequestIdForDetails(requestId);
  };
  
  const resetOperationRequestDetails = () => {
    setSelectedOperationRequestIdForDetails(null);
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
