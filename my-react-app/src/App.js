import React, { useState, useEffect } from 'react';
import { BrowserRouter as Router, Route, Routes, Link } from 'react-router-dom';
import { jwtDecode } from 'jwt-decode'; // Use named import
import CreateStaff from './components/createStaff/CreateStaff';
import UpdateStaff from './components/updateStaff/UpdateStaff';
import StaffList from './components/staffList/StaffList';
import Login from './components/auth/Login';
import CreateOperationRequest from './components/createOperationRequest/CreateOperationRequest';
import OperationRequestList from './components/listOperationRequest/OperationRequestList';
import OperationRequestDeleteConfirmation from './components/deleteOperationRequest/OperationRequestDeleteConfirmation';
import OperationRequestDetails from './components/consultOperationRequest/OperationRequestDetails';

import logo from './assets/hospital.png';
import './App.css';

const App = () => {
  const [authToken, setAuthToken] = useState(localStorage.getItem('authToken'));
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
  };

  return (
    <Router>
      <div>
        <header className="app-header">
          <div className="logo-title">
            <img src={logo} alt="Hospital Logo" className="app-logo" />
            <h1 className="app-title">Hospital Management App</h1>
          </div>
          <nav className="app-nav">
            <Link to="/" onClick={handleHomeClick} className="nav-link">Home</Link>
            {isAuthenticated() && (
              <button onClick={() => setShowStaffActions(!showStaffActions)} className="nav-link">
                Staff
              </button>
            )}
            {isAuthenticated() ? (
              <button onClick={handleLogout} className="nav-link logout-button">Logout</button>
            ) : (
              <Link to="/login" className="nav-link">Login</Link>
            )}
          </nav>
        </header>
  
        {/* Staff Action Bar Below Header */}
        {showStaffActions && (
          <div className="staff-action-bar">
            {isAdmin && (
              <>
                <button
                  onClick={() => setSelectedStaffAction('Create Staff')}
                  className={`action-button ${selectedStaffAction === 'Create Staff' ? 'active' : ''}`}
                >
                  Create Staff
                </button>
                <button
                  onClick={() => setSelectedStaffAction('Update Staff')}
                  className={`action-button ${selectedStaffAction === 'Update Staff' ? 'active' : ''}`}
                >
                  Update Staff
                </button>
              </>
            )}
            {isDoctor && (
              <>
                <button
                  onClick={() => setSelectedOperationRequest('Request Operation')}
                  className={`action-button ${selectedOperationRequest === 'Request Operation' ? 'active' : ''}`}
                >
                  Request Operation
                </button>
                <button
                  onClick={() => setSelectedOperationRequest('View Operation Requests')}
                  className={`action-button ${selectedOperationRequest === 'View Operation Requests' ? 'active' : ''}`}
                >
                  View Operation Requests
                </button>
                <button
                  onClick={() => setSelectedOperationRequest('Delete Operation Requests')}
                  className={`action-button ${selectedOperationRequest === 'Delete Operation Requests' ? 'active' : ''}`}
                >
                  Delete Operation Requests
                </button>
              </>
            )}
          </div>
        )}
  
        <main className="main-content">
          <Routes>
            <Route
              path="/"
              element={
                selectedStaffAction === null && selectedOperationRequest === null ? (
                  <div>
                    <h1 className="welcome-title">Welcome to<br/>Hospital Management</h1>
                    <p className="welcome-subtitle">Your Healthcare Administration Solution</p>
                  </div>
                ) : null
              }
            />
            <Route path="/login" element={<Login onLogin={handleLogin} />} />
          </Routes>
  
          {/* Conditionally Render Components Based on Action */}
          {selectedStaffAction === 'Create Staff' && <CreateStaff />}
          {selectedStaffAction === 'Update Staff' && (
            selectedStaffId ? (
              <UpdateStaff staffId={selectedStaffId} onBack={resetStaffAction} />
            ) : (
              <StaffList onSelectStaff={handleSelectStaff} />
            )
          )}
          {selectedOperationRequest === 'Request Operation' && <CreateOperationRequest />}
          {selectedOperationRequest === 'View Operation Requests' && (
            selectedOperationRequestIdForDetails ? (
              <OperationRequestDetails
                operationRequestId={selectedOperationRequestIdForDetails}
                onBack={resetOperationRequestDetails}
              />
            ) : (
              <OperationRequestList onSelectOperationRequest={handleSelectOperationRequestForDetails} />
            )
          )}
          {selectedOperationRequest === 'Delete Operation Requests' && (
            selectedOperationRequestId ? (
              <OperationRequestDeleteConfirmation
                operationRequestId={selectedOperationRequestId}
                onConfirm={resetOperationRequestAction}
                onCancel={resetOperationRequestAction}
              />
            ) : (
              <OperationRequestList onSelectOperationRequest={handleSelectOperationRequestForDeletion} />
            )
          )}
        </main>
      </div>
    </Router>
  );
  
};

export default App;
