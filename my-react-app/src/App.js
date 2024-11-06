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
                  navigate('/staff/filter');
                }}
                className={`action-button ${selectedStaffAction === 'Update Staff' ? 'active' : ''}`}
              >
                View Staffs
              </button>
            </>
          )}
          {isDoctor && (
            <>
              <button
                onClick={() => {
                  setSelectedOperationRequest('Request Operation');
                  navigate('/operation/request');
                }}
                className={`action-button ${selectedOperationRequest === 'Request Operation' ? 'active' : ''}`}
              >
                Request Operation
              </button>
              <button
                onClick={() => {
                  setSelectedOperationRequest('View Operation Requests');
                  navigate('/operation/view');
                }}
                className={`action-button ${selectedOperationRequest === 'View Operation Requests' ? 'active' : ''}`}
              >
                View Operation Requests
              </button>
              <button
                onClick={() => {
                  setSelectedOperationRequest('Delete Operation Requests');
                  navigate('/operation/delete');
                }}
                className={`action-button ${selectedOperationRequest === 'Delete Operation Requests' ? 'active' : ''}`}
              >
                Delete Operation Requests
              </button>
            </>
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
  
  {/* Staff Routes */}
  <Route path="/staff/create" element={
    isAdmin ? <CreateStaff /> : <Navigate to="/" />
  } />
  <Route path="/staff/filter" element={
    isAdmin ? <StaffList onSelectStaff={handleSelectStaff} /> : <Navigate to="/" />
  } />
  <Route path="/staff/update/:id" element={
    isAdmin ? <UpdateStaff staffId={selectedStaffId} onBack={resetStaffAction} /> : <Navigate to="/" />
  } />
  
  {/* Operation Request Routes */}
  <Route path="/operation/request" element={
    isDoctor ? <CreateOperationRequest /> : <Navigate to="/" />
  } />
  <Route path="/operation/view" element={
    isDoctor ? (
      selectedOperationRequestIdForDetails ? (
        <OperationRequestDetails
          operationRequestId={selectedOperationRequestIdForDetails}
          onBack={resetOperationRequestDetails}
        />
      ) : (
        <OperationRequestList onSelectOperationRequest={handleSelectOperationRequestForDetails} />
      )
    ) : <Navigate to="/" />
  } />
  <Route path="/operation/delete" element={
    isDoctor ? (
      selectedOperationRequestId ? (
        <OperationRequestDeleteConfirmation
          operationRequestId={selectedOperationRequestId}
          onConfirm={resetOperationRequestAction}
          onCancel={resetOperationRequestAction}
        />
      ) : (
        <OperationRequestList onSelectOperationRequest={handleSelectOperationRequestForDeletion} />
      )
    ) : <Navigate to="/" />
  } />
  
  <Route path="*" element={<NotFound />} />
</Routes>

    </div>
  );
  
};

export default App;
