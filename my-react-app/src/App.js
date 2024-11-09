import React, { useState } from 'react';
import { Routes, Route, Link, Navigate, useNavigate } from 'react-router-dom';
import { useAuth } from './context/AuthContext';
import CreateStaff from './components/Staff/createStaff/CreateStaff';
import UpdateStaff from './components/Staff/updateStaff/UpdateStaff';
import StaffList from './components/Staff/staffList/StaffList';
import Login from './components/auth/Login';
import DeactivateStaff from './components/Staff/deactivateStaff/deactivateStaff';
import UpdatePatient from './components/Patient/updatePatient/UpdatePatient';
import CreateOperationRequest from './components/OperationRequest/createOperationRequest/CreateOperationRequest';
import OperationRequestList from './components/OperationRequest/listOperationRequest/OperationRequestList';
import OperationRequestDeleteConfirmation from './components/OperationRequest/deleteOperationRequest/OperationRequestDeleteConfirmation';
import NotFound from './components/notFound/NotFound';
import logo from './assets/hospital.png';
import './App.css';
import PatientList from './components/Patient/patientList/PatientList';
import DeactivatedStaffList from './components/Staff/DeactivatedStaffList/DeactivatedStaffList';
import DeletePatient from './components/Patient/deletePatient/DeletePatient';
import ProtectedRoute from './context/ProtectedRoute';


const App = () => {
  const navigate = useNavigate();
  const { isAdmin, isDoctor,isPatient, isAuthenticated, logout, } = useAuth();

  const [showStaffActions, setShowStaffActions] = useState(false);
  const [showPatientActions, setShowPatientActions] = useState(false);
  const [selectedStaffAction, setSelectedStaffAction] = useState(null);
  const [selectedPatientAction, setSelectedPatientAction] = useState(null);
  const [selectedPatientId, setSelectedPatientId] = useState(null);
  const [selectedStaffId, setSelectedStaffId] = useState(null);
  const [selectedOperationRequest, setSelectedOperationRequest] = useState(null);
  const [selectedOperationRequestId, setSelectedOperationRequestId] = useState(null);
  const [/*selectedOperationRequestIdForDetails*/, setSelectedOperationRequestIdForDetails] = useState(null);
 

  const handleHomeClick = () => {
    setShowStaffActions(false);
    setShowPatientActions(false);
    setSelectedStaffAction(null);
    setSelectedPatientAction(null);
    setSelectedStaffId(null);
    setSelectedOperationRequest(null);
    setSelectedOperationRequestId(null);
  };

  const handleSelectStaff = (staffId) => {
    setSelectedStaffId(staffId);
    setSelectedStaffAction('Update Staff');
    navigate(`/staff/update/${staffId}`);
  };
  /*const handleSelectPatient = () => {
    setSelectedPatientAction('Update Profile');
    navigate('/patient/update');
  };*/

  const resetOperationRequestAction = () => {
    setSelectedOperationRequestId(null);
    setSelectedOperationRequest(null);
    navigate('/operationrequest/filter'); // Navigate to the desired page
  };

  const resetStaffAction = () => {
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
    setShowStaffActions(true); 
    navigate('/staff/filter');
  };
  const resetPatientAction = () => {
    setSelectedPatientAction(null);
    setSelectedPatientId(null);
    setShowPatientActions(true);
    navigate('/patient/list');
  };
  const handleSelectPatientFromList = (patientId) => {
    setSelectedPatientId(patientId);
    setSelectedPatientAction('View Patient');
    navigate(`/patient/admin/edit-patient-profile/${patientId}`);
  };

  const handleSelectOperationRequestForDetails = (requestId) => {
    setSelectedOperationRequestIdForDetails(requestId);
  };
  
  /*const resetOperationRequestDetails = () => {
    setSelectedOperationRequestIdForDetails(null);
    navigate('/staff/list');
  };*/

  const handleLogoutAndReset = () => {
    logout();
    setShowStaffActions(false);
    setShowPatientActions(false);
    setSelectedStaffAction(null);
    setSelectedPatientAction(null);
    setSelectedStaffId(null);
    navigate('/');
  };

  const handleStaffClick = () => {
    setShowStaffActions(true);
    setShowPatientActions(false); // Esconde as ações de paciente
    setSelectedStaffAction(null);
    setSelectedPatientAction(null);
  };

  const handlePatientClick = () => {
    setShowPatientActions(true);
    setShowStaffActions(false);
    setSelectedPatientAction(null);
    setSelectedStaffAction(null);
  };

  const handleDeactivateStaff = (staffId) => {
    setSelectedStaffId(staffId);
    setSelectedStaffAction('Deactivate Staff');
    navigate(`/staff/deactivate/${staffId}`);
  };

  const handleDeleteOperationRequest = (requestId) => {
    setSelectedOperationRequestId(requestId);
    setSelectedOperationRequest('Delete Operation Request');
    navigate(`/operation/delete/${requestId}`);
  };

  const handleDeletePatient = (patientId) => {
    setSelectedPatientId(patientId);
    setSelectedPatientAction('Delete Patient');
    navigate(`/patient/delete/${patientId}`);
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
            <>
              {(isAdmin || isDoctor) && (
                <button 
                  onClick={handleStaffClick}
                  className={`nav-button ${showStaffActions ? 'active' : ''}`}
                >
                  Staff
                </button>
              )}
              {(isPatient || isAdmin) && (
                <button 
                  onClick={handlePatientClick}
                  className={`nav-button ${showPatientActions ? 'active' : ''}`}
                >
                  Patient
                </button>
              )}
            </>
          )}
          {isAuthenticated && (
            <button 
              onClick={handleLogoutAndReset} 
              className="nav-link logout-button"
            >
              Logout
            </button>
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
              <button
                onClick={() => {
                  setSelectedStaffAction('View Deactivated Staffs');
                  navigate('/staff/deactivated-staffs');
                }}
                className={`action-button ${selectedStaffAction === 'View Deactivated Staffs' ? 'active' : ''}`}
              >
                View Deactivated Staffs
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
                  navigate('/operationrequest/filter');
                }}
                className={`action-button ${selectedOperationRequest === 'View Operation Requests' ? 'active' : ''}`}
              >
                Manage Operation Requests
              </button>
            </>
          )}
          

        </div>
      )}
      {showPatientActions && (
        <div className="patient-action-bar">
          {isPatient && (
          <button
            onClick={() => {
              setSelectedPatientAction('Update Profile');
              navigate('/patient/update');
            }}
            className={`action-button ${selectedPatientAction === 'Update Profile' ? 'active' : ''}`}
          >
            Update Profile
          </button>
          )}
          {isAdmin && (
            <button
              onClick={() => {
                setSelectedPatientAction('View Patients');
                navigate('/patient/list');
              }}
              className={`action-button ${selectedPatientAction === 'View Patients' ? 'active' : ''}`}
            >
              View Patients
            </button>
          )}
        </div>
      )}

<Routes>
  {/* Public Routes */}
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
  
  {/* Admin Routes */}
  <Route path="/staff/create" element={
    <ProtectedRoute requiredRole="admin">
      <CreateStaff />
    </ProtectedRoute>
  } />
  <Route path="/staff/filter" element={
    <ProtectedRoute requiredRole="admin">
      <StaffList onSelectStaff={handleSelectStaff} onDeactivateStaff={handleDeactivateStaff} />
    </ProtectedRoute>
  } />
  <Route path="/staff/update/:id" element={
    <ProtectedRoute requiredRole="admin">
      <UpdateStaff staffId={selectedStaffId} onBack={resetStaffAction} />
    </ProtectedRoute>
  } />
  <Route path="/staff/deactivate/:id" element={
    <ProtectedRoute requiredRole="admin">
      <DeactivateStaff staffId={selectedStaffId} onBack={resetStaffAction} />
    </ProtectedRoute>
  } />
  <Route path="/staff/deactivated-staffs" element={
    <ProtectedRoute requiredRole="admin">
      <DeactivatedStaffList onSelectStaff={handleSelectStaff} onDeactivateStaff={handleDeactivateStaff} />
    </ProtectedRoute>
  } />
  <Route path="/patient/list" element={
    <ProtectedRoute requiredRole="admin">
      <PatientList onSelectPatient={handleSelectPatientFromList}/>
    </ProtectedRoute>
  } />
  
  {/* Doctor Routes */}
  <Route path="/operation/request" element={
    <ProtectedRoute requiredRole="doctor">
      <CreateOperationRequest />
    </ProtectedRoute>
  } />
  <Route path="/operationrequest/filter" element={
    <ProtectedRoute requiredRole="doctor">
      <OperationRequestList 
        onSelectOperationRequest={handleSelectOperationRequestForDetails} 
        onDeleteOperationRequest={handleDeleteOperationRequest} 
      />
    </ProtectedRoute>
  } />
  <Route path="/operation/delete/:id" element={
    <ProtectedRoute requiredRole="doctor">
      <OperationRequestDeleteConfirmation
        operationRequestId={selectedOperationRequestId}
        onConfirm={resetOperationRequestAction}
        onCancel={resetOperationRequestAction}
      />
    </ProtectedRoute>
  } />

  {/* Patient Routes */}
  <Route path="/patient/update" element={
    <ProtectedRoute requiredRole="patient">
      <UpdatePatient />
    </ProtectedRoute>
  } />
  
  {/* Admin Patient Routes */}
  <Route path="/patient/admin/edit-patient-profile/:id" element={
    <ProtectedRoute requiredRole="admin">
      <UpdatePatient patientId={selectedPatientId} onBack={resetPatientAction} />
    </ProtectedRoute>
  } />

  {/* Catch-all route for 404 */}
  <Route path="*" element={<NotFound />} />
  <Route path="/patient/list" element={
    isAdmin ? <PatientList onSelectPatient={handleSelectPatientFromList}/> : <Navigate to="/" />
  } />
  {
  <Route path="/patient/delete/:id" element={isAdmin ? (<DeletePatient patientId={selectedPatientId} onDelete={handleDeletePatient} />) : <Navigate to="/" />} />
  }
</Routes>

    </div>
  );
  
};

export default App;
