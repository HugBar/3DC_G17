import React, { useState } from 'react';
import { Routes, Route, Link, Navigate, useNavigate } from 'react-router-dom';
import { useAuth } from './context/AuthContext';
import CreateStaff from './components/createStaff/CreateStaff';
import UpdateStaff from './components/updateStaff/UpdateStaff';
import StaffList from './components/staffList/StaffList';
import Login from './components/auth/Login';
import DeactivateStaff from './components/deactivateStaff/deactivateStaff';
import UpdatePatient from './components/updatePatient/UpdatePatient';
import CreateOperationRequest from './components/createOperationRequest/CreateOperationRequest';
import OperationRequestList from './components/listOperationRequest/OperationRequestList';
import OperationRequestDeleteConfirmation from './components/deleteOperationRequest/OperationRequestDeleteConfirmation';
import OperationRequestDetails from './components/consultOperationRequest/OperationRequestDetails';
import NotFound from './components/notFound/NotFound';
import logo from './assets/hospital.png';
import './App.css';
import PatientList from './components/patientList/PatientList';
import DeactivatedStaffList from './components/DeactivatedStaffList/DeactivatedStaffList';


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
  const [selectedOperationRequestIdForDetails, setSelectedOperationRequestIdForDetails] = useState(null);
 

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
  const handleSelectPatient = () => {
    setSelectedPatientAction('Update Profile');
    navigate('/patient/update');
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
    setShowStaffActions(true); 
    navigate('/staff/filter');
  };
  const resetPatientAction = () => {
    setSelectedPatientAction(null);
    setShowPatientActions(false);
  };
  const handleSelectPatientFromList = (patientId) => {
    setSelectedPatientId(patientId);
    setSelectedPatientAction('View Patient');
    navigate(`/patient/view/${patientId}`);
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
    setShowStaffActions(false); // Esconde as ações de staff
    setSelectedPatientAction(null);
    setSelectedStaffAction(null);
  };

  const handleDeactivateStaff = (staffId) => {
    setSelectedStaffId(staffId);
    setSelectedStaffAction('Deactivate Staff');
    navigate(`/staff/deactivate/${staffId}`);
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
    isAdmin ? <StaffList onSelectStaff={handleSelectStaff} onDeactivateStaff={handleDeactivateStaff} /> : <Navigate to="/" />
  } />
  <Route path="/staff/update/:id" element={
    isAdmin ? <UpdateStaff staffId={selectedStaffId} onBack={resetStaffAction} /> : <Navigate to="/" />
  } />
  <Route path="/staff/deactivate/:id" element={
    isAdmin ? <DeactivateStaff staffId={selectedStaffId} onBack={resetStaffAction} /> : <Navigate to="/" />
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
  <Route path="/staff/deactivate/:id" element={
    isAdmin ? <DeactivateStaff staffId={selectedStaffId} onBack={resetStaffAction} /> : <Navigate to="/" />
  } />
  <Route path="/staff/deactivated-staffs" element={
    isAdmin ? <DeactivatedStaffList onSelectStaff={handleSelectStaff} onDeactivateStaff={handleDeactivateStaff} /> : <Navigate to="/" />
  } />
  {/* Rotas para Pacientes */}
      <Route path="/patient/update" element={
      isAuthenticated && isPatient ? <UpdatePatient /> : <Navigate to="/" />
  } />
  <Route path="*" element={<NotFound />} />
  <Route path="/patient/list" element={
    isAdmin ? <PatientList onSelectPatient={handleSelectPatientFromList}/> : <Navigate to="/" />
  } />
</Routes>

    </div>
  );
  
};

export default App;
