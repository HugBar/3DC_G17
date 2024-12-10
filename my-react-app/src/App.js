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
import DeleteAccount from './components/Patient/deleteAccount/DeleteAccount';
import CreatePatient from './components/Patient/createPatient/CreatePatient';
import Hospital3DView from './components/Hospital/Hospital3DView';
import AppointmentScheduler from './components/Appointments/AppointmentScheduler';
import AuthCallback from './components/auth/AuthCallback';
import EditPatientProfile from './components/Patient/editPatientProfile/EditPatientProfile';
import UpdateOperationRequest from './components/OperationRequest/updateOperationRequest/UpdateOperationRequest'
import UpdateOperationType from './components/OperationType/updateOperationType/UpdateOperationType';
import CreateSpecialization from './components/Specialization/createSpecialization/CreateSpecialization';
import UpdateMedicalRecord from './components/MedicalRecord/updateRecord/UpdateMedicalRecord';
import AddMedicalCondition from './components/MedicalCondition/AddMedicalCondition/AddMedicalCondition';
import AddAllergy from './components/Allergy/AddAllergy';
import SearchMedicalCondition from './components/MedicalCondition/SearchMedicalCondition/SearchMedicalCondition';
import SearchMedicalRecord from './components/MedicalRecord/searchRecord/SearchMedicalRecord';
import SearchAllergy from './components/Allergy/SearchAllergy/SearchAllergy';


const App = () => {
  const navigate = useNavigate();
  const { isAdmin, isDoctor,isPatient, isAuthenticated, logout, } = useAuth();

  const [showStaffActions, setShowStaffActions] = useState(false);
  const [showPatientActions, setShowPatientActions] = useState(false);
  const [selectedStaffAction, setSelectedStaffAction] = useState(null);
  const [selectedPatientAction, setSelectedPatientAction] = useState('');
  const [selectedPatientId, setSelectedPatientId] = useState(null);
  const [selectedStaffId, setSelectedStaffId] = useState(null);
  const [selectedOperationRequest, setSelectedOperationRequest] = useState(null);
  const [selectedOperationRequestId, setSelectedOperationRequestId] = useState(null);
  const [/*selectedOperationRequestIdForDetails*/, setSelectedOperationRequestIdForDetails] = useState(null);
  const [showMoreActions, setShowMoreActions] = useState(false);
 

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
    setShowStaffActions(!showStaffActions);
    setShowPatientActions(false); 
    setShowMoreActions(false); // Add this line
    setSelectedStaffAction(null);
    setSelectedPatientAction(null);
  };

  const handlePatientClick = () => {
    setShowPatientActions(!showPatientActions);
    setShowStaffActions(false);
    setShowMoreActions(false); // Add this line
    setSelectedPatientAction(null);
    setSelectedStaffAction(null);
  };
  
  const handleMoreClick = () => {
    setShowMoreActions(!showMoreActions); // Toggle more actions
    setShowPatientActions(false);
    setShowStaffActions(false);
    setSelectedPatientAction(null);
    setSelectedStaffAction(null);
  }

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

  const showPatientActionsOnLogin = () => {
    setShowPatientActions(true);
    setShowStaffActions(false);
    setSelectedPatientAction(null);
    setSelectedStaffAction(null);
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
              <button
                  onClick={() => navigate('/hospital-3d')} 
                  className="nav-button"
              >
                  View Hospital 3D
              </button>
              {(isAdmin || isDoctor) && (
                <button 
                  onClick={handleStaffClick}
                  className={`nav-button ${showStaffActions ? 'active' : ''}`}
                >
                  Staff
                </button>
              )}
              {isAdmin && (
                <button 
                  onClick={() => navigate('/appointments')}
                  className="nav-button"
                >
                  Appointments
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
              {(isAdmin) && (
                <button
                  onClick={handleMoreClick}
                  className={`nav-button ${showMoreActions ? 'active' : ''}`}
                >
                  More
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
              <button
                onClick={() => {
                  setSelectedStaffAction('Update Operation Types');
                  navigate('/operation-types/update');
                }}
                className={`action-button ${selectedStaffAction === 'Update Operation Types' ? 'active' : ''}`}
              >
                Manage Operation Types
              </button>
            </>
          )}
          {isDoctor && (
            <>
              <button
                onClick={() => {
                  setSelectedOperationRequest('Update Medical Records');
                  navigate('/medical-records/update');
                }}
                className={`action-button ${selectedOperationRequest === 'Update Medical Records' ? 'active' : ''}`}
              >
                Update Medical Records
              </button>
              <button
                onClick={() => {
                  setSelectedOperationRequest('Search Medical Records');
                  navigate('/medical-records/search');
                }}
                className={`action-button ${selectedOperationRequest === 'Search Medical Records' ? 'active' : ''}`}
              >
                Search Medical Records
              </button>
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
              <button
                onClick={() => {
                  setSelectedStaffAction('Search Medical Conditions');
                  navigate('/medical-conditions/search');
                }}
                className={`action-button ${selectedStaffAction === 'Search Medical Conditions' ? 'active' : ''}`}
              >
                Search Medical Conditions
              </button>
              <button
                onClick={() => {
                  setSelectedStaffAction('Search Allergies');
                  navigate('/allergies/search');
                }}
                className={`action-button ${selectedStaffAction === 'Search Allergies' ? 'active' : ''}`}
              >
                Search Allergies
              </button>
            </>
          )}
          

        </div>
      )}
      {showPatientActions && (
        <div className="patient-action-bar">
          {isPatient && (
            <>
              <button
                onClick={() => {
                  setSelectedPatientAction('Update Profile');
                  navigate('/patient/update');
                }}
                className={`action-button ${selectedPatientAction === 'Update Profile' ? 'active' : ''}`}
              >
                Update Profile
              </button>
              <button
                onClick={() => {
                  setSelectedPatientAction('Create Patient');
                  navigate('/patient/create-profile');
                }}
                className={`action-button ${selectedPatientAction === 'Create Patient' ? 'active' : ''}`}
              >
                Create Profile
              </button>
              <button
                onClick={() => {
                  setSelectedPatientAction('Delete Account');
                  navigate('/patient/delete-account');
                }}
                className={`action-button ${selectedPatientAction === 'Delete Account' ? 'active' : ''}`}
              >
                Delete Account
              </button>
            </>
          )}
          {isAdmin && (
            <>
            <button
              onClick={() => {
                setSelectedPatientAction('Create Patient');
                navigate('/patient/create');
              }}
              className={`action-button ${selectedPatientAction === 'Create Patient' ? 'active' : ''}`}
            >
              Create Patient
            </button>
            <button
              onClick={() => {
                setSelectedPatientAction('View Patients');
                navigate('/patient/list');
              }}
              className={`action-button ${selectedPatientAction === 'View Patients' ? 'active' : ''}`}
            >
              View Patients
            </button>
            
            </>
          )}
        </div>
      )}
      {showMoreActions && (
        <div className="more-action-bar">
          {isAdmin && (
            <>
              <button
                onClick={() => {
                  setSelectedStaffAction('Add Medical Condition');
                  navigate('/medical-condition/add');
                }}
                className={`action-button ${selectedStaffAction === 'Add Medical Condition' ? 'active' : ''}`}
              >
                Add Medical Condition
              </button>
              <button
                onClick={() => {
                  setSelectedStaffAction('Add Specialization');
                  navigate('/specialization/create');
                          }}
                className={`action-button ${selectedStaffAction === 'Add Specialization' ? 'active' : ''}`}
              >
                Add Specialization
              </button>
              <button
                onClick={() => {
                  setSelectedStaffAction('Add Allergy');
                  navigate('/allergy/add-allergy');
                }}
                className={`action-button ${selectedStaffAction === 'Add Allergy' ? 'active' : ''}`}
                >
                Add Allergy
                </button>
                </>
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
      <DeactivateStaff onBack={resetStaffAction} />
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
  <Route 
  path="/appointments" 
  element={
    <ProtectedRoute requiredRole="admin">
      <AppointmentScheduler />
    </ProtectedRoute>
  } 
/>
  
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
<Route 
  path="/operationrequest/update/:id" 
  element={
    <ProtectedRoute requiredRole="doctor">
      <UpdateOperationRequest onBack={() => navigate('/operationrequest/filter')} />
    </ProtectedRoute>
  } 
/>

  <Route path="/operation/delete/:id" element={
    <ProtectedRoute requiredRole="doctor">
      <OperationRequestDeleteConfirmation
        operationRequestId={selectedOperationRequestId}
        onConfirm={resetOperationRequestAction}
        onCancel={resetOperationRequestAction}
      />
    </ProtectedRoute>
  } />
  <Route path="/hospital-3d" element={<Hospital3DView />} />

  {/* Patient Routes */}
  <Route path="/patient/create" element={
    <ProtectedRoute requiredRole="admin">
      <CreatePatient isAdmin={true} />
    </ProtectedRoute>
  } />
  <Route path="/patient/create-profile" element={
  <ProtectedRoute requiredRole="patient">
    <CreatePatient isAdmin={false} />
  </ProtectedRoute>
} />
  <Route path="/patient/update" element={
    <ProtectedRoute requiredRole="patient">
      <UpdatePatient />
    </ProtectedRoute>
  } />
  
  {/* Admin Patient Routes */}
  <Route path="/patient/admin/edit-patient-profile/:id" element={
    <ProtectedRoute requiredRole="admin">
      <EditPatientProfile patientId={selectedPatientId} onBack={resetPatientAction} />
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
  {
  <Route path="/patient/delete-account" element={ isPatient ? (
    <ProtectedRoute requiredRole="patient">
      <DeleteAccount />
    </ProtectedRoute>
  ) : <Navigate to="/" />
} />
}
  <Route path="/hospital-3d" element={<Hospital3DView />} />
  <Route path="/auth/callback" element={
    <AuthCallback onLoginSuccess={showPatientActionsOnLogin} />
  } />
  {isAdmin && (
    <Route 
      path="/operation-types/update" 
      element={<UpdateOperationType />} 
    />
  )}
  <Route path="/specialization/create" element={
    <ProtectedRoute requiredRole="admin">
      <CreateSpecialization />
    </ProtectedRoute>
  } />
  <Route path="/medical-condition/add" element={
    <ProtectedRoute requiredRole="admin">
      <AddMedicalCondition />
    </ProtectedRoute>
  } />
  <Route path="/allergy/add-allergy" element={
    <ProtectedRoute requiredRole="admin">
      <AddAllergy />
    </ProtectedRoute>
  } />
  <Route path="/medical-conditions/search" 
  element={
    <ProtectedRoute requiredRole="doctor">
      <SearchMedicalCondition />
    </ProtectedRoute>
  } />
    <Route path="/medical-records/update" 
    element={
      <ProtectedRoute requiredRole="doctor">
        <UpdateMedicalRecord />
      </ProtectedRoute>
    } 
  />
  <Route path="/medical-records/search" 
  element={
    <ProtectedRoute requiredRole="doctor">
      <SearchMedicalRecord />
    </ProtectedRoute>
  } 
/>
  <Route path="/allergies/search" 
  element={
    <ProtectedRoute requiredRole="doctor">
      <SearchAllergy />
    </ProtectedRoute>
  }
  />
</Routes>

    </div>
  );
  
};

export default App;
