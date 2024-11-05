import React, { useState, useEffect } from 'react';
import { BrowserRouter as Router, Route, Routes, Link } from 'react-router-dom';
import { jwtDecode } from 'jwt-decode'; // Use named import
import CreateStaff from './components/createStaff/CreateStaff.js';
import UpdateStaff from './components/updateStaff/UpdateStaff.js';
import StaffList from './components/staffList/StaffList.js';
import Login from './components/auth/Login';
import CreateOperationRequest from './components/createOperationRequest/CreateOperationRequest.js';
import logo from './assets/hospital.png';
import './App.css';

const App = () => {
  const [authToken, setAuthToken] = useState(localStorage.getItem('authToken'));
  const [showStaffActions, setShowStaffActions] = useState(false);
  const [selectedStaffAction, setSelectedStaffAction] = useState(null);
  const [selectedStaffId, setSelectedStaffId] = useState(null);
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
    setIsAdmin(false);
    setIsDoctor(false);
  };

  const isAuthenticated = () => !!authToken;

  const handleHomeClick = () => {
    setShowStaffActions(false);
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
  };

  const handleSelectStaff = (staffId) => {
    setSelectedStaffId(staffId);
    setSelectedStaffAction('Update Staff');
  };

  const resetStaffAction = () => {
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
    setShowStaffActions(true);
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
              <button
                onClick={() => setSelectedStaffAction('Request Operation')}
                className={`action-button ${selectedStaffAction === 'Request Operation' ? 'active' : ''}`}
              >
                Request Operation
              </button>
            )}
          </div>
        )}

        <main className="main-content">
          <Routes>
            <Route
              path="/"
              element={
                selectedStaffAction === null ? (
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
          {selectedStaffAction === 'Reactivate Staff' && <h2>Reactivate Staff Section</h2>}
          {selectedStaffAction === 'Request Operation' && <CreateOperationRequest />}
        </main>
      </div>
    </Router>
  );
};

export default App;
