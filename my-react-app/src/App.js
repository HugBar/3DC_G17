import React, { useState } from 'react';
import { BrowserRouter as Router, Route, Routes, Link } from 'react-router-dom';
import CreateStaff from './components/createStaff/CreateStaff.js';
import UpdateStaff from './components/updateStaff/UpdateStaff.js';
import StaffList from './components/staffList/StaffList.js'; // Import the new StaffList component
import Login from './components/auth/Login';
import logo from './assets/hospital.png';
import './App.css';

const App = () => {
  const [authToken, setAuthToken] = useState(localStorage.getItem('authToken'));
  const [showStaffActions, setShowStaffActions] = useState(false);
  const [selectedStaffAction, setSelectedStaffAction] = useState(null);
  const [selectedStaffId, setSelectedStaffId] = useState(null); // Track selected staff ID for update

  const handleLogin = (token) => {
    setAuthToken(token);
    localStorage.setItem('authToken', token);
  };

  const handleLogout = () => {
    setAuthToken(null);
    localStorage.removeItem('authToken');
    setShowStaffActions(false);
    setSelectedStaffAction(null);
    setSelectedStaffId(null); // Clear selected staff on logout
  };

  const isAuthenticated = () => !!authToken;

  const handleHomeClick = () => {
    setShowStaffActions(false);
    setSelectedStaffAction(null);
    setSelectedStaffId(null);
  };

  const handleSelectStaff = (staffId) => {
    setSelectedStaffId(staffId);
    setSelectedStaffAction('Update Staff'); // Transition to Update Staff view
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
            <button
              onClick={() => setSelectedStaffAction('Reactivate Staff')}
              className={`action-button ${selectedStaffAction === 'Reactivate Staff' ? 'active' : ''}`}
            >
              Reactivate Staff
            </button>
          </div>
        )}

        <main className="main-content">
          <Routes>
            {/* Home content is only displayed when no staff action is selected */}
            <Route
              path="/"
              element={
                selectedStaffAction === null ? (
                  <h2>Welcome to the Hospital Management App</h2>
                ) : null
              }
            />
            <Route path="/login" element={<Login onLogin={handleLogin} />} />
          </Routes>

          {/* Display Selected Action Component in Main Content */}
          {selectedStaffAction === 'Create Staff' && <CreateStaff />}
          {selectedStaffAction === 'Update Staff' && (
            selectedStaffId ? (
              <UpdateStaff staffId={selectedStaffId} />
            ) : (
              <StaffList onSelectStaff={handleSelectStaff} />
            )
          )}
          {selectedStaffAction === 'Reactivate Staff' && (
            <h2>Reactivate Staff Section</h2>
          )}
        </main>
      </div>
    </Router>
  );
};

export default App;
