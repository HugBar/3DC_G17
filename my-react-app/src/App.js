import React, { useState } from 'react';
import { BrowserRouter as Router, Route, Routes, Link, Navigate } from 'react-router-dom';
import CreateStaff from './components/CreateStaff';
import Login from './components/Login';
import './App.css'; // Import the CSS file for styling

const App = () => {
  const [authToken, setAuthToken] = useState(localStorage.getItem('authToken'));

  const handleLogin = (token) => {
    setAuthToken(token);
    localStorage.setItem('authToken', token);
  };

  const handleLogout = () => {
    setAuthToken(null);
    localStorage.removeItem('authToken');
  };

  const isAuthenticated = () => {
    return !!authToken;
  };

  return (
    <Router>
      <div>
        <header className="app-header">
          <h1 className="app-title">Hospital Management App</h1>
          <nav className="app-nav">
            <Link to="/" className="nav-link">Home</Link>
            {isAuthenticated() && (
              <Link to="/create-staff" className="nav-link">Create Staff</Link>
            )}
            {isAuthenticated() ? (
              <button onClick={handleLogout} className="nav-link">Logout</button>
            ) : (
              <Link to="/login" className="nav-link">Login</Link>
            )}
          </nav>
        </header>
        <main>
          <Routes>
            <Route path="/" element={<h1>Welcome to the Hospital Management App</h1>} />
            <Route path="/login" element={<Login onLogin={handleLogin} />} />
            <Route
              path="/create-staff"
              element={isAuthenticated() ? <CreateStaff /> : <Navigate to="/login" />}
            />
          </Routes>
        </main>
      </div>
    </Router>
  );
};

export default App;