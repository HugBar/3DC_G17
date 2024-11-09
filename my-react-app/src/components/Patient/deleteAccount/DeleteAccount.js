import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../../../context/AuthContext';
import patientService from '../../../api/patientService';
import './DeleteAccount.css';

const DeleteAccount = () => {
  const [showTokenModal, setShowTokenModal] = useState(false);
  const [token, setToken] = useState('');
  const [error, setError] = useState('');
  const [successMessage, setSuccessMessage] = useState('');
  const [isDeleted, setIsDeleted] = useState(false);
  const { userEmail, logout } = useAuth();
  const navigate = useNavigate();

  const handleDeleteClick = async () => {
    try {
      await patientService.requestAccountDeletion(userEmail);
      setShowTokenModal(true);
      setSuccessMessage('Confirmation email sent. Please check your inbox.');
    } catch (error) {
      setError('Failed to send confirmation email. Please try again.');
    }
  };

  const handleConfirmDeletion = async (e) => {
    e.preventDefault();
    if (!token) {
      setError('Please enter the confirmation token from your email.');
      return;
    }

    try {
      await patientService.confirmAccountDeletion(token);
      setIsDeleted(true);
      setSuccessMessage('Account successfully deleted.');
      setTimeout(() => {
        logout();
        navigate('/login');
      }, 3000);
    } catch (error) {
      setError('Invalid token or deletion failed. Please try again.');
    }
  };

  return (
    <div className="delete-account-container">
      <h2 className="delete-account-title">Delete Account</h2>
      <p className="warning-text">
        Warning: This action cannot be undone. All your personal data will be permanently deleted.
      </p>
      
      <button 
        className="delete-button"
        onClick={handleDeleteClick}
      >
        Delete My Account
      </button>

      {/* Token Input Modal */}
      {showTokenModal && (
        <div className="modal">
          <div className="modal-content">
            {isDeleted ? (
              <div className="deletion-success">
                <h3>Account Deleted</h3>
                <p className="success-text">{successMessage}</p>
                <p className="redirect-text">Redirecting to login page...</p>
              </div>
            ) : (
              <>
                <h3 className="modal-title">Enter Confirmation Token</h3>
                <p className="modal-message">A confirmation token has been sent to your email.</p>
                <p className="modal-instruction">Please enter the token to complete the account deletion.</p>
                <form onSubmit={handleConfirmDeletion} className="token-form">
                  <input
                    type="text"
                    value={token}
                    onChange={(e) => setToken(e.target.value)}
                    placeholder="Enter token"
                    className="token-input"
                  />
                  {error && <div className="modal-error-message">{error}</div>}
                  <div className="modal-buttons">
                    <button type="submit">Confirm Deletion</button>
                    <button type="button" onClick={() => setShowTokenModal(false)}>Cancel</button>
                  </div>
                </form>
              </>
            )}
          </div>
        </div>
      )}

      {!showTokenModal && error && <div className="error-message">{error}</div>}
      {!showTokenModal && successMessage && <div className="success-message">{successMessage}</div>}
    </div>
  );
};

export default DeleteAccount;