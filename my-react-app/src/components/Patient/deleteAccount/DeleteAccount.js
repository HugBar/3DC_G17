import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../../../context/AuthContext';
import patientService from '../../../api/patientService';
import './DeleteAccount.css';

const DeleteAccount = () => {
  const [showCodeModal, setShowCodeModal] = useState(false);
  const [verificationCode, setVerificationCode] = useState('');
  const [error, setError] = useState('');
  const [successMessage, setSuccessMessage] = useState('');
  const [isDeleted, setIsDeleted] = useState(false);
  const { userEmail, logout } = useAuth();
  const navigate = useNavigate();

  const handleDeleteClick = async () => {
    try {
      await patientService.requestAccountDeletion(userEmail);
      setShowCodeModal(true);
      setSuccessMessage('Verification code sent. Please check your inbox.');
    } catch (error) {
      setError('Failed to send verification code. Please try again.');
    }
  };

  const handleConfirmDeletion = async (e) => {
    e.preventDefault();
    if (!verificationCode || verificationCode.length !== 6) {
      setError('Please enter the 6-digit verification code from your email.');
      return;
    }

    try {
      await patientService.confirmAccountDeletion(verificationCode);
      setIsDeleted(true);
      setSuccessMessage('Account successfully deleted.');
      setTimeout(() => {
        logout();
        navigate('/login');
      }, 3000);
    } catch (error) {
      setError('Invalid verification code or deletion failed. Please try again.');
    }
  };

  return (
    <div className="delete-account-container">
      <h2 className="delete-account-title">Delete Account</h2>
      
      <div className="warning-section">
        <p className="warning-text">
          ⚠️ <strong>Warning:</strong> This action is irreversible. Your personal identifiable information will be permanently deleted.
        </p>
      </div>
  
      <div className="data-retention-info">
        <h3 className="retention-title">🔒 Important Information About Data Retention:</h3>
        <p className="retention-intro">
          For legal and medical purposes, the following information will be preserved:
        </p>
        <ul className="retention-list">
          <li>📋 Medical History Records</li>
          <li>📅 Appointment History</li>
          <li>📊 Age Range (anonymized)</li>
          <li>⚧ Gender</li>
        </ul>
        <p className="retention-reason">
          This data is retained in compliance with healthcare regulations to ensure the integrity of medical records. 
          <strong>All personal identifiable information will be anonymized.</strong>
        </p>
      </div>
  
      <button 
        className="delete-button" 
        onClick={handleDeleteClick}
      >
        Delete My Account
      </button>

      {showCodeModal && (
        <div className="delete-account-modal-overlay">
          <div className="delete-account-modal">
            {isDeleted ? (
              <div className="deletion-success">
                <h3>Account Deleted</h3>
                <p className="success-text">{successMessage}</p>
                <p className="redirect-text">Redirecting to login page...</p>
              </div>
            ) : (
              <>
                <h3 className="modal-title">Enter Verification Code</h3>
                <p className="modal-message">A 6-digit verification code has been sent to your email.</p>
                <p className="modal-instruction">Please enter the code to complete the account deletion.</p>
                <form onSubmit={handleConfirmDeletion} className="token-form">
                  <input
                    type="text"
                    value={verificationCode}
                    onChange={(e) => {
                      const value = e.target.value.replace(/[^0-9]/g, '');
                      if (value.length <= 6) {
                        setVerificationCode(value);
                      }
                    }}
                    placeholder="000000"
                    className="token-input"
                    maxLength={6}
                    pattern="\d{6}"
                    inputMode="numeric"
                    autoComplete="one-time-code"
                  />
                  {error && <div className="modal-error-message">{error}</div>}
                  <div className="modal-buttons">
                    <button type="submit">Confirm Deletion</button>
                    <button type="button" onClick={() => setShowCodeModal(false)}>Cancel</button>
                  </div>
                </form>
              </>
            )}
          </div>
        </div>
      )}

      {!showCodeModal && error && <div className="error-message">{error}</div>}
      {!showCodeModal && successMessage && <div className="success-message">{successMessage}</div>}
    </div>
  );
};

export default DeleteAccount;