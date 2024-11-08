import React, { useState } from 'react';
import staffService from '../../api/staffService';
import './deactivateStaff.css';

const DeactivateStaff = ({ staffId, onBack }) => {
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');

  const handleDeactivate = async () => {
    if (!staffId) {
      setErrorMessage('No staff member selected to deactivate.');
      return;
    }

    try {
      await staffService.deactivateStaff(staffId);
      setSuccessMessage('Staff member successfully deactivated.');
      setErrorMessage('');
    } catch (error) {
      setErrorMessage('Error deactivating staff member.');
      setSuccessMessage('');
    }
  };

  return (
    <div className="deactivate-staff-container">
      <h2>Deactivate Staff</h2>
      {successMessage && <p className="success-message">{successMessage}</p>}
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      <div className="button-group">
        <button onClick={handleDeactivate} className="deactivate-button">
          Deactivate
        </button>
        <button onClick={onBack} className="back-button">
          Back
        </button>
      </div>
    </div>
  );
};

export default DeactivateStaff;
