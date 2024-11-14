import React, { useState } from 'react';
import { useParams } from 'react-router-dom';
import staffService from '../../../api/staffService';
import './deactivateStaff.css';

const DeactivateStaff = ({ onBack }) => {
  const { id } = useParams();
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');

  const handleDeactivate = async () => {
    if (!id) {
      setErrorMessage('No staff member selected to deactivate.');
      return;
    }

    try {
      await staffService.deactivateStaff(id);
      setSuccessMessage('Staff member successfully deactivated.');
      setErrorMessage('');
      setTimeout(() => {
        onBack();
      }, 2000);
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
