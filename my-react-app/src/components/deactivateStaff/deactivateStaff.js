import React, { useState, useEffect } from 'react';
import staffService from '../../api/staffService';
import './deactivateStaff.css';

const DeactivateStaff = ({ onBack }) => {
  const [staffList, setStaffList] = useState([]);
  const [selectedStaffId, setSelectedStaffId] = useState(null);
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');

  useEffect(() => {
    const fetchStaffList = async () => {
      try {
        const data = await staffService.getAllStaff();
        setStaffList(data);
      } catch (error) {
        setErrorMessage('Error fetching staff list.');
      }
    };

    fetchStaffList();
  }, []);

  const handleDeactivate = async () => {
    if (!selectedStaffId) {
      setErrorMessage('Please select a staff member to deactivate.');
      return;
    }

    try {
      await staffService.deactivateStaff(selectedStaffId);
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
      <ul className="staff-list">
        {staffList.map((staff) => (
          <li
            key={staff.id}
            onClick={() => setSelectedStaffId(staff.id)}
            className={`staff-item ${selectedStaffId === staff.id ? 'selected' : ''}`}
          >
            {staff.firstName} {staff.lastName}
          </li>
        ))}
      </ul>
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
