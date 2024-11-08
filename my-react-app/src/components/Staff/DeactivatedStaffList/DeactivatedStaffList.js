import React, { useState, useEffect } from 'react';
//import { useNavigate } from 'react-router-dom';
import staffService from '../../../api/staffService';
import './DeactivatedStaffList.css';

const DeactivatedStaffList = () => {
  /*const navigate = useNavigate();*/
  const [deactivatedStaff, setDeactivatedStaff] = useState([]);
  const [errorMessage, setErrorMessage] = useState('');
  const [selectedStaff, setSelectedStaff] = useState(null);

  useEffect(() => {
    fetchDeactivatedStaff();
  }, []);

  const fetchDeactivatedStaff = async () => {
    try {
      const data = await staffService.getDeactivatedStaff();
      setDeactivatedStaff(data);
      setErrorMessage('');
    } catch (error) {
      console.error('Error fetching deactivated staff:', error);
      setErrorMessage('Error fetching deactivated staff list.');
      setDeactivatedStaff([]);
    }
  };

  const handleStaffSelect = (staff) => {
    setSelectedStaff(staff);
  };

  const handleCloseDetails = () => {
    setSelectedStaff(null);
  };

  return (
    <div className="deactivated-staff-container">
      <h2>Deactivated Staff Members</h2>
      
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      
      <div className="staff-grid">
        {deactivatedStaff.map((staff) => (
          <div 
            key={staff.id} 
            className={`staff-card ${selectedStaff?.id === staff.id ? 'selected' : ''}`}
            onClick={() => handleStaffSelect(staff)}
          >
            <h3>{staff.firstName} {staff.lastName}</h3>
            <p><strong>Email:</strong> {staff.email}</p>
            <p><strong>Specialization:</strong> {staff.specialization}</p>
            <p><strong>Phone:</strong> {staff.phoneNumber}</p>
          </div>
        ))}
        {deactivatedStaff.length === 0 && !errorMessage && (
          <p className="no-results">No deactivated staff members found</p>
        )}
      </div>

      {selectedStaff && (
        <div className="staff-details-modal">
          <div className="modal-content">
            <h3>Staff Details</h3>
            <div className="details-grid">
              <p><strong>Name:</strong> {selectedStaff.firstName} {selectedStaff.lastName}</p>
              <p><strong>Email:</strong> {selectedStaff.email}</p>
              <p><strong>Phone:</strong> {selectedStaff.phoneNumber}</p>
              <p><strong>Specialization:</strong> {selectedStaff.specialization}</p>
              <p><strong>License Number:</strong> {selectedStaff.licenseNumber}</p>
            </div>
            <div className="modal-actions">
              <button onClick={handleCloseDetails} className="close-button">
                Close
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default DeactivatedStaffList;