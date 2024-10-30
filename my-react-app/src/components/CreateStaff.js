import React, { useState } from 'react';
import { createStaff } from '../api/staff';
import './CreateStaff.css'; // Import a CSS file for additional styles

const CreateStaff = () => {
  const [staffData, setStaffData] = useState({
    firstName: '',
    lastName: '',
    email: '',
    phoneNumber: '',
    specialization: '',
    availabilitySlots: [{ startTime: '', endTime: '' }]
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');

  const handleChange = (e) => {
    const { name, value } = e.target;
    setStaffData((prevData) => ({
      ...prevData,
      [name]: value
    }));
  };

  const handleSlotChange = (index, e) => {
    const { name, value } = e.target;
    const newSlots = [...staffData.availabilitySlots];
    newSlots[index][name] = value;
    setStaffData((prevData) => ({
      ...prevData,
      availabilitySlots: newSlots
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      await createStaff(staffData);
      setSuccessMessage('Staff created successfully!');
      setErrorMessage('');
    } catch (error) {
      setErrorMessage('Error creating staff.');
      setSuccessMessage('');
    }
  };

  return (
    <div className="create-staff-container">
      <h2>Create Staff</h2>
      {successMessage && <p className="success-message">{successMessage}</p>}
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      <form onSubmit={handleSubmit} className="staff-form">
        <div className="form-group">
          <label>First Name:</label>
          <input type="text" name="firstName" value={staffData.firstName} onChange={handleChange} required />
        </div>
        <div className="form-group">
          <label>Last Name:</label>
          <input type="text" name="lastName" value={staffData.lastName} onChange={handleChange} required />
        </div>
        <div className="form-group">
          <label>Email:</label>
          <input type="email" name="email" value={staffData.email} onChange={handleChange} required />
        </div>
        <div className="form-group">
          <label>Phone Number:</label>
          <input type="tel" name="phoneNumber" value={staffData.phoneNumber} onChange={handleChange} required />
        </div>
        <div className="form-group">
          <label>Specialization:</label>
          <input type="text" name="specialization" value={staffData.specialization} onChange={handleChange} required />
        </div>
        {staffData.availabilitySlots.map((slot, index) => (
          <div key={index} className="form-group">
            <label>Start Time:</label>
            <input type="datetime-local" name="startTime" value={slot.startTime} onChange={(e) => handleSlotChange(index, e)} required />
            <label>End Time:</label>
            <input type="datetime-local" name="endTime" value={slot.endTime} onChange={(e) => handleSlotChange(index, e)} required />
          </div>
        ))}
        <button type="submit" className="submit-button">Create Staff</button>
      </form>
    </div>
  );
};

export default CreateStaff;