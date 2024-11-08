import React, { useState } from 'react';
import staffService from '../../../api/staffService';
import useFormValidation from '../../../hooks/useFormValidation';
import './CreateStaff.css';

const CreateStaff = () => {
  const [staffData, setStaffData] = useState({
    firstName: '',
    lastName: '',
    email: '',
    phoneNumber: '',
    specialization: '',
    availabilitySlots: [{ startTime: '', endTime: '' }],
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');

  const { errors, validate } = useFormValidation();

  const handleChange = (e) => {
    const { name, value } = e.target;
    setStaffData((prevData) => ({
      ...prevData,
      [name]: value,
    }));
  };

  const handleSlotChange = (index, e) => {
    const { name, value } = e.target;
    const newSlots = [...staffData.availabilitySlots];
    newSlots[index][name] = value;
    setStaffData((prevData) => ({
      ...prevData,
      availabilitySlots: newSlots,
    }));
  };

  const addSlot = () => {
    setStaffData((prevData) => ({
      ...prevData,
      availabilitySlots: [...prevData.availabilitySlots, { startTime: '', endTime: '' }],
    }));
  };

  const removeSlot = (index) => {
    const newSlots = staffData.availabilitySlots.filter((_, i) => i !== index);
    setStaffData((prevData) => ({
      ...prevData,
      availabilitySlots: newSlots,
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    if (validate(staffData)) {
      try {
        await staffService.createStaff(staffData);
        setSuccessMessage('Staff created successfully!');
        setErrorMessage('');
        // Clear form on success
        setStaffData({
          firstName: '',
          lastName: '',
          email: '',
          phoneNumber: '',
          specialization: '',
          availabilitySlots: [{ startTime: '', endTime: '' }],
        });
      } catch (error) {
        setErrorMessage('Error creating staff.');
        setSuccessMessage('');
      }
    } else {
      setErrorMessage('Please fix the errors before submitting.');
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
          {errors.firstName && <p className="error-message">{errors.firstName}</p>}
        </div>
        <div className="form-group">
          <label>Last Name:</label>
          <input type="text" name="lastName" value={staffData.lastName} onChange={handleChange} required />
          {errors.lastName && <p className="error-message">{errors.lastName}</p>}
        </div>
        <div className="form-group">
          <label>Email:</label>
          <input type="email" name="email" value={staffData.email} onChange={handleChange} required />
          {errors.email && <p className="error-message">{errors.email}</p>}
        </div>
        <div className="form-group">
          <label>Phone Number:</label>
          <input type="tel" name="phoneNumber" value={staffData.phoneNumber} onChange={handleChange} required />
          {errors.phoneNumber && <p className="error-message">{errors.phoneNumber}</p>}
        </div>
        <div className="form-group">
          <label>Specialization:</label>
          <input type="text" name="specialization" value={staffData.specialization} onChange={handleChange} required />
          {errors.specialization && <p className="error-message">{errors.specialization}</p>}
        </div>
        
        {/* Dynamic Slot Inputs */}
        {staffData.availabilitySlots.map((slot, index) => (
  <div key={index} className="form-group slot-container">
    <div className="time-inputs">
      <div className="form-group">
        <label>Start Time:</label>
        <input
          type="datetime-local"
          name="startTime"
          value={slot.startTime}
          onChange={(e) => handleSlotChange(index, e)}
          required
        />
        {errors[`availabilitySlots.${index}.startTime`] && (
          <p className="error-message">{errors[`availabilitySlots.${index}.startTime`]}</p>
        )}
      </div>
      <div className="form-group">
        <label>End Time:</label>
        <input
          type="datetime-local"
          name="endTime"
          value={slot.endTime}
          onChange={(e) => handleSlotChange(index, e)}
          required
        />
        {errors[`availabilitySlots.${index}.endTime`] && (
          <p className="error-message">{errors[`availabilitySlots.${index}.endTime`]}</p>
        )}
      </div>
    </div>
    <div className="button-group">
      <button type="button" onClick={() => removeSlot(index)} className="remove-slot-button">
        Remove Slot
      </button>
    </div>
  </div>
))}

<div className="button-group">
  <button type="button" onClick={addSlot} className="add-slot-button">
    Add Slot
  </button>
</div>
        <button type="submit" className="submit-button">
          Create 
        </button>
      </form>
    </div>
  );
};

export default CreateStaff;
