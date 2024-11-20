import React, { useState } from 'react';
import staffService from '../../../api/staffService';
import { CreateStaffDTO, AvailabilitySlotDTO } from '../../../dtos/StaffDTOs';
import useFormValidation from '../../../hooks/useFormValidation';
import './CreateStaff.css';

const CreateStaff = () => {
  const [staffData, setStaffData] = useState({
    firstName: '',
    lastName: '',
    email: '',
    phoneNumber: '',
    specialization: '',
    availabilitySlots: [new AvailabilitySlotDTO('', '')]
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
    newSlots[index] = new AvailabilitySlotDTO(
      name === 'startTime' ? value : newSlots[index].startTime,
      name === 'endTime' ? value : newSlots[index].endTime
    );
    setStaffData(prevData => ({
      ...prevData,
      availabilitySlots: newSlots
    }));
  };

  const addSlot = () => {
    setStaffData((prevData) => ({
      ...prevData,
      availabilitySlots: [...prevData.availabilitySlots, new AvailabilitySlotDTO('', '')],
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
        const staffDTO = new CreateStaffDTO(
          staffData.firstName,
          staffData.lastName,
          staffData.email,
          staffData.phoneNumber,
          staffData.specialization,
          staffData.availabilitySlots
        );

        const createdStaff = await staffService.createStaff(staffDTO);
        setSuccessMessage(`Staff ${createdStaff.firstName} ${createdStaff.lastName} created successfully!`);
        setErrorMessage('');
        // Clear form
        setStaffData({
          firstName: '',
          lastName: '',
          email: '',
          phoneNumber: '',
          specialization: '',
          availabilitySlots: [new AvailabilitySlotDTO('', '')]
        });
      } catch (error) {
        console.log('Full error:', error);
        
        if (error.response) {
          console.error('Error response data:', error.response.data);
          console.error('Error status:', error.response.status);
          setErrorMessage(error.response.data.message || 
                         error.response.data.title || 
                         `Server error: ${error.response.status}`);
        } else if (error.request) {
          console.error('Error request:', error.request);
          setErrorMessage('No response received from server. Please try again.');
        } else {
          console.error('Error message:', error.message);
          setErrorMessage(`Error: ${error.message}`);
        }
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
          <label htmlFor="firstName">First Name:</label>
          <input
            id="firstName"
            name="firstName"
            type="text"
            value={staffData.firstName}
            onChange={handleChange}
            required
          />
          {errors.firstName && <p className="error-message">{errors.firstName}</p>}
        </div>
        <div className="form-group">
          <label htmlFor="lastName">Last Name:</label>
          <input
            id="lastName"
            name="lastName"
            type="text"
            value={staffData.lastName}
            onChange={handleChange}
            required
          />
          {errors.lastName && <p className="error-message">{errors.lastName}</p>}
        </div>
        <div className="form-group">
          <label htmlFor="email">Email:</label>
          <input
            id="email"
            name="email"
            type="email"
            value={staffData.email}
            onChange={handleChange}
            required
          />
          {errors.email && <p className="error-message">{errors.email}</p>}
        </div>
        <div className="form-group">
          <label htmlFor="phoneNumber">Phone Number:</label>
          <input
            id="phoneNumber"
            name="phoneNumber"
            type="tel"
            value={staffData.phoneNumber}
            onChange={handleChange}
            required
          />
          {errors.phoneNumber && <p className="error-message">{errors.phoneNumber}</p>}
        </div>
        <div className="form-group">
          <label htmlFor="specialization">Specialization:</label>
          <input
            id="specialization"
            name="specialization"
            type="text"
            value={staffData.specialization}
            onChange={handleChange}
            required
          />
          {errors.specialization && <p className="error-message">{errors.specialization}</p>}
        </div>
        
        {/* Dynamic Slot Inputs */}
        {staffData.availabilitySlots.map((slot, index) => (
  <div key={index} className="form-group slot-container">
    <div className="time-inputs">
      <div className="form-group">
        <label htmlFor={`startTime-${index}`}>Start Time:</label>
        <input
          id={`startTime-${index}`}
          name="startTime"
          type="datetime-local"
          value={slot.startTime}
          onChange={(e) => handleSlotChange(index, e)}
          required
        />
        {errors[`availabilitySlots.${index}.startTime`] && (
          <p className="error-message">{errors[`availabilitySlots.${index}.startTime`]}</p>
        )}
      </div>
      <div className="form-group">
        <label htmlFor={`endTime-${index}`}>End Time:</label>
        <input
          id={`endTime-${index}`}
          name="endTime"
          type="datetime-local"
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
