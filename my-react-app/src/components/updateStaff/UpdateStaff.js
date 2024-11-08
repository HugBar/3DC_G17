import React, { useState, useEffect } from 'react';
import staffService from '../../api/staffService';
import useFormValidation from '../../hooks/useFormValidation';
import './UpdateStaff.css';

const UpdateStaff = ({ staffId, onBack }) => {
  const [staffData, setStaffData] = useState({
    email: '',
    phoneNumber: '',
    specialization: '',
    availabilitySlots: [{ startTime: '', endTime: '' }],
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');

  const { errors, validate } = useFormValidation();

  useEffect(() => {
    const fetchStaffData = async () => {
      try {
        const data = await staffService.getStaffById(staffId);
        setStaffData(data);
      } catch (error) {
        console.error('Error fetching staff data:', error);
        setErrorMessage('Error fetching staff data.');
      }
    };

    fetchStaffData();
  }, [staffId]);

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
        await staffService.updateStaff(staffId, staffData);
        setSuccessMessage('Staff updated successfully!');
        setErrorMessage('');
      } catch (error) {
        setErrorMessage('Error updating staff.');
        setSuccessMessage('');
      }
    } else {
      setErrorMessage('Please fix the errors before submitting.');
    }
  };

  return (
    <div className="create-staff-container">
      <h2>Update Staff</h2>
      {successMessage && <p className="success-message">{successMessage}</p>}
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      <form onSubmit={handleSubmit} className="staff-form">
        <div className="form-group">
          <label htmlFor="email">Email:</label>
          <input id="email" type="email" name="email" value={staffData.email} onChange={handleChange} required />
          {errors.email && <p className="error-message">{errors.email}</p>}
        </div>
        <div className="form-group">
          <label htmlFor="phoneNumber">Phone Number:</label>
          <input id="phoneNumber" type="tel" name="phoneNumber" value={staffData.phoneNumber} onChange={handleChange} required />
          {errors.phoneNumber && <p className="error-message">{errors.phoneNumber}</p>}
        </div>
        <div className="form-group">
          <label htmlFor="specialization">Specialization:</label>
          <input id="specialization" type="text" name="specialization" value={staffData.specialization} onChange={handleChange} required />
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
                <label htmlFor={`endTime-${index}`}>End Time:</label>
                <input
                  id={`endTime-${index}`}
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

        <div className="button-group">
          <button type="button" onClick={onBack} className="back-button submit-button">
            Back
          </button>
          <button type="submit" className="submit-button">
            Update
          </button>
        </div>
      </form>
    </div>
  );
};

export default UpdateStaff;
