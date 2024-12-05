import React, { useState, useEffect } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import staffService from '../../../api/staffService';
import { StaffDTO, AvailabilitySlotDTO } from '../../../dtos/StaffDTOs';
import useFormValidation from '../../../hooks/useFormValidation';
import specializationService from '../../../api/specializationService';
import './UpdateStaff.css';

const UpdateStaff = ({ onBack }) => {
  const navigate = useNavigate();
  const { id } = useParams();

  const [staffData, setStaffData] = useState({
    email: '',
    phoneNumber: '',
    specialization: '',
    availabilitySlots: [{ startTime: '', endTime: '' }],
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');
  const [specializations, setSpecializations] = useState([]);

  const { errors, validate } = useFormValidation();

  useEffect(() => {
    const fetchStaffData = async () => {
      try {
        const token = localStorage.getItem('authToken');
        if (!token) {
          navigate('/login');
          return;
        }

        if (!id) {
          setErrorMessage('Staff ID is required');
          return;
        }

        const data = await staffService.getStaffById(id);
        const staffDTO = StaffDTO.fromJSON(data);
        setStaffData(staffDTO);
      } catch (error) {
        console.error('Error fetching staff data:', error);
        if (error.response?.status === 401) {
          navigate('/login');
        } else {
          setErrorMessage('Error fetching staff data. Please try again later.');
        }
      }
    };

    fetchStaffData();
  }, [id, navigate]);

  useEffect(() => {
    const fetchSpecializations = async () => {
      try {
        const data = await specializationService.getAllSpecializations();
        setSpecializations(data);
      } catch (error) {
        console.error('Error fetching specializations:', error);
        setErrorMessage('Error loading specializations');
      }
    };

    fetchSpecializations();
  }, []);

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
        await staffService.updateStaff(id, staffData);
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
      <form 
        onSubmit={handleSubmit} 
        className="staff-form" 
      >
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
          <select
            id="specialization"
            name="specialization"
            value={staffData.specialization}
            onChange={handleChange}
            required
          >
            <option value="">Select a specialization</option>
            {specializations.map((spec) => (
              <option key={spec._id} value={spec.name}>
                {spec.name}
              </option>
            ))}
          </select>
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
