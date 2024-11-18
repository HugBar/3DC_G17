// my-react-app/src/components/Patient/editPatientProfile/EditPatientProfile.js
import React, { useState, useEffect } from 'react';
import patientService from '../../../api/patientService';
import './EditPatientProfile.css';

const EditPatientProfile = ({ patientId, onBack }) => {
  const [originalData, setOriginalData] = useState({});
  const [patientData, setPatientData] = useState({
    phoneNumber: '',
    firstName: '',
    lastName: '',
    dateOfBirth: '',
    gender: '',
    contactInfo: '',
    emergencyContact: '',
    medicalHistory: ''
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');
  const [fieldErrors, setFieldErrors] = useState({});
  const [isSubmitted, setIsSubmitted] = useState(false);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    const fetchPatientData = async () => {
      try {
        setIsLoading(true);
        const data = await patientService.getPatientById(patientId);
        setPatientData(data);
        setOriginalData(data);
      } catch (error) {
        console.error('Failed to fetch patient data:', error);
        setErrorMessage('Failed to fetch patient data');
      } finally {
        setIsLoading(false);
      }
    };

    if (patientId) {
      fetchPatientData();
    }
  }, [patientId]);

  useEffect(() => {
    if (successMessage || errorMessage || Object.keys(fieldErrors).length > 0) {
      const timer = setTimeout(() => {
        setSuccessMessage('');
        setErrorMessage('');
        setFieldErrors({});
        setIsSubmitted(false);
      }, 3000);

      return () => clearTimeout(timer);
    }
  }, [successMessage, errorMessage, fieldErrors]);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setPatientData(prev => ({
      ...prev,
      [name]: value
    }));
  };

  const validateForm = () => {
    const errors = {};
    
    if (!patientData.phoneNumber) {
      errors.phoneNumber = 'Phone number is required';
    } else if (!/^\d{9}$/.test(patientData.phoneNumber)) {
      errors.phoneNumber = 'Phone number must be exactly 9 digits';
    }

    if (!patientData.firstName) {
      errors.firstName = 'First name is required';
    } else if (patientData.firstName.length > 100) {
      errors.firstName = 'First name cannot exceed 100 characters';
    }

    if (!patientData.lastName) {
      errors.lastName = 'Last name is required';
    } else if (patientData.lastName.length > 100) {
      errors.lastName = 'Last name cannot exceed 100 characters';
    }

    if (!patientData.dateOfBirth) {
      errors.dateOfBirth = 'Date of birth is required';
    }

    if (!patientData.gender) {
      errors.gender = 'Gender is required';
    }

    return errors;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsSubmitted(true);
    
    const errors = validateForm();
    if (Object.keys(errors).length > 0) {
      setFieldErrors(errors);
      return;
    }

    const updatedFields = {};
    for (const key in patientData) {
      if (patientData[key] !== originalData[key]) {
        updatedFields[key] = patientData[key];
      }
    }

    if (Object.keys(updatedFields).length === 0) {
      setErrorMessage('No changes detected');
      return;
    }

    try {
      await patientService.updateAdminPatientProfile(patientId, updatedFields);
      setSuccessMessage('Patient profile updated successfully');
      if (onBack) {
        setTimeout(() => {
          onBack();
        }, 2000);
      }
    } catch (error) {
      console.error('Failed to update profile:', error);
      setErrorMessage(error.response?.data || 'Failed to update patient profile');
    }
  };

  if (isLoading) {
    return <div>Loading...</div>;
  }

  return (
    <div className="update-profile-container">
      <h2>Update Patient Profile</h2>
      <form className="update-profile-form" onSubmit={handleSubmit}>
        <div className="form-group">
          <label>First Name: *</label>
          <input
            type="text"
            name="firstName"
            value={patientData.firstName}
            onChange={handleChange}
            required
          />
          {isSubmitted && fieldErrors.firstName && (
            <div className="field-error-message">{fieldErrors.firstName}</div>
          )}
        </div>
  
        <div className="form-group">
          <label>Last Name: *</label>
          <input
            type="text"
            name="lastName"
            value={patientData.lastName}
            onChange={handleChange}
            required
          />
          {isSubmitted && fieldErrors.lastName && (
            <div className="field-error-message">{fieldErrors.lastName}</div>
          )}
        </div>
  
        <div className="form-group">
          <label>Phone Number: *</label>
          <input
            type="text"
            name="phoneNumber"
            value={patientData.phoneNumber}
            onChange={handleChange}
            required
          />
          {isSubmitted && fieldErrors.phoneNumber && (
            <div className="field-error-message">{fieldErrors.phoneNumber}</div>
          )}
        </div>
  
        <div className="form-group">
          <label>Date of Birth: *</label>
          <input
            type="date"
            name="dateOfBirth"
            value={patientData.dateOfBirth}
            onChange={handleChange}
            required
          />
          {isSubmitted && fieldErrors.dateOfBirth && (
            <div className="field-error-message">{fieldErrors.dateOfBirth}</div>
          )}
        </div>
  
        <div className="form-group">
          <label>Gender: *</label>
          <select
            name="gender"
            value={patientData.gender}
            onChange={handleChange}
            required
          >
            <option value="">Select Gender</option>
            <option value="Male">Male</option>
            <option value="Female">Female</option>
            <option value="Other">Other</option>
          </select>
          {isSubmitted && fieldErrors.gender && (
            <div className="field-error-message">{fieldErrors.gender}</div>
          )}
        </div>
  
        <div className="form-group">
          <label>Contact Info:</label>
          <input
            type="text"
            name="contactInfo"
            value={patientData.contactInfo}
            onChange={handleChange}
          />
        </div>
  
        <div className="form-group">
          <label>Emergency Contact:</label>
          <input
            type="text"
            name="emergencyContact"
            value={patientData.emergencyContact}
            onChange={handleChange}
          />
        </div>
  
        <div className="form-group">
          <label>Medical History:</label>
          <textarea
            name="medicalHistory"
            value={patientData.medicalHistory}
            onChange={handleChange}
            rows="4"
          />
        </div>
  
        <div className="form-group">
          <label>Medical Number:</label>
          <input
            type="text"
            name="medicalNr"
            value={patientData.medicalNr}
            disabled
            className="readonly-field"
          />
          <small className="field-note">Medical Number cannot be modified</small>
        </div>
  
        <div className="button-group">
          <button type="submit" className="update-button">Update Patient</button>
          <button type="button" className="back-button" onClick={onBack}>Back</button>
        </div>
      </form>
  
      {successMessage && (
        <div className="success-message">{successMessage}</div>
      )}
      {errorMessage && (
        <div className="error-message">{errorMessage}</div>
      )}
    </div>
  );
}

export default EditPatientProfile;
