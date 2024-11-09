import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import patientService from '../../../api/patientService';
import './UpdatePatient.css';

const UpdatePatient = ({ patientEmail, onBack }) => {
  const [originalData, setOriginalData] = useState({});
  const [patientData, setPatientData] = useState({
    email: '',
    phoneNumber: '',
    firstName: '',
    lastName: '',
    dateOfBirth: '',
    gender: '',
    contactInfo: '',
    emergencyContact: '',
    medicalHistory: '',
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [showModal, setShowModal] = useState(false);
  const [errorMessage, setErrorMessage] = useState('');
  const [fieldErrors, setFieldErrors] = useState({});
  const [isSubmitted, setIsSubmitted] = useState(false);
  const navigate = useNavigate();

  useEffect(() => {
    const fetchPatientData = async () => {
      try {
        const data = await patientService.getPatientProfile(patientEmail);
        setPatientData(data);
        setOriginalData(data);
      } catch (error) {
        console.error('Failed to fetch patient data:', error);
        setErrorMessage('Failed to fetch patient data');
      }
    };

    fetchPatientData();
  }, [patientEmail]);

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
    setPatientData({ ...patientData, [name]: value });
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsSubmitted(true);
    
    // Validar todos os campos
    const errors = {};
    if (patientData.email && !/^[\w-.]+@([\w-]+\.)+[\w-]{2,4}$/.test(patientData.email)) {
      errors.email = 'Please enter a valid email.';
    }
    if (patientData.phoneNumber && !/^\d{9}$/.test(patientData.phoneNumber)) {
      errors.phoneNumber = 'Phone number must be exactly 9 digits.';
    }
    if (patientData.firstName && patientData.firstName.length > 100) {
      errors.firstName = 'First name cannot exceed 100 characters.';
    }
    if (patientData.lastName && patientData.lastName.length > 100) {
      errors.lastName = 'Last name cannot exceed 100 characters.';
    }
    if (patientData.gender && patientData.gender.length > 50) {
      errors.gender = 'Gender cannot exceed 50 characters.';
    }
    if (patientData.contactInfo && patientData.contactInfo.length > 500) {
      errors.contactInfo = 'Contact information cannot exceed 500 characters.';
    }
    if (patientData.emergencyContact && patientData.emergencyContact.length > 500) {
      errors.emergencyContact = 'Emergency contact information cannot exceed 500 characters.';
    }
    if (patientData.medicalHistory && patientData.medicalHistory.length > 1000) {
      errors.medicalHistory = 'Medical history cannot exceed 1000 characters.';
    }

    setFieldErrors(errors);

    // Se houver erros, não continua com a submissão
    if (Object.keys(errors).length > 0) {
      return;
    }

    // Verifica se houve alterações
    const updatedFields = {};
    let emailChanged = false;

    for (const key in patientData) {
      if (patientData[key] !== originalData[key] && patientData[key] !== null && patientData[key] !== '') {
        updatedFields[key] = patientData[key];
        if (key === 'email') {
          emailChanged = true;
        }
      }
    }

    if (Object.keys(updatedFields).length === 0) {
      setErrorMessage('No changes detected');
      return;
    }

    try {
      await patientService.updatePatientProfile(patientEmail, updatedFields);
      setSuccessMessage('Profile updated successfully');
      setErrorMessage('');
      setIsSubmitted(false);
      setFieldErrors({});
      
      if (emailChanged) {
        setShowModal(true);
      }
    } catch (error) {
      console.error('Failed to update profile:', error);
      if (error.response && error.response.data) {
        setErrorMessage(error.response.data);
      } else {
        setErrorMessage('Failed to update profile');
      }
    }
  };

  const handleModalClose = () => {
    setShowModal(false);
    localStorage.removeItem('authToken');
    navigate('/login');
  };

  return (
    <div className="update-profile-container">
      <h2>Update Profile</h2>
      <form className="update-profile-form" onSubmit={handleSubmit}>
        <div className="form-group">
          <label>Email:</label>
          <input
            type="email"
            name="email"
            value={patientData.email}
            onChange={handleChange}
          />
          {isSubmitted && fieldErrors.email && (
            <div className="field-error-message">{fieldErrors.email}</div>
          )}
        </div>
        <div className="form-group">
          <label>Phone Number:</label>
          <input
            type="text"
            name="phoneNumber"
            value={patientData.phoneNumber}
            onChange={handleChange}
          />
          {isSubmitted && fieldErrors.phoneNumber && (
            <div className="field-error-message">{fieldErrors.phoneNumber}</div>
          )}
        </div>
        <div className="form-group">
          <label>First Name:</label>
          <input
            type="text"
            name="firstName"
            value={patientData.firstName}
            onChange={handleChange}
          />
          {isSubmitted && fieldErrors.firstName && (
            <div className="field-error-message">{fieldErrors.firstName}</div>
          )}
        </div>
        <div className="form-group">
          <label>Last Name:</label>
          <input
            type="text"
            name="lastName"
            value={patientData.lastName}
            onChange={handleChange}
          />
          {isSubmitted && fieldErrors.lastName && (
            <div className="field-error-message">{fieldErrors.lastName}</div>
          )}
        </div>
        <div className="form-group">
          <label>Date of Birth:</label>
          <input
            type="date"
            name="dateOfBirth"
            value={patientData.dateOfBirth}
            onChange={handleChange}
          />
        </div>
        <div className="form-group">
          <label>Gender:</label>
          <input
            type="text"
            name="gender"
            value={patientData.gender}
            onChange={handleChange}
          />
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
          {isSubmitted && fieldErrors.contactInfo && (
            <div className="field-error-message">{fieldErrors.contactInfo}</div>
          )}
        </div>
        <div className="form-group">
          <label>Emergency Contact:</label>
          <input
            type="text"
            name="emergencyContact"
            value={patientData.emergencyContact}
            onChange={handleChange}
          />
          {isSubmitted && fieldErrors.emergencyContact && (
            <div className="field-error-message">{fieldErrors.emergencyContact}</div>
          )}
        </div>
        <div className="form-group">
          <label>Medical History:</label>
          <textarea
            name="medicalHistory"
            value={patientData.medicalHistory}
            onChange={handleChange}
          />
          {isSubmitted && fieldErrors.medicalHistory && (
            <div className="field-error-message">{fieldErrors.medicalHistory}</div>
          )}
        </div>
        <button type="submit" className="update-button">Update</button>
      </form>

      {successMessage && (
        <div className="success-message">
          {successMessage}
        </div>
      )}
      {showModal && (
        <div className="modal">
          <div className="modal-content">
            <h3>Email Change</h3>
            <p>Data has been updated successfully. You will need to login again. Thank you!</p>
            <button onClick={handleModalClose}>OK</button>
          </div>
        </div>
      )}
    </div>
  );
};

export default UpdatePatient;