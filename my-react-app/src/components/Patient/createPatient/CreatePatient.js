import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import usePatientFormValidation from '../../../hooks/usePatientFormValidation';
import patientService from '../../../api/patientService';
import './CreatePatient.css';
import { jwtDecode } from 'jwt-decode'; // Use named import

const CreatePatient = ({ isAdmin }) => {
  const navigate = useNavigate();
  const { errors, validate } = usePatientFormValidation();
  const [successMessage, setSuccessMessage] = useState('');
  const [values, setValues] = useState({
    firstName: '',
    lastName: '',
    email: '',
    phoneNumber: '',
    dateOfBirth: '',
    gender: '',
    contactInfo: '',
    emergencyContact: '',
    medicalNr: ''
  });

  useEffect(() => {
    if (!isAdmin) {
      const token = localStorage.getItem('authToken');
      if (token) {
        try {
          const decodedToken = jwtDecode(token);
          setValues(prev => ({
            ...prev,
            email: decodedToken.email
          }));
        } catch (error) {
          console.error('Error decoding token:', error);
        }
      }
    }
  }, [isAdmin]);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setValues({
      ...values,
      [name]: value
    });
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    if (validate(values)) {
      try {
        if (isAdmin) {
          await patientService.registerPatient(values);
        } else {
          await patientService.registerPatientItself(values);
        }
        setSuccessMessage('Patient registered successfully!');
        setTimeout(() => {
          if (isAdmin) {
            navigate('/patient/list');
          } else {
            navigate('/patient/update');
          }
        }, 2000);
      } catch (error) {
        console.error('Error registering patient:', error);
      }
    }
  };

  return (
    <div className="create-patient-container">
      <h2>Register New Patient</h2>
      {successMessage && <div className="success-message">{successMessage}</div>}
      <form onSubmit={handleSubmit}>
        <div className="form-group">
          <label htmlFor="firstName">First Name:</label>
          <input
            id="firstName"
            name="firstName"
            type="text"
            value={values.firstName}
            onChange={handleChange}
            className={errors.firstName ? 'error' : ''}
          />
          {errors.firstName && <span className="error-message">{errors.firstName}</span>}
        </div>

        <div className="form-group">
          <label htmlFor="lastName">Last Name:</label>
          <input
            id="lastName"
            name="lastName"
            type="text"
            value={values.lastName}
            onChange={handleChange}
            className={errors.lastName ? 'error' : ''}
          />
          {errors.lastName && <span className="error-message">{errors.lastName}</span>}
        </div>

        {isAdmin && (
          <div className="form-group">
            <label htmlFor="email">Email:</label>
            <input
              id="email"
              type="email"
              name="email"
              value={values.email}
              onChange={handleChange}
              className={errors.email ? 'error' : ''}
            />
            {errors.email && <span className="error-message">{errors.email}</span>}
          </div>
        )}

        <div className="form-group">
          <label htmlFor="phoneNumber">Phone Number:</label>
          <input
            id="phoneNumber"
            type="text"
            name="phoneNumber"
            value={values.phoneNumber}
            onChange={handleChange}
            className={errors.phoneNumber ? 'error' : ''}
            placeholder="9 digits"
          />
          {errors.phoneNumber && <span className="error-message">{errors.phoneNumber}</span>}
        </div>

        <div className="form-group">
          <label htmlFor="dateOfBirth">Date of Birth:</label>
          <input
            id="dateOfBirth"
            type="date"
            name="dateOfBirth"
            value={values.dateOfBirth}
            onChange={handleChange}
            className={errors.dateOfBirth ? 'error' : ''}
          />
          {errors.dateOfBirth && <span className="error-message">{errors.dateOfBirth}</span>}
        </div>

        <div className="form-group">
          <label htmlFor="gender">Gender:</label>
          <select
            id="gender"
            name="gender"
            value={values.gender}
            onChange={handleChange}
            className={errors.gender ? 'error' : ''}
          >
            <option value="">Select Gender</option>
            <option value="Male">Male</option>
            <option value="Female">Female</option>
            <option value="Other">Other</option>
          </select>
          {errors.gender && <span className="error-message">{errors.gender}</span>}
        </div>

        <div className="form-group">
          <label htmlFor="contactInfo">Contact Info:</label>
          <textarea
            id="contactInfo"
            name="contactInfo"
            value={values.contactInfo}
            onChange={handleChange}
            className={errors.contactInfo ? 'error' : ''}
          />
          {errors.contactInfo && <span className="error-message">{errors.contactInfo}</span>}
        </div>

        <div className="form-group">
          <label htmlFor="emergencyContact">Emergency Contact:</label>
          <textarea
            id="emergencyContact"
            name="emergencyContact"
            value={values.emergencyContact}
            onChange={handleChange}
            className={errors.emergencyContact ? 'error' : ''}
          />
          {errors.emergencyContact && <span className="error-message">{errors.emergencyContact}</span>}
        </div>

        <div className="form-buttons">
          <button type="submit">Register Patient</button>
          <button type="button" onClick={() => navigate('/patient/list')}>Cancel</button>
        </div>
      </form>
    </div>
  );
};

export default CreatePatient;
