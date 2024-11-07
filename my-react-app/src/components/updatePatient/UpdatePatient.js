import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import patientService from '../../api/patientService';
import './UpdatePatient.css';

const UpdatePatient = ({ patientId, onBack }) => {
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
  const navigate = useNavigate();

  useEffect(() => {
    const fetchPatientData = async () => {
      try {
        const data = await patientService.getPatientProfile(patientId);
        setPatientData(data);
        setOriginalData(data);
      } catch (error) {
        console.error('Failed to fetch patient data:', error);
      }
    };

    fetchPatientData();
  }, [patientId]);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setPatientData({ ...patientData, [name]: value });
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    const updatedFields = {};
    let emailChanged = false;
  
    for (const key in patientData) {
      // Verifica se o valor foi alterado e não é null
      if (patientData[key] !== originalData[key] && patientData[key] !== null) {
        updatedFields[key] = patientData[key];
        if (key === 'email') {
          emailChanged = true;
        }
      }
    }
  
    if (Object.keys(updatedFields).length === 0) {
      alert('No changes detected');
      return;
    }
  
    try {
      await patientService.updatePatientProfile(patientId, updatedFields);
      setSuccessMessage('Profile updated successfully');
      
      if (emailChanged) {
        setShowModal(true);
      }
    } catch (error) {
      console.error('Failed to update profile:', error);
      alert('Failed to update profile');
    }
  };

  const handleModalClose = () => {
    setShowModal(false);
    localStorage.removeItem('authToken'); // Remove o token de autenticação
    navigate('/login'); // Redireciona para a página de login
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
        </div>
        <div className="form-group">
          <label>Phone Number:</label>
          <input
            type="text"
            name="phoneNumber"
            value={patientData.phoneNumber}
            onChange={handleChange}
          />
        </div>
        <div className="form-group">
          <label>First Name:</label>
          <input
            type="text"
            name="firstName"
            value={patientData.firstName}
            onChange={handleChange}
          />
        </div>
        <div className="form-group">
          <label>Last Name:</label>
          <input
            type="text"
            name="lastName"
            value={patientData.lastName}
            onChange={handleChange}
          />
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
          />
        </div>
        <button type="submit" className="update-button">Update</button>
      </form>

      {showModal && (
        <div className="modal">
          <div className="modal-content">
            <h3>Alteração de Email</h3>
            <p>Os dados foram alterados com sucesso. Será necessário fazer login novamente. Obrigado!</p>
            <button onClick={handleModalClose}>OK</button>
          </div>
        </div>
      )}
    </div>
  );
};

export default UpdatePatient;