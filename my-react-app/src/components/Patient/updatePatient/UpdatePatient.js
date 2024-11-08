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
  const navigate = useNavigate();

  useEffect(() => {
    const fetchPatientData = async () => {
      try {
        const data = await patientService.getPatientProfile(patientEmail);
        setPatientData(data);
        setOriginalData(data);
      } catch (error) {
        console.error('Failed to fetch patient data:', error);
      }
    };

    fetchPatientData();
  }, [patientEmail]);

  useEffect(() => {
    if (successMessage || errorMessage) {
      const timer = setTimeout(() => {
        setSuccessMessage('');
        setErrorMessage('');
      }, 5000);

      return () => clearTimeout(timer);
    }
  }, [successMessage, errorMessage]);

const handleChange = (e) => {
    const { name, value } = e.target;
    setPatientData({ ...patientData, [name]: value });
  };

  const validateFields = () => {
    if (patientData.email && !/^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$/.test(patientData.email)) {
      return 'Por favor, insira um email válido.';
    }
    if (patientData.phoneNumber && !/^\d{9}$/.test(patientData.phoneNumber)) {
      return 'O número de telefone deve ter exatamente 9 dígitos.';
    }
    if (patientData.firstName && patientData.firstName.length > 100) {
      return 'O primeiro nome não pode ter mais de 100 caracteres.';
    }
    if (patientData.lastName && patientData.lastName.length > 100) {
      return 'O sobrenome não pode ter mais de 100 caracteres.';
    }
    if (patientData.gender && patientData.gender.length > 50) {
      return 'O gênero não pode ter mais de 50 caracteres.';
    }
    if (patientData.contactInfo && patientData.contactInfo.length > 500) {
      return 'As informações de contato não podem ter mais de 500 caracteres.';
    }
    if (patientData.emergencyContact && patientData.emergencyContact.length > 500) {
      return 'As informações de contato de emergência não podem ter mais de 500 caracteres.';
    }
    if (patientData.medicalHistory && patientData.medicalHistory.length > 1000) {
      return 'O histórico médico não pode ter mais de 1000 caracteres.';
    }
    if (patientData.medicalNr && !/^MED-[A-Z0-9]{8}$/.test(patientData.medicalNr)) {
      return 'Formato de número médico inválido. Deve ser "MED-" seguido de 8 caracteres alfanuméricos.';
    }
    return null;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    const validationError = validateFields();
    if (validationError) {
      setErrorMessage(validationError);
      return;
    }
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
      setErrorMessage('Nenhuma alteração detetada');
      return;
    }

    try {
      await patientService.updatePatientProfile(patientEmail, updatedFields);
      setSuccessMessage('Perfil atualizado com sucesso');
      setErrorMessage(''); // Limpa a mensagem de erro
      
      if (emailChanged) {
        setShowModal(true);
      }
    } catch (error) {
      console.error('Falha ao atualizar o perfil:', error);
      if (error.response && error.response.data) {
        setErrorMessage(error.response.data); // Exibe a mensagem de erro do backend
      } else {
        setErrorMessage('Falha ao atualizar o perfil');
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
      <div className="navbar">
      </div>
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

      {successMessage && (
        <div className="success-message">
          {successMessage}
        </div>
      )}
      {errorMessage && (
     <div className="error-message">
       {errorMessage}
        </div>
      )}
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