import React, { useState } from 'react';
import specializationService from '../../../api/specializationService';
import './CreateSpecialization.css';

const CreateSpecialization = () => {
  const [specializationData, setSpecializationData] = useState({
    name: '',
    description: ''
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');

  const handleChange = (e) => {
    const { name, value } = e.target;
    setSpecializationData(prevData => ({
      ...prevData,
      [name]: value
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      await specializationService.addSpecialization(specializationData);
      setSuccessMessage('Specialization added successfully!');
      setErrorMessage('');
      // Clear form
      setSpecializationData({
        name: '',
        description: ''
      });
    } catch (error) {
      setErrorMessage(error.response?.data?.message || 'Error adding specialization');
      setSuccessMessage('');
    }
  };

  return (
    <div className="create-specialization-container">
      <h2>Add New Specialization</h2>
      {successMessage && <p className="success-message">{successMessage}</p>}
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      
      <form onSubmit={handleSubmit} className="specialization-form">
        <div className="form-group">
          <label htmlFor="name">Specialization Name:</label>
          <input
            id="name"
            name="name"
            type="text"
            value={specializationData.name}
            onChange={handleChange}
            required
          />
        </div>

        <div className="form-group">
          <label htmlFor="description">Description:</label>
          <textarea
            id="description"
            name="description"
            value={specializationData.description}
            onChange={handleChange}
            required
          />
        </div>

        <button type="submit" className="submit-button">
          Add Specialization
        </button>
      </form>
    </div>
  );
};

export default CreateSpecialization;