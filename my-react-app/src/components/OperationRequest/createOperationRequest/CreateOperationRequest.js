import React, { useState } from 'react';
import operationRequestService from '../../../api/operationRequestService';
import './CreateOperationRequest.css';

const CreateOperationRequest = () => {
  const [operationData, setOperationData] = useState({
    patientId: '',
    doctorId: '',
    operationTypeId: '',
    deadline: '',
    priority: 'elective', // Default priority
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');

  const handleChange = (e) => {
    const { name, value } = e.target;
    setOperationData((prevData) => ({
      ...prevData,
      [name]: value,
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      await operationRequestService.createOperationRequest(operationData);
      setSuccessMessage('Operation request created successfully!');
      setErrorMessage('');
      // Clear form on success
      setOperationData({
        patientId: '',
        doctorId: '',
        operationTypeId: '',
        deadline: '',
        priority: 'elective',
      });
    } catch (error) {
      setErrorMessage('Error creating operation request.');
      setSuccessMessage('');
    }
  };

  return (
    <div className="create-operation-request-container">
      <h2>Create Operation Request</h2>
      {successMessage && <p className="success-message">{successMessage}</p>}
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      <form onSubmit={handleSubmit} className="operation-request-form">
        <div className="form-group">
          <label htmlFor="patientId">Patient ID:</label>
          <input
            id="patientId"
            type="text"
            name="patientId"
            value={operationData.patientId}
            onChange={handleChange}
            required
          />
        </div>
        <div className="form-group">
          <label htmlFor="doctorId">Doctor ID:</label>
          <input
            id="doctorId"
            type="text"
            name="doctorId"
            value={operationData.doctorId}
            onChange={handleChange}
            required
          />
        </div>
        <div className="form-group">
          <label htmlFor="operationTypeId">Operation Type ID:</label>
          <input
            id="operationTypeId"
            type="text"
            name="operationTypeId"
            value={operationData.operationTypeId}
            onChange={handleChange}
            required
          />
        </div>
        <div className="form-group">
          <label htmlFor="deadline">Deadline:</label>
          <input
            id="deadline"
            type="datetime-local"
            name="deadline"
            value={operationData.deadline}
            onChange={handleChange}
            required
          />
        </div>
        <div className="form-group">
          <label htmlFor="priority">Priority:</label>
          <select
            id="priority"
            name="priority"
            value={operationData.priority}
            onChange={handleChange}
            required
          >
            <option value="elective">Elective</option>
            <option value="urgent">Urgent</option>
            <option value="emergency">Emergency</option>
          </select>
        </div>
        <button type="submit" className="submit-button">Submit Request</button>
      </form>
    </div>
  );
};

export default CreateOperationRequest; 