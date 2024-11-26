import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import operationRequestService from '../../../api/operationRequestService';
import './UpdateOperationRequest.css';

const UpdateOperationRequest = ({ requestId, onBack }) => {
  const { id } = useParams(); // Get id from URL params

  const [setOriginalData] = useState({});
  const [requestData, setRequestData] = useState({
    operationTypeId: '',
    deadline: '',
    priority: '',
    patientId: '',
    doctorId: ''
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');
  const [fieldErrors, setFieldErrors] = useState({});
  const [isSubmitted, setIsSubmitted] = useState(false)

  useEffect(() => {
    const fetchRequestData = async () => {
      try {
        const data = await operationRequestService.getOperationRequestById(id);
        setRequestData({
          operationTypeId: data.operationTypeId || '',
          deadline: data.deadline || '',
          priority: data.priority || '',
          patientId: data.patientId || '',
          doctorId: data.doctorId || ''
        });
        setOriginalData(data);
      } catch (error) {
        console.error('Failed to fetch operation request data:', error);
        setErrorMessage('Failed to fetch operation request data');
      }
    };

    fetchRequestData();
  }, [requestId,id]);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setRequestData({ ...requestData, [name]: value });
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsSubmitted(true);
    
    // Validate fields
    const errors = {};
    if (!requestData.operationTypeId) {
      errors.operationTypeId = 'Operation type is required.';
    }
    if (!requestData.deadline) {
      errors.deadline = 'Deadline is required.';
    }
    if (!requestData.priority) {
      errors.priority = 'Priority is required.';
    }
    if (!requestData.patientId) {
      errors.patientId = 'Patient ID is required.';
    }
    if (!requestData.doctorId) {
      errors.doctorId = 'Doctor ID is required.';
    }

    setFieldErrors(errors);

    if (Object.keys(errors).length > 0) {
      return;
    }

    try {
      await operationRequestService.updateOperationRequest(id, requestData);
      setSuccessMessage('Operation request updated successfully');
      setErrorMessage('');
      setFieldErrors({});
      if (onBack) {
        onBack();
      }
    } catch (error) {
      console.error('Failed to update operation request:', error);
      setErrorMessage(error.response?.data || 'Failed to update operation request');
    }
  };

  return (
    <div className="update-request-container">
      <h2>Update Operation Request</h2>
      <form className="update-request-form" onSubmit={handleSubmit}>
        <div className="form-group">
          <label>Patient ID:</label>
          <input
            type="text"
            name="patientId"
            value={requestData.patientId}
            onChange={handleChange}
            required
          />
          {isSubmitted && fieldErrors.patientId && (
            <div className="field-error-message">{fieldErrors.patientId}</div>
          )}
        </div>

        <div className="form-group">
          <label>Doctor ID:</label>
          <input
            type="text"
            name="doctorId"
            value={requestData.doctorId}
            onChange={handleChange}
            required
          />
          {isSubmitted && fieldErrors.doctorId && (
            <div className="field-error-message">{fieldErrors.doctorId}</div>
          )}
        </div>

        <div className="form-group">
          <label>Operation Type ID:</label>
          <input
            type="text"
            name="operationTypeId"
            value={requestData.operationTypeId}
            onChange={handleChange}
            required
          />
          {isSubmitted && fieldErrors.operationTypeId && (
            <div className="field-error-message">{fieldErrors.operationTypeId}</div>
          )}
        </div>

        <div className="form-group">
          <label>Deadline:</label>
          <input
            type="datetime-local"
            name="deadline"
            value={requestData.deadline ? new Date(requestData.deadline).toISOString().slice(0, 16) : ''}
            onChange={handleChange}
            required
          />
          {isSubmitted && fieldErrors.deadline && (
            <div className="field-error-message">{fieldErrors.deadline}</div>
          )}
        </div>

        <div className="form-group">
          <label>Priority:</label>
          <select
            name="priority"
            value={requestData.priority}
            onChange={handleChange}
            required
          >
            <option value="">Select Priority</option>
            <option value="elective">Elective</option>
            <option value="urgent">Urgent</option>
            <option value="emergency">Emergency</option>
          </select>
          {isSubmitted && fieldErrors.priority && (
            <div className="field-error-message">{fieldErrors.priority}</div>
          )}
        </div>

        <div className="button-group">
          <button type="submit" className="update-button">Update Request</button>
          {onBack && (
            <button type="button" className="back-button" onClick={onBack}>
              Back
            </button>
          )}
        </div>
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
    </div>
  );
};

export default UpdateOperationRequest;
