import React, { useState, useEffect } from 'react';
import operationRequestService from '../../../api/operationRequestService';
import './CreateOperationRequest.css';

const CreateOperationRequest = () => {
  const [operationData, setOperationData] = useState({
    patientMRN: '',
    doctorLicenseNumber: '',
    operationTypeId: '',
    deadline: '',
    priority: 'elective',
  });
  const [operationTypes, setOperationTypes] = useState([]);
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');
  const [setIsLoading] = useState(true);

  useEffect(() => {
    const controller = new AbortController();

    const fetchOperationTypes = async () => {
      try {
        const response = await operationRequestService.getAllOperationTypes();
        if (!controller.signal.aborted) {
          setOperationTypes(response);
          setIsLoading(false);
          setErrorMessage('');
        }
      } catch (error) {
        if (!controller.signal.aborted) {
          setErrorMessage('Error fetching operation types');
          setIsLoading(false);
        }
      }
    };

    fetchOperationTypes();

    return () => {
      controller.abort();
    };
  }, [setIsLoading]);

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
        patientMRN: '',
        doctorLicenseNumber: '',
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
          <label htmlFor="patientMRN">Patient Medical Record Number:</label>
          <input
            id="patientMRN"
            type="text"
            name="patientMRN"
            value={operationData.patientMRN}
            onChange={handleChange}
            required
            placeholder="Enter patient MRN"
          />
        </div>
        <div className="form-group">
          <label htmlFor="doctorLicenseNumber">Doctor License Number:</label>
          <input
            id="doctorLicenseNumber"
            type="text"
            name="doctorLicenseNumber"
            value={operationData.doctorLicenseNumber}
            onChange={handleChange}
            required
            placeholder="Format: LIC-XXXXXXXX"
          />
        </div>
        <div className="form-group">
          <label htmlFor="operationTypeId">Operation Type:</label>
          <select
            id="operationTypeId"
            name="operationTypeId"
            value={operationData.operationTypeId}
            onChange={handleChange}
            required
          >
            <option value="">Select Operation Type</option>
            {operationTypes.map((type, index) => (
            <option 
              key={type?.operationTypeCode || `fallback-${index}`} 
              value={type?.operationTypeCode || ''}
            >
              {type?.name || 'Unknown'} {type?.version ? `(Version ${type.version})` : ''}
            </option>
          ))}


          </select>
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