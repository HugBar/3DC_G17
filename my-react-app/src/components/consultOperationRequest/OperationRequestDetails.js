// src/components/OperationRequestDetails/OperationRequestDetails.js
import React, { useState, useEffect } from 'react';
import operationRequestService from '../../api/operationRequestService';
import staffService from '../../api/staffService';
import './OperationRequestDetails.css';

const OperationRequestDetails = ({ operationRequestId, onBack }) => {
  const [operationDetails, setOperationDetails] = useState(null);
  const [doctorName, setDoctorName] = useState(null);
  const [errorMessage, setErrorMessage] = useState('');

  useEffect(() => {
    const fetchOperationDetails = async () => {
      try {
        const data = await operationRequestService.getOperationRequestById(operationRequestId);
        setOperationDetails(data);
        const doctorData = await staffService.getStaffById(data.doctorId);
        setDoctorName(doctorData.firstName + " " + doctorData.lastName);
      } catch (error) {
        setErrorMessage('Error fetching operation details.');
      }
    };

    fetchOperationDetails();
  }, [operationRequestId]);

  if (!operationDetails) return <p>{errorMessage || 'Loading operation details...'}</p>;

  return (
    <div className="operation-details-container">
      <h2>Operation Request Details</h2>
      <p><strong>ID:</strong> {operationDetails.id}</p>
      <p><strong>Patient ID:</strong> {operationDetails.patientId}</p>
      <p><strong>Doctor ID:</strong> {operationDetails.doctorId}</p>
      <p><strong>Doctor Name:</strong> {doctorName}</p>
      <p><strong>Operation Type ID:</strong> {operationDetails.operationTypeId}</p>
      <p><strong>Priority:</strong> {operationDetails.priority}</p>
      <p><strong>Deadline:</strong> {new Date(operationDetails.deadline).toLocaleString()}</p>
      <button onClick={onBack} className="back-button">Back to List</button>
    </div>
  );
};

export default OperationRequestDetails;
