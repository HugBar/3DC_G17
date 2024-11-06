// src/components/OperationRequestList/OperationRequestList.js
import React, { useState, useEffect } from 'react';
import operationRequestService from '../../api/operationRequestService';
import './OperationRequestList.css';

const OperationRequestList = ({ onSelectOperationRequest }) => {
  const [operationRequestList, setOperationRequestList] = useState([]);
  const [errorMessage, setErrorMessage] = useState('');

  useEffect(() => {
    const fetchOperationRequests = async () => {
      try {
        const data = await operationRequestService.getAllOperationRequests({});
        setOperationRequestList(data);
      } catch (error) {
        setErrorMessage('Error fetching operation requests.');
      }
    };

    fetchOperationRequests();
  }, []);

  return (
    <div className="operation-request-list-container">
      <h2>Select an Operation Request</h2>
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      <ul className="operation-request-list">
        {operationRequestList.map((request) => (
          <li key={request.id} onClick={() => onSelectOperationRequest(request.id)} className="operation-request-item">
            {request.id}
          </li>
        ))}
      </ul>
    </div>
  );
};

export default OperationRequestList;
