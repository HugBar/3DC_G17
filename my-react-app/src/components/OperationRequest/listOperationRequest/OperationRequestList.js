// src/components/OperationRequestList/OperationRequestList.js
import React, { useState, useEffect, useCallback } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import operationRequestService from '../../../api/operationRequestService';
import './OperationRequestList.css';

const OperationRequestList = ({ onDeleteOperationRequest }) => {
  const navigate = useNavigate();
  const location = useLocation();
  const [operationRequestList, setOperationRequestList] = useState([]);
  const [errorMessage, setErrorMessage] = useState('');
  const [selectedRequest, setSelectedRequest] = useState(null);
  const [filters, setFilters] = useState({
    doctorLicenseNumber: '',
    patientMedicalNumber: '',
    priority: ''
  });

  useEffect(() => {
    const searchParams = new URLSearchParams(location.search);
    const urlFilters = {
      doctorId: searchParams.get('doctorId') || '',
      patientId: searchParams.get('patientId') || '',
      priority: searchParams.get('priority') || ''
    };
    setFilters(urlFilters);
  }, [location.search]);

  const updateURLWithFilters = useCallback((currentFilters) => {
    const params = new URLSearchParams();
    Object.entries(currentFilters).forEach(([key, value]) => {
      if (value) params.append(key, value);
    });
    const newURL = `/operationrequest/filter?${params.toString()}`;
    navigate(newURL, { replace: true });
  }, [navigate]);

  const handleFilterChange = (e) => {
    const { name, value } = e.target;
    const newFilters = { ...filters, [name]: value };
    setFilters(newFilters);
    updateURLWithFilters(newFilters);
  };

  const clearFilters = () => {
    const emptyFilters = { doctorId: '', patientId: '', priority: '' };
    setFilters(emptyFilters);
    navigate('/operationrequest/filter'); // Clears the URL parameters
  };

  const fetchOperationRequests = useCallback(async () => {
    try {
      const data = await operationRequestService.getAllOperationRequests(filters);
      setOperationRequestList(data);
      setErrorMessage('');
    } catch (error) {
      if (error.response && error.response.status === 404) {
        setOperationRequestList([]);
        setErrorMessage('No operation requests found.');
      } else {
        setErrorMessage('Error fetching operation requests.');
      }
    }
  }, [filters]);

  useEffect(() => {
    fetchOperationRequests();
  }, [fetchOperationRequests]);

  const handleRequestSelect = (requestId) => {
    const request = operationRequestList.find(req => req.id === requestId);
    setSelectedRequest(request);
  };

  const handleCloseDetails = () => {
    setSelectedRequest(null);
  };

  const handleDeleteClick = () => {
    if (selectedRequest) {
      onDeleteOperationRequest(selectedRequest.id);
    }
  };

  return (
    <div className="operation-request-list-container">
      <h2>Select an Operation Request</h2>
      
      <div className="filters-section">
        <div className="filter-group">
          <input
            type="text"
            name="doctorLicenseNumber"
            placeholder="Doctor License Number"
            value={filters.doctorLicenseNumber}
            onChange={handleFilterChange}
          />
          <input
            type="text"
            name="patientMedicalNumber"
            placeholder="Patient Medical Number"
            value={filters.patientMedicalNumber}
            onChange={handleFilterChange}
          />
          <input
            type="text"
            name="priority"
            placeholder="Priority"
            value={filters.priority}
            onChange={handleFilterChange}
          />
        </div>
        <button onClick={clearFilters} className="clear-filters-button">
          Clear Filters
        </button>
      </div>

      {errorMessage && <p className="error-message">{errorMessage}</p>}
      
      <div className="operation-request-grid">
        {operationRequestList.map((request) => (
          <div 
            key={request.id} 
            className={`operation-request-card ${selectedRequest?.id === request.id ? 'selected' : ''}`}
            onClick={() => handleRequestSelect(request.id)}
          >
            <h3>Request ID: {request.id}</h3>
            <p><strong>Doctor License Number:</strong> {request.doctorLicenseNumber}</p>
            <p><strong>Patient Medical Number:</strong> {request.patientMedicalNumber}</p>
            <p><strong>Priority:</strong> {request.priority}</p>
          </div>
        ))}
        {operationRequestList.length === 0 && !errorMessage && (
          <p className="no-results">No operation requests found</p>
        )}
      </div>

      {selectedRequest && (
        <div className="operation-request-details-modal">
          <div className="modal-content">
            <h2>Operation Request Details</h2>
            <p><strong>ID:</strong> {selectedRequest.id}</p>
            <p><strong>Patient Medical Number:</strong> {selectedRequest.patientMedicalNumber}</p>
            <p><strong>Doctor License Number:</strong> {selectedRequest.doctorLicenseNumber}</p>
            <p><strong>Priority:</strong> {selectedRequest.priority}</p>
            <p><strong>Deadline:</strong> {new Date(selectedRequest.deadline).toLocaleString()}</p>
            <button onClick={handleDeleteClick} className="delete-button">Delete Operation Request</button>
            <button onClick={handleCloseDetails} className="back-button">Back to List</button>
          </div>
        </div>
      )}
    </div>
  );
};

export default OperationRequestList;
