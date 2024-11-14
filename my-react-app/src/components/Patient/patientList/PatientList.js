import React, { useState, useEffect } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import  patientService  from '../../../api/patientService';
import './PatientList.css';

const PatientList = ({onSelectPatient}) => {
  const navigate = useNavigate();
  const location = useLocation();
  const [patientList, setPatientList] = useState([]);
  const [errorMessage, setErrorMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const pageSize = 1;
  
  const [filters, setFilters] = useState({
    firstName: '',
    lastName: '',
    email: '',
    medicalNr: ''
  });

  const [selectedPatient, setSelectedPatient] = useState(null);

  useEffect(() => {
    const searchParams = new URLSearchParams(location.search);
    const page = parseInt(searchParams.get('page')) || 1;
    setCurrentPage(page);
    
    const urlFilters = {
      firstName: searchParams.get('firstName') || '',
      lastName: searchParams.get('lastName') || '',
      email: searchParams.get('email') || '',
      medicalNr: searchParams.get('medicalNr') || ''
    };
    setFilters(urlFilters);
  }, [location.search]);

  const handleFilterChange = (e) => {
    const { name, value } = e.target;
    setFilters(prev => ({ ...prev, [name]: value }));
    setCurrentPage(1); // Reset para primeira página ao filtrar
    updateURL({ ...filters, [name]: value }, 1);
  };

  const updateURL = (currentFilters, page) => {
    const params = new URLSearchParams();
    
    Object.entries(currentFilters).forEach(([key, value]) => {
      if (value) params.append(key, value);
    });
    
    params.append('page', page.toString());
    
    navigate(`/patient/list?${params.toString()}`, { replace: true });
  };

  const clearFilters = () => {
    setFilters({
      firstName: '',
      lastName: '',
      email: '',
      medicalNr: ''
    });
    setCurrentPage(1);
    navigate('/patient/list');
  };

  const handlePageChange = (newPage) => {
    setCurrentPage(newPage);
    updateURL(filters, newPage);
  };

  const handleSelectPatient = () => {
    if (selectedPatient) {
      onSelectPatient(selectedPatient.id);
    }
  };

  const handleDeleteClick = (patientId) => {
    navigate(`/patient/delete/${patientId}`);
  };

  useEffect(() => {
    const fetchPatients = async () => {
      try {
        const response = await patientService.getAllPatients(filters, currentPage, pageSize);
        console.log('API Response:', response); // Para debug
        
        if (response && response.items) {
          setPatientList(response.items);
          setTotalPages(response.totalPages);
        }
      } catch (error) {
        if (error.response && error.response.status === 404) {
          setPatientList([]);
          setErrorMessage('No patients found.');
        } else {
          setErrorMessage('Error fetching patient list.');
        }
      }
    };

    fetchPatients();
  }, [currentPage, filters, pageSize]);

  const handlePatientSelect = async (patientId) => {
    try {
      const patientDetails = await patientService.getPatientById(patientId);
      setSelectedPatient(patientDetails);
    } catch (error) {
      setErrorMessage('Error fetching patient details.');
    }
  };

  return (
    <div className="patient-list-container">
      <h2>Patient List</h2>

      <div className="filter-group">
        <div className="filters-section">
          <input
            type="text"
            name="firstName"
            placeholder="First Name"
            value={filters.firstName}
            onChange={handleFilterChange}
          />
          <input
            type="text"
            name="lastName"
            placeholder="Last Name"
            value={filters.lastName}
            onChange={handleFilterChange}
          />
          <input
            type="text"
            name="email"
            placeholder="Email"
            value={filters.email}
            onChange={handleFilterChange}
          />
          <input
            type="text"
            name="medicalNr"
            placeholder="Medical Record Number"
            value={filters.medicalNr}
            onChange={handleFilterChange}
          />
          
        </div>
        <button onClick={clearFilters} className="clear-filters-button">
          Clear Filters
        </button>
      </div>
      

      {errorMessage && <div className="error-message">{errorMessage}</div>}

      {patientList && patientList.length > 0 && (
        <div className="patient-grid">
          {patientList.map((patient) => (
            <div 
              key={patient.id} 
              className={`patient-card ${selectedPatient?.id === patient.id ? 'selected' : ''}`}
              onClick={() => handlePatientSelect(patient.id)}
            >
              <h3>{`${patient.firstName} ${patient.lastName}`}</h3>
              <p><strong>Medical Record #:</strong> {patient.medicalNr}</p>
              <p><strong>Email:</strong> {patient.email}</p>
              <p><strong>Phone:</strong> {patient.phoneNumber}</p>
              <p><strong>Date of Birth:</strong> {patient?.dateOfBirth || 'Not available'}</p>
            </div>
          ))}
          
        </div>
      )}

      {selectedPatient && (
        <div className="patient-details-modal">
          <div className="modal-content">
            <h3>Patient Details</h3>
            <div className="details-grid">
              <p><strong>Name:</strong> {selectedPatient.firstName} {selectedPatient.lastName}</p>
              <p><strong>Email:</strong> {selectedPatient.email}</p>
              <p><strong>Medical Record #:</strong> {selectedPatient.medicalNr}</p>
              <p><strong>Phone:</strong> {selectedPatient.phoneNumber}</p>
              <p><strong>Date of Birth:</strong> {selectedPatient?.dateOfBirth || 'Not available'}</p>
              <p><strong>Gender:</strong> {selectedPatient?.gender || 'Not available'}</p>
              <p><strong>Emergency Contact:</strong> {selectedPatient?.emergencyContact || 'Not available'}</p>
              </div>
            <div className="modal-actions">
              <button onClick={handleSelectPatient} className="update-button">
                Update Patient
              </button>
              <button onClick={() => handleDeleteClick(selectedPatient.id)} className="delete-button">
                Delete Patient
              </button>
              <button onClick={() => setSelectedPatient(null)} className="close-button">
                Close
              </button>
            </div>
          </div>
        </div>
      )}

      <div className="pagination-controls">
        <button 
          onClick={() => handlePageChange(currentPage - 1)}
          disabled={currentPage <= 1}
          className="pagination-button"
        >
          Previous
        </button>
        
        <span className="page-info">
          Page {currentPage} of {totalPages}
        </span>
        
        <button 
          onClick={() => handlePageChange(currentPage + 1)}
          disabled={currentPage >= totalPages}
          className="pagination-button"
        >
          Next
        </button>
      </div>
    </div>
  );
};

export default PatientList;