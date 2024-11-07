import React, { useState, useEffect } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import  patientService  from '../../api/patientService';
import './PatientList.css';

const PatientList = () => {
  const navigate = useNavigate();
  const location = useLocation();
  const [patientList, setPatientList] = useState([]);
  const [errorMessage, setErrorMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const pageSize = 5; // Mesmo valor default do backend
  
  const [filters, setFilters] = useState({
    firstName: '',
    lastName: '',
    email: '',
    medicalNr: ''
  });

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

  useEffect(() => {
    const fetchPatients = async () => {
      try {
        const response = await patientService.getAllPatients(filters, currentPage, pageSize);
        console.log('Fetched patients:', response); // Debug log
        
        if (response && Array.isArray(response)) {
          setPatientList(response);
          setErrorMessage('');
        } else {
          setPatientList([]);
          setErrorMessage('Invalid response format');
        }
      } catch (error) {
        console.error('Error in fetchPatients:', error);
        if (error.response?.status === 404) {
          setPatientList([]);
          setErrorMessage('No patients found.');
        } else if (error.response && error.response.status === 401) {
          setPatientList([]);
          setErrorMessage('Unauthorized. Please login.');
        } else {
          setPatientList([]);
          setErrorMessage('Error fetching patient list.');
        }
      }
    };

    fetchPatients();
  }, [filters, currentPage, pageSize]);

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

      {patientList.length > 0 ? (
        <div className="patient-grid">
          {patientList.map((patient) => (
            <div key={patient.id} className="patient-card">
              <h3>{`${patient.firstName} ${patient.lastName}`}</h3>
              <p><strong>Medical Record #:</strong> {patient.medicalNr}</p>
              <p><strong>Email:</strong> {patient.email}</p>
              <p><strong>Phone:</strong> {patient.phoneNumber}</p>
            </div>
          ))}
        </div>
      ) : (
        <div className="no-results">
          {errorMessage || 'No patients found'}
        </div>
      )}

      <div className="pagination">
        <button 
          onClick={() => handlePageChange(currentPage - 1)}
          disabled={currentPage === 1}
        >
          Previous
        </button>
        
        <span className="page-info">
          Page {currentPage} of {totalPages}
        </span>
        
        <button 
          onClick={() => handlePageChange(currentPage + 1)}
          disabled={currentPage === totalPages}
        >
          Next
        </button>
      </div>
    </div>
  );
};

export default PatientList;