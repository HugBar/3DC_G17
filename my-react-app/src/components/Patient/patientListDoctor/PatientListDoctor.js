import React, { useState, useEffect } from 'react';
import { useNavigate} from 'react-router-dom';
import patientService from '../../../api/patientService';
import './PatientListDoctor.css';

const PatientListDoctor = () => {
  const navigate = useNavigate();
  const [patientList, setPatientList] = useState([]);
  const [errorMessage, setErrorMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const pageSize = 2;

  const [filters, setFilters] = useState({
    firstName: '',
    lastName: '',
    email: '',
    medicalNr: ''
  });

  // Handle filter changes
  const handleFilterChange = (e) => {
    const { name, value } = e.target;
    setFilters(prev => ({ ...prev, [name]: value }));
    setCurrentPage(1);
    updateURL({ ...filters, [name]: value }, 1);
  };

  // Update URL with filters
  const updateURL = (currentFilters, page) => {
    const params = new URLSearchParams();
    Object.entries(currentFilters).forEach(([key, value]) => {
      if (value) params.append(key, value);
    });
    params.append('page', page.toString());
    navigate(`?${params.toString()}`);
  };

  // Clear filters
  const clearFilters = () => {
    setFilters({
      firstName: '',
      lastName: '',
      email: '',
      medicalNr: ''
    });
    setCurrentPage(1);
    navigate('');
  };

  // Navigate to patient profile
  const handlePatientClick = (patientId) => {
    if (!patientId) {
      console.error('No patient ID provided');
      return;
    }
    navigate(`/patient/${patientId}`);
  };

  // Fetch patients
  useEffect(() => {
    const fetchPatients = async () => {
      try {
        
        const response = await patientService.getAllPatients(filters, currentPage, pageSize);
        if (response && response.items) {
          setPatientList(response.items);
          setTotalPages(response.totalPages);
        }
      } catch (error) {
        setErrorMessage('Error fetching patient list.');
        console.error(error);
      }
    };

    fetchPatients();
  }, [currentPage, filters, pageSize]);

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
            name="medicalNr"
            placeholder="Medical Record Number"
            value={filters.medicalNr}
            onChange={handleFilterChange}
          />
        </div>
        <button onClick={clearFilters} className="clear-filters-button">Clear Filters</button>
      </div>

      {errorMessage && <div className="error-message">{errorMessage}</div>}

      <div className="patient-grid">
        {patientList.map((patient) => (
          <div
            key={patient.id}
            className="patient-card"
            onClick={() => handlePatientClick(patient.id)}
          >
            <h3>{patient.firstName} {patient.lastName}</h3>
            <p><strong>Medical Record #:</strong> {patient.medicalNr}</p>
            <p><strong>Email:</strong> {patient.email}</p>
            <p><strong>Phone:</strong> {patient.phoneNumber}</p>
          </div>
        ))}
      </div>

      <div className="pagination-controls">
        <button
          onClick={() => updateURL(filters, currentPage - 1)}
          disabled={currentPage <= 1}
          className="pagination-button"
        >
          Previous
        </button>
        <span className="page-info">Page {currentPage} of {totalPages}</span>
        <button
          onClick={() => updateURL(filters, currentPage + 1)}
          disabled={currentPage >= totalPages}
          className="pagination-button"
        >
          Next
        </button>
      </div>
    </div>
  );
};

export default PatientListDoctor;