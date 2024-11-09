// src/components/StaffList/StaffList.js
import React, { useState, useEffect, useCallback } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import staffService from '../../../api/staffService';
import './StaffList.css';

const StaffList = ({ onSelectStaff, onDeactivateStaff }) => {
  const navigate = useNavigate();
  const location = useLocation();
  const [staffList, setStaffList] = useState([]);
  const [errorMessage, setErrorMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const pageSize = 2; // Mesmo tamanho usado no PatientList
  const [selectedStaff, setSelectedStaff] = useState(null);
  const [filters, setFilters] = useState({
    firstName: '',
    lastName: '',
    email: '',
    specialization: ''
  });

  useEffect(() => {
    const searchParams = new URLSearchParams(location.search);
    const page = parseInt(searchParams.get('page')) || 1;
    setCurrentPage(page);
    
    const urlFilters = {
      firstName: searchParams.get('firstName') || '',
      lastName: searchParams.get('lastName') || '',
      email: searchParams.get('email') || '',
      specialization: searchParams.get('specialization') || ''
    };
    setFilters(urlFilters);
  }, [location.search]);

  const updateURLWithFilters = useCallback((currentFilters, page) => {
    const params = new URLSearchParams();
    Object.entries(currentFilters).forEach(([key, value]) => {
      if (value) params.append(key, value);
    });
    params.append('page', page.toString());
    navigate(`/staff/filter?${params.toString()}`, { replace: true });
  }, [navigate]);

  const handleFilterChange = (e) => {
    const { name, value } = e.target;
    const newFilters = {
      ...filters,
      [name]: value
    };
    setFilters(newFilters);
    setCurrentPage(1); // Reset para primeira página ao filtrar
    updateURLWithFilters(newFilters, 1);
  };

  const clearFilters = () => {
    setFilters({
      firstName: '',
      lastName: '',
      email: '',
      specialization: ''
    });
    setCurrentPage(1);
    navigate('/staff/filter');
  };

  const handlePageChange = (newPage) => {
    setCurrentPage(newPage);
    updateURLWithFilters(filters, newPage);
  };

  const fetchStaffList = useCallback(async () => {
    try {
      const response = await staffService.getAllStaff(filters, currentPage, pageSize);
      setStaffList(response.items);
      setTotalPages(response.totalPages);
      setErrorMessage('');
    } catch (error) {
      if (error.response && error.response.status === 404) {
        setStaffList([]);
        setErrorMessage('No staff members found.');
      } else {
        setErrorMessage('Error fetching staff list.');
      }
    }
  }, [filters, currentPage, pageSize]);

  useEffect(() => {
    fetchStaffList();
  }, [fetchStaffList]);

  const handleStaffSelect = async (staffId) => {
    try {
      const staffDetails = await staffService.getStaffById(staffId);
      setSelectedStaff(staffDetails);
    } catch (error) {
      setErrorMessage('Error fetching staff details.');
    }
  };

  const handleCloseDetails = () => {
    setSelectedStaff(null);
  };

  const handleUpdateClick = () => {
    if (selectedStaff) {
      onSelectStaff(selectedStaff.id);
    }
  };

  const handleDeactivateClick = () => {
    if (selectedStaff) {
      onDeactivateStaff(selectedStaff.id);
    }
  };

  return (
    <div className="staff-list-container">
      <h2>Select a Staff Member to Update or Deactivate</h2>
      
      <div className="filters-section">
        <div className="filter-group">
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
            type="email"
            name="email"
            placeholder="Email"
            value={filters.email}
            onChange={handleFilterChange}
          />
          <input
            type="text"
            name="specialization"
            placeholder="Specialization"
            value={filters.specialization}
            onChange={handleFilterChange}
          />
        </div>
        <button onClick={clearFilters} className="clear-filters-button">
          Clear Filters
        </button>

      </div>

      {errorMessage && <p className="error-message">{errorMessage}</p>}
      
      <div className="staff-grid">
        {staffList.map((staff) => (
          <div 
            key={staff.id} 
            className={`staff-card ${selectedStaff?.id === staff.id ? 'selected' : ''}`}
            onClick={() => handleStaffSelect(staff.id)}
          >
            <h3>{staff.firstName} {staff.lastName}</h3>
            <p><strong>Email:</strong> {staff.email}</p>
            <p><strong>Specialization:</strong> {staff.specialization}</p>
            <p><strong>Phone:</strong> {staff.phoneNumber}</p>
          </div>
        ))}
        {staffList.length === 0 && !errorMessage && (
          <p className="no-results">No staff members found</p>
        )}
      </div>

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

      {selectedStaff && (
        <div className="staff-details-modal">
          <div className="modal-content">
            <h3>Staff Details</h3>
            <div className="details-grid">
              <p><strong>Name:</strong> {selectedStaff.firstName} {selectedStaff.lastName}</p>
              <p><strong>Email:</strong> {selectedStaff.email}</p>
              <p><strong>Phone:</strong> {selectedStaff.phoneNumber}</p>
              <p><strong>Specialization:</strong> {selectedStaff.specialization}</p>
              <p><strong>License Number:</strong> {selectedStaff.licenseNumber}</p>
              {selectedStaff.availabilitySlots && (
                <div className="availability-slots">
                  <h4>Availability Slots</h4>
                  {selectedStaff.availabilitySlots.map((slot, index) => (
                    <p key={index}>
                      {new Date(slot.startTime).toLocaleString()} - 
                      {new Date(slot.endTime).toLocaleString()}
                    </p>
                  ))}
                </div>
              )}
            </div>
            <div className="modal-actions">
              <button onClick={handleUpdateClick} className="update-button">
                Update Staff
              </button>
              <button onClick={handleDeactivateClick} className="deactivate-button">
                Deactivate Staff
              </button>
              <button onClick={handleCloseDetails} className="close-button">
                Close
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default StaffList;
