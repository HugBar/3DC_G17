// src/components/StaffList/StaffList.js
import React, { useState, useEffect } from 'react';
import staffService from '../../api/staffService';
import './StaffList.css';

const StaffList = ({ onSelectStaff }) => {
  const [staffList, setStaffList] = useState([]);
  const [errorMessage, setErrorMessage] = useState('');

  useEffect(() => {
    const fetchStaffList = async () => {
      try {
        const data = await staffService.getAllStaff(); 
        setStaffList(data);
      } catch (error) {
        setErrorMessage('Error fetching staff list.');
      }
    };

    fetchStaffList();
  }, []);

  return (
    <div className="staff-list-container">
      <h2>Select a Staff Member to Update</h2>
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      <ul className="staff-list">
        {staffList.map((staff) => (
          <li key={staff.id} onClick={() => onSelectStaff(staff.id)} className="staff-item">
            {staff.firstName} {staff.lastName}
          </li>
        ))}
      </ul>
    </div>
  );
};

export default StaffList;
