import React, { useState } from 'react';
import './AppointmentScheduler.css';

const AppointmentScheduler = () => {
  const [schedulerData, setSchedulerData] = useState({
    date: '',
    roomNumber: '',
    heuristic: 'Earliest Availability' // default heuristic
  });
  const [successMessage, setSuccessMessage] = useState('');
  const [errorMessage, setErrorMessage] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setSchedulerData((prevData) => ({
      ...prevData,
      [name]: value,
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsLoading(true);
    try {
        const response = await fetch('/api/scheduler/get-scheduling-data', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(schedulerData)
        });
        
        if (!response.ok) throw new Error('Failed to fetch scheduling data');
        
        //const data = await response.json();
        // TODO: Send data to Prolog scheduler
        setSuccessMessage('Schedule generated successfully!');
    } catch (error) {
        setErrorMessage('Error generating schedule.');
    } finally {
        setIsLoading(false);
    }
};

  return (
    <div className="scheduler-container">
      <h2>Generate Appointments Schedule</h2>
      {successMessage && <p className="success-message">{successMessage}</p>}
      {errorMessage && <p className="error-message">{errorMessage}</p>}
      <form onSubmit={handleSubmit} className="scheduler-form">
        <div className="form-group">
          <label htmlFor="date">Select Date:</label>
          <input
            id="date"
            name="date"
            type="date"
            value={schedulerData.date}
            onChange={handleChange}
            required
          />
        </div>

        <div className="form-group">
          <label htmlFor="roomNumber">Room Number:</label>
          <input
            id="roomNumber"
            name="roomNumber"
            type="number"
            min="1"
            value={schedulerData.roomNumber}
            onChange={handleChange}
            required
          />
        </div>

        <div className="form-group">
          <label htmlFor="heuristic">Scheduling Algorithm:</label>
          <select
            id="heuristic"
            name="heuristic"
            value={schedulerData.heuristic}
            onChange={handleChange}
            required
          >
            <option value="greedy">Earliest Availability</option>
          </select>
        </div>

        <button 
          type="submit" 
          className="submit-button"
          disabled={isLoading}
        >
          {isLoading ? 'Generating Schedule...' : 'Generate Schedule'}
        </button>
      </form>
    </div>
  );
};

export default AppointmentScheduler; 