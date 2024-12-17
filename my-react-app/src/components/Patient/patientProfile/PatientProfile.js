import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { useParams } from 'react-router-dom';
import patientService from '../../../api/patientService';
import medicalRecordService from '../../../api/medicalRecordService';
import './PatientProfile.css';

const PatientProfile = () => {
  const navigate = useNavigate();
  const { patientId } = useParams();
  const [patient, setPatient] = useState(null);
  const [medicalRecord, setMedicalRecord] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

   useEffect(() => {
    const fetchData = async () => {
      if (!patientId) {
        setError('Invalid patient ID');
        setLoading(false);
        return;
      }

      try {
        setLoading(true);
        const patientData = await patientService.getPatientById(patientId);
      

        const medicalRecordData = await medicalRecordService.getMedicalRecord(patientData.medicalNr);

        
        if (!patientData) {
          throw new Error('Patient not found');
        }
        
        setPatient(patientData);
        setMedicalRecord(medicalRecordData);
        // ...existing medical record fetch...
      } catch (err) {
        setError(err.message || 'Failed to fetch patient data');
      } finally {
        setLoading(false);
      }
    };

    fetchData();
  }, [patientId]);
  
  useEffect(() => {
    const loadingTimeout = setTimeout(() => {
      if (loading) {
        setError('Loading timed out. Please try again.');
        setLoading(false);
      }
    }, 10000); // 10 second timeout

    return () => clearTimeout(loadingTimeout);
  }, [loading]);

  if (loading) {
    return (
      <div className="loading-container">
        <div className="loading">Loading patient data...</div>
        <div className="loading-subtitle">Please wait while we fetch the patient information</div>
      </div>
    );
  }
  if (error) {
    return (
      <div className="error-container">
        <div className="error">{error}</div>   
        <button onClick={() => navigate(-1)} className="back-button">
          Go Back
        </button>
      </div>
    );
  }

  if (!patient) {
    return (
      <div className="not-found-container">
        <div>No patient found</div>
        <button onClick={() => navigate(-1)} className="back-button">
          Go Back
        </button>
      </div>
    );
  }

  const handleUpdateMedicalRecord = () => {
    // Navigate to the update medical record page
    navigate(`/medical-records/update/${patient.medicalNr}`);
  };

  const handleGoBack = () => {
    navigate(-1);
  }

  return (
    <div className="patient-profile-container">
      <div className="profile-header">
        <h1>Patient Profile</h1>
      </div>

      {loading && (
        <div className="loading-spinner">
          <div className="spinner"></div>
          <p>Loading patient information...</p>
        </div>
      )}

      {error && (
        <div className="error-message">
          <p>{error}</p>
          <button onClick={() => handleGoBack}>Go Back</button>
        </div>
      )}

      {patient && !loading && (
        <div className="profile-content">
          <section className="personal-info">
            <h2>Personal Information</h2>
            <div className="info-grid">
              <div className="info-item">
                <label>Full Name</label>
                <p>{`${patient.firstName} ${patient.lastName}`}</p>
              </div>
              <div className="info-item">
                <label>Medical Record Number</label>
                <p>{patient.medicalNr}</p>
              </div>
              <div className="info-item">
                <label>Email</label>
                <p>{patient.email}</p>
              </div>
              <div className="info-item">
                <label>Phone</label>
                <p>{patient.phoneNumber}</p>
              </div>
              <div className="info-item">
                <label>Date of Birth</label>
                <p>{patient.dateOfBirth}</p>
              </div>
              <div className="info-item">
                <label>Gender</label>
                <p>{patient.gender}</p>
              </div>
            </div>
          </section>

          {medicalRecord && (
            <section className="medical-record">
              <h2>Medical Record</h2>
              <div className="medical-info">
                <div className="conditions">
                  <h3>Medical Conditions</h3>
                  {medicalRecord.conditions?.length > 0 ? (
                    <ul className="conditions-list">
                      {medicalRecord.conditions.map((condition, index) => (
                        <li key={index} className="condition-item">
                          <span className="condition-name">{condition.name}</span>
                          <span className={`severity ${condition.severity.toLowerCase()}`}>
                            {condition.severity}
                          </span>
                        </li>
                      ))}
                    </ul>
                  ) : (
                    <p class='green-text'>No medical conditions recorded</p>
                  )}

                  <h3>Allergies</h3>

                  {medicalRecord.allergies?.length > 0 ? (
                    <ul className="allergies-list">
                      {medicalRecord.allergies.map((allergy, index) => (
                        <li key={index} className="allergy-item">
                          <span className="allergy-name">{allergy.name}</span>
                          <span className={`severity ${allergy.severity.toLowerCase()}`}>
                            {allergy.severity}
                          </span>
                        </li>
                      ))}
                    </ul>
                  ) : (
                    <p class='green-text'>No allergies recorded</p>
                  )}
                </div>
              </div>
              <div className="modal-actions">
              <button onClick={() => handleUpdateMedicalRecord()} className='update-button'>              
                Update Medical Record
              </button>
              </div>
            </section>
          )}
        </div>
      )}
      <br></br>

      <button className="back-button" onClick={() => navigate(-1)}>
          Back to List
      </button>
    </div>
  );
};

export default PatientProfile;