import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import medicalRecordService from '../../../api/MedicalRecord/medicalRecordService';
import './SearchMedicalRecord.css';

const SearchMedicalRecord = () => {
    const navigate = useNavigate();
    const [patientId, setPatientId] = useState('');
    const [conditionName, setConditionName] = useState('');
    const [allergyName, setAllergyName] = useState('');
    const [record, setRecord] = useState(null);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState('');

    const handleSearch = async () => {
        try {
            setLoading(true);
            setError('');
            const response = await medicalRecordService.searchMedicalRecord(
                patientId,
                conditionName,
                allergyName
            );
            setRecord(response);
        } catch (error) {
            setError('Medical record not found');
            setRecord(null);
        } finally {
            setLoading(false);
        }
    };

    const handleUpdate = () => {
        navigate(`/medical-records/update/${patientId}`);
    };

    return (
        <div className="search-medical-record-container">
            <h2>Search Medical Record</h2>

            <div className="search-section">
                <div className="form-group">
                    <label htmlFor="patientId">Patient Medical Number:</label>
                    <input
                        id="patientId"
                        type="text"
                        value={patientId}
                        onChange={(e) => setPatientId(e.target.value)}
                        required
                    />
                </div>
                <div className="form-group">
                    <label htmlFor="conditionName">Condition Name (optional):</label>
                    <input
                        id="conditionName"
                        type="text"
                        value={conditionName}
                        onChange={(e) => setConditionName(e.target.value)}
                    />
                </div>
                <div className="form-group">
                    <label htmlFor="allergyName">Allergy Name (optional):</label>
                    <input
                        id="allergyName"
                        type="text"
                        value={allergyName}
                        onChange={(e) => setAllergyName(e.target.value)}
                    />
                </div>
                <button 
                    className="search-button"
                    onClick={handleSearch}
                    disabled={loading || !patientId}
                >
                    Search
                </button>
            </div>

            {error && (
                <div role="alert" className="error-message">
                    {error}
                </div>
            )}

            {record && (
                <div className="medical-record-details">
                    <h3>Medical Record Details</h3>
                    {record.conditions && conditionName && (
                        <div className="section">
                            <h4>Medical Conditions</h4>
                            {record.conditions.length > 0 ? (
                                <ul>
                                    {record.conditions.map((condition, index) => (
                                        <li key={index} className="list-item">
                                            {condition.name} - Severity: {condition.severity}
                                        </li>
                                    ))}
                                </ul>
                            ) : (
                                <p>No medical conditions found</p>
                            )}
                        </div>
                    )}

                    {record.allergies && allergyName && (
                        <div className="section">
                            <h4>Allergies</h4>
                            {record.allergies.length > 0 ? (
                                <ul>
                                    {record.allergies.map((allergy, index) => (
                                        <li key={index} className="list-item">
                                            {allergy.name} - Severity: {allergy.severity}
                                        </li>
                                    ))}
                                </ul>
                            ) : (
                                <p>No allergies found</p>
                            )}
                        </div>
                    )}
                    
                    {!conditionName && !allergyName && (
                        <>
                            <div className="section">
                                <h4>Medical Conditions</h4>
                                {record.conditions.length > 0 ? (
                                    <ul>
                                        {record.conditions.map((condition, index) => (
                                            <li key={index} className="list-item">
                                                {condition.name} - Severity: {condition.severity}
                                            </li>
                                        ))}
                                    </ul>
                                ) : (
                                    <p>No medical conditions found</p>
                                )}
                            </div>
                            <div className="section">
                                <h4>Allergies</h4>
                                {record.allergies.length > 0 ? (
                                    <ul>
                                        {record.allergies.map((allergy, index) => (
                                            <li key={index} className="list-item">
                                                {allergy.name} - Severity: {allergy.severity}
                                            </li>
                                        ))}
                                    </ul>
                                ) : (
                                    <p>No allergies found</p>
                                )}
                            </div>
                        </>
                    )}
                    
                    <button
                        className="update-button"
                        onClick={handleUpdate}
                    >
                        Update Medical Record
                    </button>
                </div>
            )}
        </div>
    );
};

export default SearchMedicalRecord; 