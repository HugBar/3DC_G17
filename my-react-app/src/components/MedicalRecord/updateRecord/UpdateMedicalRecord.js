import React, { useState, useEffect } from 'react';
import medicalRecordService from '../../../api/medicalRecordService';
import './UpdateMedicalRecord.css';

const UpdateMedicalRecord = () => {
    const [patientId, setPatientId] = useState('');
    const [record, setRecord] = useState(null);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState('');
    const [selectedConditions, setSelectedConditions] = useState([]);
    const [selectedAllergies, setSelectedAllergies] = useState([]);
    const [availableConditions, setAvailableConditions] = useState([]);
    const [availableAllergies, setAvailableAllergies] = useState([]);
    const [successMessage, setSuccessMessage] = useState('');

    useEffect(() => {
        const fetchAvailableOptions = async () => {
            try {
                
                // Fetch conditions
                const conditions = await medicalRecordService.getAllMedicalConditions();
                const transformedConditions = conditions.map(condition => ({
                    _id: condition._id || String(Math.random()),
                    name: condition.name || '',
                    severity: condition.severity || ''
                }));
                setAvailableConditions(transformedConditions);
    
                // Fetch allergies
                const allergies = await medicalRecordService.getAllAllergies();
                const transformedAllergies = allergies.map(allergy => ({
                    _id: allergy._id || String(Math.random()),
                    name: allergy.name || '',
                    severity: allergy.severity || ''
                }));
                setAvailableAllergies(transformedAllergies);
            } catch (error) {
                console.error('Error details:', error.response || error);
                setAvailableConditions([]);
                setAvailableAllergies([]);
            }
        };
        fetchAvailableOptions();
    }, []);


    const handleRemoveCondition = (conditionName) => {
        setSelectedConditions(selectedConditions.filter(c => c.name !== conditionName));
    };


    const handleRemoveAllergy = (allergyName) => {
        setSelectedAllergies(selectedAllergies.filter(a => a.name !== allergyName));
    };

    const handleSearch = async () => {
        try {
            setLoading(true);
            setError('');
            const response = await medicalRecordService.getMedicalRecord(patientId);
            setRecord(response);
            setSelectedConditions(response?.conditions || []);
            setSelectedAllergies(response?.allergies || []);
        } catch (error) {
            setError('Medical record not found. A new one will be created upon update.');
            setSelectedConditions([]);
            setSelectedAllergies([]);
        } finally {
            setLoading(false);
        }
    };

    const handleUpdate = async () => {
        try {
            setLoading(true);
            setSuccessMessage('');
            const updateData = {
                conditions: selectedConditions,
                allergies: selectedAllergies
            };
            
            await medicalRecordService.updateMedicalRecord(patientId, updateData);
            setError('');
            setSuccessMessage('Medical record updated successfully');
        } catch (error) {
            setError('Failed to update medical record');
            setSuccessMessage('');
        } finally {
            setLoading(false);
        }
    };

    return (
        <div className="update-medical-record-container">
            <h2>Update Medical Record</h2>

            <div className="search-section">
                <div className="form-group">
                    <label>Patient Medical Number:</label>
                    <input
                        type="text"
                        value={patientId}
                        onChange={(e) => setPatientId(e.target.value)}
                    />
                </div>
                <button 
                    className="search-button"
                    onClick={handleSearch}
                    disabled={loading || !patientId}
                >
                    Search Patient
                </button>
            </div>

            {error && (
                <div className="error-message">
                    {error}
                </div>
            )}

            {successMessage && (
                <div className="success-message">
                    {successMessage}
                </div>
            )}

            {record && (
                <div className="medical-record-form">
                    <div className="section">
                        <h3>Medical Conditions</h3>
                        <div className="current-list">
                            <h4>Current Conditions:</h4>
                            {selectedConditions.length > 0 ? (
                                <ul>
                                    {selectedConditions.map((condition, index) => (
                                        <li key={index} className="list-item">
                                            {condition.name}
                                            <button 
                                                onClick={() => handleRemoveCondition(condition.name)}
                                                className="remove-button"
                                            >
                                                Remove
                                            </button>
                                        </li>
                                    ))}
                                </ul>
                            ) : (
                                <p>No medical conditions recorded</p>
                            )}
                        </div>
                        <div className="add-section">
    <h4>Add Medical Condition:</h4>
    <select
        onChange={(e) => {
            if (e.target.value) {
                const selected = availableConditions.find(c => c.name === e.target.value);
                if (selected) {
                    const newCondition = {
                        name: selected.name,
                        severity: selected.severity
                    };
                    if (!selectedConditions.some(c => c.name === newCondition.name)) {
                        setSelectedConditions(prev => [...prev, newCondition]);
                    }
                }
            }
        }}
        value=""
    >
        <option value="">Select a condition</option>
        {Array.isArray(availableConditions) && availableConditions.map((condition) => (
            <option 
                key={condition._id} 
                value={condition.name}
            >
                {condition.name} - {condition.severity}
            </option>
        ))}
    </select>
</div>
                    </div>

                    <div className="section">
                        <h3>Allergies</h3>
                        <div className="current-list">
                            <h4>Current Allergies:</h4>
                            {selectedAllergies.length > 0 ? (
                                <ul>
                                    {selectedAllergies.map((allergy, index) => (
                                        <li key={index} className="list-item">
                                            {allergy.name}
                                            <button 
                                                onClick={() => handleRemoveAllergy(allergy.name)}
                                                className="remove-button"
                                            >
                                                Remove
                                            </button>
                                        </li>
                                    ))}
                                </ul>
                            ) : (
                                <p>No allergies recorded</p>
                            )}
                        </div>
                        <div className="add-section">
                            <h4>Add Allergy:</h4>
                            <select
                                onChange={(e) => {
                                    if (e.target.value) {
                                        const selected = availableAllergies.find(a => a.name === e.target.value);
                                        if (selected) {
                                            const newAllergy = {
                                                name: selected.name,
                                                severity: selected.severity
                                            };
                                            if (!selectedAllergies.some(a => a.name === newAllergy.name)) {
                                                setSelectedAllergies(prev => [...prev, newAllergy]);
                                            }
                                        }
                                    }
                                }}
                                value=""
                            >
                                <option value="">Select an allergy</option>
                                {Array.isArray(availableAllergies) && availableAllergies.map((allergy) => (
                                    <option 
                                        key={allergy._id} 
                                        value={allergy.name}
                                    >
                                        {allergy.name} - {allergy.severity}
                                    </option>
                                ))}
                            </select>
                        </div>                           
                    </div>

                    <button
                        className="update-button"
                        onClick={handleUpdate}
                        disabled={loading}
                    >
                        Update Medical Record
                    </button>
                </div>
            )}
        </div>
    );
};

export default UpdateMedicalRecord;







