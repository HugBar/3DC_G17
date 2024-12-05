import React, { useState, useEffect } from 'react';
import operationTypeService from '../../../api/operationTypeService';
import './UpdateOperationType.css';
import specializationService from '../../../api/specializationService';

const UpdateOperationType = () => {
    const [operationTypes, setOperationTypes] = useState([]);
    const [specializations, setSpecializations] = useState([]);
    const [selectedType, setSelectedType] = useState(null);
    const [editedType, setEditedType] = useState({
        name: '',
        requiredStaffBySpecialization: {},
        duration: {
            Duration_AnesthesiaPreparation: '',
            Duration_Surgery: '',
            Duration_Cleaning: ''
        },
        isActive: true,
        version: 1,
        validFrom: new Date(),
        validTo: null
    });
    const [errorMessage, setErrorMessage] = useState('');
    const [successMessage, setSuccessMessage] = useState('');
    const [selectedSpecialization, setSelectedSpecialization] = useState('');
    const [staffCount, setStaffCount] = useState('1');

    useEffect(() => {
        fetchData();
    }, []);

    useEffect(() => {
        const fetchSpecializations = async () => {
            try {
                const data = await specializationService.getAllSpecializations();
                setSpecializations(data);
            } catch (error) {
                setErrorMessage('Error loading specializations');
            }
        };

        fetchSpecializations();
    }, []);

    const fetchData = async () => {
        try {
            const response = await operationTypeService.getAllOperationTypes();
            console.log('Operation Types Data:', response);
            
            // Group operations by their code and get the highest version
            const latestVersions = Object.values(response.reduce((acc, type) => {
                if (!acc[type.operationTypeCode] || 
                    acc[type.operationTypeCode].version < type.version) {
                    acc[type.operationTypeCode] = type;
                }
                return acc;
            }, {}));

            setOperationTypes(latestVersions);
        } catch (error) {
            setErrorMessage('Error loading data');
        }
    };

    const handleSelectType = (type) => {
        setSelectedType(type);
        setEditedType({
            name: type.name,
            requiredStaffBySpecialization: {...type.requiredStaffBySpecialization},
            duration: {...type.duration},
            isActive: type.isActive
        });
        setErrorMessage('');
        setSuccessMessage('');
    };

    const handleUpdate = async (e) => {
        e.preventDefault();
        if (!selectedType) return;
    
        try {
            const typeId = selectedType.id.value;
            const updateData = {
                requiredStaffBySpecialization: editedType.requiredStaffBySpecialization
            };
            
            await operationTypeService.updateOperationType(typeId, updateData);
            setSuccessMessage('Operation type updated successfully');
            fetchData();
            setSelectedType(null);
            setEditedType({
                ...editedType,
                requiredStaffBySpecialization: {}
            });
        } catch (error) {
            console.error('Update error:', error);
            setErrorMessage('Error updating operation type');
        }
    };

    return (
        <div className="update-operation-type-container">
            <h2>Update Operation Types</h2>

            {/* List of Operation Types */}
            <div className="operation-types-list">
                <h3>Select Operation Type to Update</h3>
                <table>
                    <thead>
                        <tr>
                            <th>Name</th>
                            <th>Version</th>
                            <th>Required Staff</th>
                            <th>Anesthesia Prep (min)</th>
                            <th>Surgery (min)</th>
                            <th>Cleaning (min)</th>
                            <th>Actions</th>
                        </tr>
                    </thead>
                    <tbody>
                        {operationTypes.map((type) => (
                            <tr key={type.operationTypeCode}>
                                <td>{type.name}</td>
                                <td>{type.version}</td>
                                <td>
                                    {Object.entries(type.requiredStaffBySpecialization).map(([spec, count]) => (
                                        `${spec}: ${count}, `
                                    ))}
                                </td>
                                <td>{type.duration?.Duration_AnesthesiaPreparation || 
                                    type.duration?.anesthesiaPreparation || 
                                    type.duration?.duration_AnesthesiaPrep || 
                                    'N/A'}</td>
                                <td>{type.duration?.Duration_Surgery || 
                                    type.duration?.duration_Surgery || 
                                    type.duration?.surgery || 
                                    'N/A'}</td>
                                <td>{type.duration?.Duration_Cleaning || 
                                    type.duration?.duration_Cleaning || 
                                    type.duration?.cleaning || 
                                    'N/A'}</td>
                                <td>
                                    <button 
                                        onClick={() => handleSelectType(type)}
                                        className="select-button"
                                        disabled={!type.isActive}
                                    >
                                        Select
                                    </button>
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>

            {/* Update Form */}
            {selectedType && (
                <form onSubmit={handleUpdate} className="update-form">
                    <h3>Update Staff Requirements for {selectedType.name}</h3>
                    
                    <div className="current-requirements">
                        <h4>Current Staff Requirements:</h4>
                        {Object.entries(editedType.requiredStaffBySpecialization).map(([spec, count]) => (
                            <div key={spec} className="specialization-item">
                                <span>{spec}: {count} staff member(s)</span>
                                <button 
                                    type="button" 
                                    onClick={() => {
                                        const updatedSpecs = { ...editedType.requiredStaffBySpecialization };
                                        delete updatedSpecs[spec];
                                        setEditedType(prev => ({
                                            ...prev,
                                            requiredStaffBySpecialization: updatedSpecs
                                        }));
                                    }}
                                    className="remove-spec-button"
                                >
                                    Remove
                                </button>
                            </div>
                        ))}
                    </div>

                    <div className="add-specialization">
                        <h4>Add Staff Requirement:</h4>
                        <div className="specialization-inputs">
                            <select
                                value={selectedSpecialization}
                                onChange={(e) => setSelectedSpecialization(e.target.value)}
                            >
                                <option value="">Select Specialization</option>
                                {specializations.map((spec) => (
                                    <option key={spec.id} value={spec.name}>
                                        {spec.name}
                                    </option>
                                ))}
                            </select>
                            <input
                                type="number"
                                min="1"
                                value={staffCount}
                                onChange={(e) => setStaffCount(e.target.value)}
                                placeholder="Number of staff"
                            />
                            <button 
                                type="button" 
                                onClick={() => {
                                    if (selectedSpecialization && staffCount) {
                                        setEditedType(prev => ({
                                            ...prev,
                                            requiredStaffBySpecialization: {
                                                ...prev.requiredStaffBySpecialization,
                                                [selectedSpecialization]: parseInt(staffCount)
                                            }
                                        }));
                                        setSelectedSpecialization('');
                                        setStaffCount('1');
                                    }
                                }}
                                className="add-spec-button"
                            >
                                Add Requirement
                            </button>
                        </div>
                    </div>

                    <div className="button-group">
                        <button type="submit">Update Operation Type</button>
                        <button type="button" onClick={() => setSelectedType(null)}>Cancel</button>
                    </div>
                </form>
            )}

            {errorMessage && <p className="error-message">{errorMessage}</p>}
            {successMessage && <p className="success-message">{successMessage}</p>}
        </div>
    );
};

export default UpdateOperationType;
