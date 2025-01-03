// Author: Matias Vitorino

/**
 * This component allows Doctors to search for medical conditions in the system.
 * It includes form fields for filtering by condition name and severity.
 * - The search triggers a GET request to the backend to retrieve matching conditions.
 * - The results are displayed in a grid with condition name, severity, and description.
 * - Clicking on a condition opens a modal with detailed information.
 * - The modal can be closed by clicking a button or outside the content area.
 * - Error messages are displayed when no conditions are found or an error occurs.
 * - The search results are cleared when the filters are reset.
 */

import React, { useState, useEffect, useCallback } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import medicalConditionService from '../../../api/MedicalCondition/medicalConditionService';
import { useAuth } from '../../../context/AuthContext';
import './SearchMedicalCondition.css';

const SearchMedicalCondition = () => {
    const navigate = useNavigate();
    const location = useLocation();
    const [conditions, setConditions] = useState([]);
    const [errorMessage, setErrorMessage] = useState('');
    const [selectedCondition, setSelectedCondition] = useState(null);
    const { isAdmin } = useAuth();
    
    const [filters, setFilters] = useState({
        name: '',
        severity: ''
    });

    const [shouldSearch, setShouldSearch] = useState(false);

    useEffect(() => {
        const searchParams = new URLSearchParams(location.search);
        const urlFilters = {
            name: searchParams.get('name') || '',
            severity: searchParams.get('severity') || ''
        };
        setFilters(urlFilters);
    }, [location.search]);

    const handleFilterChange = (e) => {
        const { name, value } = e.target;
        setFilters(prev => ({ ...prev, [name]: value }));
        updateURL({ ...filters, [name]: value });
    };

    const updateURL = (currentFilters) => {
        const params = new URLSearchParams();
        Object.entries(currentFilters).forEach(([key, value]) => {
            if (value) params.append(key, value);
        });
        navigate(`/medical-conditions/search?${params.toString()}`, { replace: true });
    };

    const clearFilters = () => {
        setFilters({
            name: '',
            severity: ''
        });
        navigate('/medical-conditions/search');
    };

    const searchConditions = useCallback(async () => {
        try {
            const activeFilters = {};
            if (filters.name) activeFilters.name = filters.name;
            if (filters.severity) activeFilters.severity = filters.severity;

            console.log('Enviando filtros para API:', activeFilters);

            const response = await medicalConditionService.searchMedicalConditions(activeFilters);
            console.log('Resposta da API:', response);

            const mappedConditions = response.map(condition => ({
                _id: condition._id, // Preserve the _id
                name: condition.name,
                severity: condition.severity,
                description: condition.description
            }));

            setConditions(mappedConditions);
            setErrorMessage('');
        } catch (error) {
            console.error('Erro completo:', error);
            if (error.response?.status === 404) {
                setConditions([]);
                setErrorMessage('Nenhuma condição médica encontrada.');
            } else {
                setErrorMessage('Erro ao buscar condições médicas.');
            }
        }
    }, [filters]);

    const handleSearch = () => {
        setShouldSearch(true);
    };

    useEffect(() => {
        if (shouldSearch) {
            searchConditions();
            setShouldSearch(false);
        }
    }, [shouldSearch, searchConditions]);

    return (
        <div className="medical-condition-search-container">
            <h2>Search Medical Conditions</h2>

            <div className="filter-group">
                <div className="filters-section">
                    <div className="filters-inputs">
                        <input
                            type="text"
                            name="name"
                            placeholder="Condition Name"
                            value={filters.name}
                            onChange={handleFilterChange}
                        />
                        <select
                            name="severity"
                            value={filters.severity}
                            onChange={handleFilterChange}
                        >
                            <option value="">Select Severity</option>
                            <option value="Low">Low</option>
                            <option value="Medium">Medium</option>
                            <option value="High">High</option>
                        </select>
                    </div>
                    <div className="filters-buttons">
                        <button onClick={handleSearch} className="search-button">
                            Search
                        </button>
                        <button onClick={clearFilters} className="clear-filters-button">
                            Clear Filters
                        </button>
                    </div>
                </div>
            </div>

            {errorMessage && <div className="error-message">{errorMessage}</div>}

            {!errorMessage && conditions.length === 0 && (
                <p className="no-results">No medical conditions found.</p>
            )}

            {conditions.length > 0 && (
                <div className="conditions-grid">
                    {conditions.map((condition, index) => (
                        <div 
                            key={index}
                            className={`condition-card ${selectedCondition === condition ? 'selected' : ''}`}
                            onClick={() => setSelectedCondition(condition)}
                        >
                            <h3>{condition.name}</h3>
                            <p><strong>Severity:</strong> {condition.severity}</p>
                            <p><strong>Description:</strong> {condition.description}</p>
                        </div>
                    ))}
                </div>
            )}

            {selectedCondition && (
                <div className="condition-details-modal">
                    <div className="modal-content">
                        <h3>Condition Details</h3>
                        <div className="details-grid">
                            <p><strong>Name:</strong> {selectedCondition.name}</p>
                            <p><strong>Severity:</strong> {selectedCondition.severity}</p>
                            <p><strong>Description:</strong> {selectedCondition.description}</p>
                        </div>
                        <div className="modal-actions">
                            <button onClick={() => setSelectedCondition(null)} className="close-button">
                                Close
                            </button>
                            {isAdmin && (
                                    <button 
                                        onClick={() => {
                                            if (selectedCondition && selectedCondition._id) { // Check for _id directly
                                                navigate(`/medical-conditions/update/${selectedCondition._id}`);
                                            } else {
                                                console.error('No condition ID available');
                                            }
                                        }} 
                                        className="update-button"
                                    >
                                        Update
                                    </button>
                                )}
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
};

export default SearchMedicalCondition;
