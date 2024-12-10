// src/components/Specialization/SearchSpecialization/SearchSpecialization.js
import React, { useState, useCallback, useEffect } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import specializationService from '../../../api/specializationService';
import './SearchSpecialization.css';

const SearchSpecialization = () => {
    const navigate = useNavigate();
    const location = useLocation();
    const [specializations, setSpecializations] = useState([]);
    const [errorMessage, setErrorMessage] = useState('');
    const [selectedSpecialization, setSelectedSpecialization] = useState(null);
    
    const [filters, setFilters] = useState({
        name: '',
        description: ''
    });

    const [shouldSearch, setShouldSearch] = useState(false);

    useEffect(() => {
        const loadInitialSpecializations = async () => {
            try {
                const response = await specializationService.searchSpecializations({});
                setSpecializations(response);
                setErrorMessage('');
            } catch (error) {
                setSpecializations([]);
                if (error.response?.status === 404) {
                    setErrorMessage('No specializations found');
                } else {
                setErrorMessage('Error loading specializations');
                }
            }
        };
    
        loadInitialSpecializations();
    }, []);

    useEffect(() => {
        const searchParams = new URLSearchParams(location.search);
        const urlFilters = {
            name: searchParams.get('name') || '',
            description: searchParams.get('description') || ''
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
        navigate(`/specializations/search?${params.toString()}`, { replace: true });
    };

    const clearFilters = () => {
        setFilters({
            name: '',
            description: ''
        });
        navigate('/specializations/search');
    };

    const searchSpecializations = useCallback(async () => {
        try {
            const activeFilters = {};
            if (filters.name) activeFilters.name = filters.name;
            if (filters.description) activeFilters.description = filters.description;

            const response = await specializationService.searchSpecializations(activeFilters);

            setSpecializations(response);
            setErrorMessage('');
        } catch (error) {
            setSpecializations([]);
            setErrorMessage('Error searching specializations');
        }
    }, [filters]);

    const handleSearch = () => {
        setShouldSearch(true);
    };

    useEffect(() => {
        if (shouldSearch) {
            searchSpecializations();
            setShouldSearch(false);
        }
    }, [shouldSearch, searchSpecializations]);

    return (
        <div className="specialization-search-container">
            <h2>Search Specializations</h2>

            <div className="filter-group">
                <div className="filters-section">
                    <div className="filters-inputs">
                        <input
                            type="text"
                            name="name"
                            placeholder="Specialization Name"
                            value={filters.name}
                            onChange={handleFilterChange}
                        />
                        <input
                            type="text"
                            name="description"
                            placeholder="Description"
                            value={filters.description}
                            onChange={handleFilterChange}
                        />
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

            {!errorMessage && specializations.length === 0 && (
                <p className="no-results">No specializations found.</p>
            )}

            {specializations.length > 0 && (
                <div className="specializations-grid">
                    {specializations.map((spec, index) => (
                        <div 
                            key={index}
                            className={`specialization-card ${selectedSpecialization === spec ? 'selected' : ''}`}
                            onClick={() => setSelectedSpecialization(spec)}
                        >
                            <h3>{spec.name}</h3>
                            <p>{spec.description}</p>
                        </div>
                    ))}
                </div>
            )}

            {selectedSpecialization && (
                <div className="specialization-details-modal">
                    <div className="modal-content">
                        <h3>Specialization Details</h3>
                        <div className="details-grid">
                            <p><strong>Name:</strong> {selectedSpecialization.name}</p>
                            <p><strong>Description:</strong> {selectedSpecialization.description}</p>
                        </div>
                        <div className="modal-actions">
                            <button 
                                onClick={() => {
                                    const selectedId = selectedSpecialization?._id;
                                    console.log('Selected Specialization:', selectedSpecialization);
                                    console.log('ID to delete:', selectedId);
                                    if (selectedId) {
                                        navigate(`/specializations/remove/${selectedId}`);
                                    } else {
                                        console.error('No ID found in specialization:', selectedSpecialization);
                                    }
                                }} 
                                className="delete-button"
                            >
                                Delete
                            </button>
                            <button onClick={() => setSelectedSpecialization(null)} className="close-button">
                                Close
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
};

export default SearchSpecialization;