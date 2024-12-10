import React, { useState, useEffect, useCallback } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import allergyService from '../../../api/allergyService';
import { AllergyDTO } from '../../../dtos/AllergyDTO';
import './SearchAllergy.css';

const SearchAllergy = () => {

    const navigate = useNavigate();
    const location = useLocation();
    const [allergies, setAllergies] = useState([]);
    const [errorMessage, setErrorMessage] = useState('');
    const [selectedAllergy, setSelectedAllergy] = useState(null);

    const [filters, setFilters] = useState({
        allergen: '',
        severity: ''
    });

    const [shouldSearch, setShouldSearch] = useState(false);

    useEffect(() => {
        const loadInitialAllergies = async () => {
            try {
                const response = await allergyService.searchAllergies({});
                setAllergies(response);
                setErrorMessage('');
            } catch (error) {
                setAllergies([]);
                setErrorMessage('Error loading allergies');
            }
        };
    
        loadInitialAllergies();
    }, []); 

    useEffect(() => {
        const searchParams = new URLSearchParams(location.search);
        const urlFilters = {
            allergen: searchParams.get('allergen') || '',
            severity: searchParams.get('severity') || ''
        };
        setFilters(urlFilters);
    }, [location.search]);


    const handleFilterChange = (e) => {
        const { name, value } = e.target;
        setFilters(prev => ({ ...prev, [name]: value }));
        updateURL({ ...filters, [name]: value });
    }

    const updateURL = (currentFilters) => {
        const params = new URLSearchParams();
        Object.entries(currentFilters).forEach(([key, value]) => {
            if (value) params.append(key, value);
        });
        navigate(`/allergies/search?${params.toString()}`, { replace: true });
    }

    const clearFilters = () => {
        setFilters({
            allergen: '',
            severity: ''
        });
        navigate('/allergies/search');
    }

    const searchAllergies = useCallback(async () => {
        try {
            const activeFilters = {};
            if (filters.allergen) activeFilters.allergen = filters.allergen;
            if (filters.severity) activeFilters.severity = filters.severity;

            console.log('Enviando filtros para API:', activeFilters);
            const searchParams = {
                allergen: filters.allergen, // Include even if empty
                severity: filters.severity
            };

            const response = await allergyService.searchAllergies(searchParams);

            const mappedConditions = response.map(condition => 
                AllergyDTO.fromResponse(condition)
            );
            setAllergies(mappedConditions);
            setErrorMessage('');
        } catch (error) {
            setAllergies([]);
            setErrorMessage('Error searching allergies');
        }
    }, [filters]);

    const handleSearch = () => {
        setShouldSearch(true);
    };

    useEffect(() => {
        if (shouldSearch) {
            searchAllergies();
            setShouldSearch(false);
        }
    }
    , [shouldSearch, searchAllergies]);

    return (
        <div className="allergy-search-container">
            <h2>Search Allergies</h2>
    
            <div className="filter-group">
                <div className="filters-section">
                    <div className="filters-inputs">
                        <input
                            type="text"
                            name="allergen"
                            placeholder="Allergen Name"
                            value={filters.allergen}
                            onChange={handleFilterChange}
                        />
                        <select 
                            name="severity"
                            value={filters.severity}
                            onChange={handleFilterChange}
                            data-testid="severity-select"
                            className="form-control"
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
    
            {!errorMessage && allergies.length === 0 && (
                <p className="no-results">No allergies found.</p>
            )}
    
            {allergies.length > 0 && (
                <div className="allergies-grid">
                    {allergies.map((allergy, index) => (
                        <div 
                            key={index}
                            className={`allergy-card ${selectedAllergy === allergy ? 'selected' : ''}`}
                            onClick={() => setSelectedAllergy(allergy)}
                        >
                            <h3>{allergy.allergen}</h3>
                            <p>Severity: {allergy.severity}</p>
                        </div>
                    ))}
                </div>
            )}
    
            {selectedAllergy && (
                <div className="modal">
                    <div className="modal-content">
                        <h3>Allergy Details</h3>
                        <div className="details-grid">
                            <p><strong>Allergen:</strong> {selectedAllergy.allergen}</p>
                            <p><strong>Severity:</strong> {selectedAllergy.severity}</p>
                            <p><strong>Description:</strong> {selectedAllergy.description}</p>
                        </div>
                        <button onClick={() => setSelectedAllergy(null)}>Close</button>
                    </div>
                </div>
            )}
        </div>
    );
}

export default SearchAllergy;



