// SearchAllergy.test.js
import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter, useNavigate, useLocation } from 'react-router-dom';
import SearchAllergy from './SearchAllergy';
import allergyService from '../../../api/Allergy/allergyService';

// Mock the dependencies
jest.mock('../../../api/Allergy/allergyService');
jest.mock('react-router-dom', () => ({
    ...jest.requireActual('react-router-dom'),
    useNavigate: jest.fn(),
    useLocation: jest.fn()
}));

describe('SearchAllergy Component', () => {
    const mockNavigate = jest.fn();
    const mockLocation = { search: '' };
    const mockAllergies = [
        { 
            allergen: 'Peanuts', 
            severity: 'High',
            description: 'Severe peanut allergy'
        },
        { 
            allergen: 'Lactose', 
            severity: 'Medium',
            description: 'Dairy allergy'
        }
    ];

    beforeEach(() => {
        jest.clearAllMocks();
        useNavigate.mockImplementation(() => mockNavigate);
        useLocation.mockImplementation(() => mockLocation);
    });

    const renderSearchAllergy = () => {
        return render(
            <BrowserRouter>
                <SearchAllergy />
            </BrowserRouter>
        );
    };

    test('renders search form correctly', () => {
        renderSearchAllergy();
        
        expect(screen.getByText('Search Allergies')).toBeInTheDocument();
        expect(screen.getByPlaceholderText('Allergen Name')).toBeInTheDocument();
        expect(screen.getByText('Select Severity')).toBeInTheDocument();
        expect(screen.getByText('Search')).toBeInTheDocument();
        expect(screen.getByText('Clear Filters')).toBeInTheDocument();
    });

    test('loads initial allergies on mount', async () => {
        allergyService.searchAllergies.mockResolvedValue(mockAllergies);
        
        await act(async () => {
            renderSearchAllergy();
        });

        expect(allergyService.searchAllergies).toHaveBeenCalledWith({});
        expect(screen.getByText('Peanuts')).toBeInTheDocument();
        expect(screen.getByText('Lactose')).toBeInTheDocument();
    });

    test('filters allergies by allergen', async () => {
        allergyService.searchAllergies.mockResolvedValue([mockAllergies[0]]);
        
        renderSearchAllergy();

        const allergenInput = screen.getByPlaceholderText('Allergen Name');
        fireEvent.change(allergenInput, { target: { value: 'Peanut' } });
        
        const searchButton = screen.getByText('Search');
        await act(async () => {
            fireEvent.click(searchButton);
        });

        expect(allergyService.searchAllergies).toHaveBeenCalledWith({
            allergen: 'Peanut',
            severity: ''
        });
        expect(screen.getByText('Peanuts')).toBeInTheDocument();
    });

    test('filters allergies by severity', async () => {
        allergyService.searchAllergies.mockResolvedValue([mockAllergies[0]]);
        
        renderSearchAllergy();

        const severitySelect = screen.getByRole('combobox');
        fireEvent.change(severitySelect, { target: { value: 'High' } });
        
        const searchButton = screen.getByText('Search');
        await act(async () => {
            fireEvent.click(searchButton);
        });

        expect(allergyService.searchAllergies).toHaveBeenCalledWith({
            allergen: '',
            severity: 'High'
        });
    });

    test('clears filters when clicking clear button', async () => {
        renderSearchAllergy();

        const allergenInput = screen.getByPlaceholderText('Allergen Name');
        const severitySelect = screen.getByRole('combobox');
        
        fireEvent.change(allergenInput, { target: { value: 'test' } });
        fireEvent.change(severitySelect, { target: { value: 'High' } });
        
        const clearButton = screen.getByText('Clear Filters');
        fireEvent.click(clearButton);

        expect(allergenInput.value).toBe('');
        expect(severitySelect.value).toBe('');
        expect(mockNavigate).toHaveBeenCalledWith('/allergies/search');
    });

    test('displays error message when search fails', async () => {
        allergyService.searchAllergies.mockRejectedValue(new Error('Search failed'));
        
        renderSearchAllergy();

        const searchButton = screen.getByText('Search');
        await act(async () => {
            fireEvent.click(searchButton);
        });
        
        expect(screen.getByText('Error searching allergies')).toBeInTheDocument();
    });

    test('updates URL with search parameters', () => {
        renderSearchAllergy();

        const allergenInput = screen.getByPlaceholderText('Allergen Name');
        fireEvent.change(allergenInput, { target: { value: 'Peanut' } });

        expect(mockNavigate).toHaveBeenCalledWith(
            expect.stringContaining('allergen=Peanut'),
            expect.any(Object)
        );
    });

    test('displays allergy details in modal when clicking on allergy', async () => {
        allergyService.searchAllergies.mockResolvedValue(mockAllergies);
        
        await act(async () => {
            renderSearchAllergy();
        });

        const allergyCard = screen.getByText('Peanuts');
        fireEvent.click(allergyCard);

        expect(screen.getByText('Allergy Details')).toBeInTheDocument();
        expect(screen.getByText('Severe peanut allergy')).toBeInTheDocument();
    });

    test('closes modal when clicking close button', async () => {
        allergyService.searchAllergies.mockResolvedValue(mockAllergies);
        
        await act(async () => {
            renderSearchAllergy();
        });

        const allergyCard = screen.getByText('Peanuts');
        fireEvent.click(allergyCard);

        const closeButton = screen.getByText('Close');
        fireEvent.click(closeButton);

        expect(screen.queryByText('Allergy Details')).not.toBeInTheDocument();
    });
});