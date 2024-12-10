// SearchSpecialization.test.js
import React from 'react';
import { render, screen, fireEvent, act, waitFor } from '@testing-library/react';
import { BrowserRouter, useNavigate, useLocation } from 'react-router-dom';
import SearchSpecialization from './SearchSpecialization';
import specializationService from '../../../api/specializationService';

// Mock the dependencies
jest.mock('../../../api/specializationService');
jest.mock('react-router-dom', () => ({
    ...jest.requireActual('react-router-dom'),
    useNavigate: jest.fn(),
    useLocation: jest.fn()
}));


describe('SearchSpecialization Component', () => {
    const mockNavigate = jest.fn();
    const mockLocation = { search: '' };
    const mockSpecializations = [
        { 
            name: 'Cardiology', 
            description: 'Heart specialist'
        },
        { 
            name: 'Neurology', 
            description: 'Brain specialist'
        }
    ];

    beforeEach(() => {
        jest.clearAllMocks();
        useNavigate.mockImplementation(() => mockNavigate);
        useLocation.mockImplementation(() => mockLocation);
    });

    const renderSearchSpecialization = () => {
        return render(
            <BrowserRouter>
                <SearchSpecialization />
            </BrowserRouter>
        );
    };

    test('renders search form correctly', () => {
        renderSearchSpecialization();

        expect(screen.getByText('Search Specializations')).toBeInTheDocument();
        expect(screen.getByPlaceholderText('Specialization Name')).toBeInTheDocument();
        expect(screen.getByPlaceholderText('Description')).toBeInTheDocument();
        expect(screen.getByText('Search')).toBeInTheDocument();
        expect(screen.getByText('Clear Filters')).toBeInTheDocument();
    });

    test('loads initial specializations on mount', async () => {
        specializationService.searchSpecializations.mockResolvedValue(mockSpecializations);

        await act(async () => {
            renderSearchSpecialization();
        });

        expect(specializationService.searchSpecializations).toHaveBeenCalledWith({});
    

        expect(screen.getByText('Cardiology')).toBeInTheDocument();
        expect(screen.getByText('Neurology')).toBeInTheDocument();
    });

    test('filters specializations by name', async () => {
        specializationService.searchSpecializations.mockResolvedValue([mockSpecializations[0]]);

        renderSearchSpecialization();

        const nameInput = screen.getByPlaceholderText('Specialization Name');
        fireEvent.change(nameInput, { target: { value: 'Cardio' } });

        const searchButton = screen.getByText('Search');
        await act(async () => {
            fireEvent.click(searchButton);
        });

        expect(specializationService.searchSpecializations).toHaveBeenCalledWith({
            name: 'Cardio'
        });
        expect(screen.getByText('Cardiology')).toBeInTheDocument();
    });

    test('filters specializations by description', async () => {
        specializationService.searchSpecializations.mockResolvedValue([mockSpecializations[1]]);

        renderSearchSpecialization();

        const descriptionInput = screen.getByPlaceholderText('Description');
        fireEvent.change(descriptionInput, { target: { value: 'Brain' } });

        const searchButton = screen.getByText('Search');
        await act(async () => {
            fireEvent.click(searchButton);
        });

        expect(specializationService.searchSpecializations).toHaveBeenCalledWith({
            description: 'Brain'
        });
        expect(screen.getByText('Neurology')).toBeInTheDocument();
    });

    test('clears filters when clicking clear button', () => {
        renderSearchSpecialization();

        const nameInput = screen.getByPlaceholderText('Specialization Name');
        const descriptionInput = screen.getByPlaceholderText('Description');

        fireEvent.change(nameInput, { target: { value: 'Test' } });
        fireEvent.change(descriptionInput, { target: { value: 'Test Desc' } });

        const clearButton = screen.getByText('Clear Filters');
        fireEvent.click(clearButton);

        expect(nameInput.value).toBe('');
        expect(descriptionInput.value).toBe('');
        expect(mockNavigate).toHaveBeenCalledWith('/specializations/search');
    });

    test('displays error message when search fails', async () => {
        specializationService.searchSpecializations.mockRejectedValue(new Error('Search failed'));

        renderSearchSpecialization();

        const searchButton = screen.getByText('Search');
        await act(async () => {
            fireEvent.click(searchButton);
        });

        expect(screen.getByText('Error searching specializations')).toBeInTheDocument();
    });

    test('updates URL with search parameters', () => {
        renderSearchSpecialization();

        const nameInput = screen.getByPlaceholderText('Specialization Name');
        fireEvent.change(nameInput, { target: { value: 'Cardio' } });

        expect(mockNavigate).toHaveBeenCalledWith(
            expect.stringContaining('name=Cardio'),
            { replace: true }
        );
    });

    test('displays no results message when no specializations found', async () => {
        specializationService.searchSpecializations.mockResolvedValue([]);

        renderSearchSpecialization();

        const searchButton = screen.getByText('Search');
        await act(async () => {
            fireEvent.click(searchButton);
        });

        expect(screen.getByText('No specializations found.')).toBeInTheDocument();
    });
});
