import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import SearchMedicalCondition from './SearchMedicalCondition';
import medicalConditionService from '../../../api/MedicalCondition/medicalConditionService';
import { MedicalConditionDTO } from '../../../dtos/MedicalConditionDTO';

jest.mock('../../../api/MedicalCondition/medicalConditionService');
jest.mock('../../../dtos/MedicalConditionDTO');

describe('SearchMedicalCondition Component', () => {
    beforeEach(() => {
        jest.clearAllMocks();
    });

    test('renders the component with initial state', () => {
        render(
            <MemoryRouter>
                <SearchMedicalCondition />
            </MemoryRouter>
        );

        expect(screen.getByText('Search Medical Conditions')).toBeInTheDocument();
        expect(screen.getByPlaceholderText('Condition Name')).toBeInTheDocument();
        expect(screen.getByText('Select Severity')).toBeInTheDocument();
        expect(screen.getByText('Search')).toBeInTheDocument();
        expect(screen.getByText('Clear Filters')).toBeInTheDocument();
    });

    test('loads initial conditions on mount', async () => {
        const mockResponse = [
            { name: 'Diabetes', severity: 'High', description: 'Type 1 Diabetes' },
        ];
        medicalConditionService.searchMedicalConditions.mockResolvedValue(mockResponse);
        MedicalConditionDTO.fromResponse.mockImplementation((data) => data);
    
        render(
            <MemoryRouter>
                <SearchMedicalCondition />
            </MemoryRouter>
        );
    
        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);
    
        await waitFor(() => {
            const conditionsGrid = screen.getByText('Diabetes');
            expect(conditionsGrid).toBeInTheDocument();
        });
    });

    test('displays an error message if the API call fails', async () => {
        medicalConditionService.searchMedicalConditions.mockRejectedValue(new Error('API Error'));
    
        render(
            <MemoryRouter>
                <SearchMedicalCondition />
            </MemoryRouter>
        );
    
        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);
    
        await waitFor(() => {
            const errorMessage = screen.getByText('Erro ao buscar condições médicas.');
            expect(errorMessage).toBeInTheDocument();
        });
    });

    test('updates filters and triggers search', async () => {
        render(
            <MemoryRouter>
                <SearchMedicalCondition />
            </MemoryRouter>
        );

        const nameInput = screen.getByPlaceholderText('Condition Name');
        const severitySelect = screen.getByRole('combobox');

        fireEvent.change(nameInput, { target: { name: 'name', value: 'Diabetes' } });
        fireEvent.change(severitySelect, { target: { name: 'severity', value: 'High' } });

        expect(nameInput.value).toBe('Diabetes');
        expect(severitySelect.value).toBe('High');
    });

    test('searches conditions with applied filters', async () => {
        const mockResponse = [
            { name: 'Diabetes', severity: 'High', description: 'Type 1 Diabetes' },
        ];
        medicalConditionService.searchMedicalConditions.mockResolvedValue(mockResponse);
        MedicalConditionDTO.fromResponse.mockImplementation((data) => data);

        render(
            <MemoryRouter>
                <SearchMedicalCondition />
            </MemoryRouter>
        );

        const nameInput = screen.getByPlaceholderText('Condition Name');
        const searchButton = screen.getByText('Search');

        fireEvent.change(nameInput, { target: { name: 'name', value: 'Diabetes' } });
        fireEvent.click(searchButton);

        await waitFor(() => {
            expect(medicalConditionService.searchMedicalConditions).toHaveBeenCalledWith({
                name: 'Diabetes'
            });
            expect(screen.getByText('Diabetes')).toBeInTheDocument();
        });
    });

    test('clears filters and resets search', () => {
        render(
            <MemoryRouter>
                <SearchMedicalCondition />
            </MemoryRouter>
        );

        const nameInput = screen.getByPlaceholderText('Condition Name');
        const clearButton = screen.getByText('Clear Filters');

        fireEvent.change(nameInput, { target: { name: 'name', value: 'Diabetes' } });
        expect(nameInput.value).toBe('Diabetes');

        fireEvent.click(clearButton);
        expect(nameInput.value).toBe('');
    });

    
});