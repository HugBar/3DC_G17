import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { AuthProvider } from '../../../context/AuthContext';
import SearchMedicalCondition from './SearchMedicalCondition';
import medicalConditionService from '../../../api/MedicalCondition/medicalConditionService';

// Mock the service
jest.mock('../../../api/MedicalCondition/medicalConditionService');

// Mock useNavigate
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
    ...jest.requireActual('react-router-dom'),
    useNavigate: () => mockNavigate
}));

const renderComponent = () => {
    return render(
        <BrowserRouter>
            <AuthProvider>
                <SearchMedicalCondition />
            </AuthProvider>
        </BrowserRouter>
    );
};

describe('SearchMedicalCondition Integration Tests', () => {
    beforeEach(() => {
        jest.clearAllMocks();
    });

    test('should search medical conditions with name filter', async () => {
        // Mock response data
        const mockConditions = [
            { _id: '1', name: 'Diabetes', severity: 'High', description: 'Test description' }
        ];
        medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);

        renderComponent();

        // Fill search form
        const nameInput = screen.getByPlaceholderText('Condition Name');
        fireEvent.change(nameInput, { target: { value: 'Diabetes' } });

        // Click search button
        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);

        // Verify service was called with correct parameters
        await waitFor(() => {
            expect(medicalConditionService.searchMedicalConditions).toHaveBeenCalledWith({
                name: 'Diabetes'
            });
        });

        // Verify results are displayed
        await waitFor(() => {
            expect(screen.getByText('Diabetes')).toBeInTheDocument();
        });
    });

    test('should search medical conditions with severity filter', async () => {
        const mockConditions = [
            { _id: '1', name: 'Condition1', severity: 'High', description: 'Test description' }
        ];
        medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);

        renderComponent();

        // Select severity
        const severitySelect = screen.getByRole('combobox');
        fireEvent.change(severitySelect, { target: { value: 'High' } });

        // Search
        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);

        await waitFor(() => {
            expect(medicalConditionService.searchMedicalConditions).toHaveBeenCalledWith({
                severity: 'High'
            });
        });
    });

    test('should handle no results found', async () => {
        medicalConditionService.searchMedicalConditions.mockRejectedValueOnce({
            response: { status: 404 }
        });

        renderComponent();

        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);

        await waitFor(() => {
            expect(screen.getByText('Nenhuma condição médica encontrada.')).toBeInTheDocument();
        });
    });

    test('should handle API error', async () => {
        medicalConditionService.searchMedicalConditions.mockRejectedValueOnce(new Error('API Error'));

        renderComponent();

        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);

        await waitFor(() => {
            expect(screen.getByText('Erro ao buscar condições médicas.')).toBeInTheDocument();
        });
    });

    test('should clear filters and results', async () => {
        const mockConditions = [
            { _id: '1', name: 'Test', severity: 'High', description: 'Test description' }
        ];
        medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);

        renderComponent();

        // Set filters
        const nameInput = screen.getByPlaceholderText('Condition Name');
        fireEvent.change(nameInput, { target: { value: 'Test' } });

        // Clear filters
        const clearButton = screen.getByText('Clear Filters');
        fireEvent.click(clearButton);

        // Verify filters are cleared
        expect(nameInput.value).toBe('');
        expect(mockNavigate).toHaveBeenCalledWith('/medical-conditions/search');
    });
});

describe('SearchMedicalCondition Additional Integration Tests', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        localStorage.clear();
    });

    test('should search with combined name and severity filters', async () => {
        const mockConditions = [
            { _id: '1', name: 'Diabetes', severity: 'High', description: 'Test description' }
        ];
        medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);

        renderComponent();

        // Set both filters
        const nameInput = screen.getByPlaceholderText('Condition Name');
        const severitySelect = screen.getByRole('combobox');
        
        fireEvent.change(nameInput, { target: { value: 'Diabetes' } });
        fireEvent.change(severitySelect, { target: { value: 'High' } });

        // Search
        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);

        await waitFor(() => {
            expect(medicalConditionService.searchMedicalConditions).toHaveBeenCalledWith({
                name: 'Diabetes',
                severity: 'High'
            });
        });
    });

    test('should open and close condition detail modal', async () => {
        const mockCondition = { 
            _id: '1', 
            name: 'Condition1', 
            severity: 'High', 
            description: 'Detailed description' 
        };
        medicalConditionService.searchMedicalConditions.mockResolvedValueOnce([mockCondition]);

        renderComponent();

        // Search and click on result
        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);

        await waitFor(() => {
            const conditionCard = screen.getByText('Condition1').closest('.condition-card');
            fireEvent.click(conditionCard);
        });

        // Verify modal content using getAllByText and selecting first occurrence
        await waitFor(() => {
            const descriptions = screen.getAllByText(/Detailed description/i);
            expect(descriptions[0]).toBeInTheDocument();
        });

        // Close modal
        const closeButton = screen.getByText('Close');
        fireEvent.click(closeButton);

        // Verify modal is closed
        await waitFor(() => {
            const modal = screen.queryByRole('dialog');
            expect(modal).not.toBeInTheDocument();
        });
    });

    test('should handle authentication token in requests', async () => {
        const mockToken = 'fake-auth-token';
        localStorage.setItem('authToken', mockToken);

        // Mock getAuthToken directly
        jest.spyOn(medicalConditionService, 'getAuthToken').mockReturnValue(mockToken);

        const mockConditions = [
            { _id: '1', name: 'Test', severity: 'Medium', description: 'Test' }
        ];
        medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);

        renderComponent();

        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);

        await waitFor(() => {
            expect(medicalConditionService.getAuthToken()).toBe(mockToken);
        });
    });

    test('should update search history in URL', async () => {
        renderComponent();

        const nameInput = screen.getByPlaceholderText('Condition Name');
        fireEvent.change(nameInput, { target: { value: 'TestCondition' } });

        const searchButton = screen.getByText('Search');
        fireEvent.click(searchButton);

        await waitFor(() => {
            expect(mockNavigate).toHaveBeenCalledWith(
                '/medical-conditions/search?name=TestCondition',
                expect.anything()
            );
        });
    });
});