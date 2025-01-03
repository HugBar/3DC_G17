import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { AuthProvider } from '../../../context/AuthContext';
import SearchMedicalCondition from './SearchMedicalCondition';
import medicalConditionService from '../../../api/MedicalCondition/medicalConditionService';

// Mocks
jest.mock('../../../api/MedicalCondition/medicalConditionService');
jest.mock('react-router-dom', () => ({
    ...jest.requireActual('react-router-dom'),
    useNavigate: () => jest.fn(),
    useLocation: () => ({ search: '' })
}));

const renderWithProviders = (ui) => {
    return render(
        <BrowserRouter>
            <AuthProvider>
                {ui}
            </AuthProvider>
        </BrowserRouter>
    );
};

describe('SearchMedicalCondition Unit Tests', () => {
    beforeEach(() => {
        jest.clearAllMocks();
    });

    describe('Component Rendering', () => {
        test('renders search form with all elements', () => {
            renderWithProviders(<SearchMedicalCondition />);
            
            expect(screen.getByText('Search Medical Conditions')).toBeInTheDocument();
            expect(screen.getByPlaceholderText('Condition Name')).toBeInTheDocument();
            expect(screen.getByText('Select Severity')).toBeInTheDocument();
            expect(screen.getByText('Search')).toBeInTheDocument();
            expect(screen.getByText('Clear Filters')).toBeInTheDocument();
        });

        test('renders empty state message when no conditions', () => {
            renderWithProviders(<SearchMedicalCondition />);
            expect(screen.getByText('No medical conditions found.')).toBeInTheDocument();
        });
    });

    describe('Filter Handling', () => {
        test('updates name filter on input change', () => {
            renderWithProviders(<SearchMedicalCondition />);
            const nameInput = screen.getByPlaceholderText('Condition Name');
            
            fireEvent.change(nameInput, { target: { value: 'Diabetes' } });
            expect(nameInput.value).toBe('Diabetes');
        });

        test('updates severity filter on select change', () => {
            renderWithProviders(<SearchMedicalCondition />);
            const severitySelect = screen.getByRole('combobox');
            
            fireEvent.change(severitySelect, { target: { value: 'High' } });
            expect(severitySelect.value).toBe('High');
        });

        test('clears filters when clear button clicked', () => {
            renderWithProviders(<SearchMedicalCondition />);
            const nameInput = screen.getByPlaceholderText('Condition Name');
            const severitySelect = screen.getByRole('combobox');
            
            fireEvent.change(nameInput, { target: { value: 'Test' } });
            fireEvent.change(severitySelect, { target: { value: 'High' } });
            fireEvent.click(screen.getByText('Clear Filters'));
            
            expect(nameInput.value).toBe('');
            expect(severitySelect.value).toBe('');
        });
    });

    describe('Search Functionality', () => {
        test('calls searchMedicalConditions with correct filters', async () => {
            const mockConditions = [
                { _id: '1', name: 'Test', severity: 'High', description: 'Test desc' }
            ];
            medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);
            
            renderWithProviders(<SearchMedicalCondition />);
            
            const nameInput = screen.getByPlaceholderText('Condition Name');
            fireEvent.change(nameInput, { target: { value: 'Test' } });
            fireEvent.click(screen.getByText('Search'));
            
            await waitFor(() => {
                expect(medicalConditionService.searchMedicalConditions)
                    .toHaveBeenCalledWith({ name: 'Test' });
            });
        });

        test('displays medical conditions after successful search', async () => {
            const mockConditions = [
                { _id: '1', name: 'Diabetes', severity: 'High', description: 'Test description' }
            ];
            medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);
            
            renderWithProviders(<SearchMedicalCondition />);
            fireEvent.click(screen.getByText('Search'));
            
            await waitFor(() => {
                // Check condition name
                expect(screen.getByRole('heading', { name: 'Diabetes' })).toBeInTheDocument();
                
                // Check severity using more specific selector
                const severityElement = screen.getByText((content, element) => {
                    return element.tagName.toLowerCase() === 'p' && 
                           element.textContent.includes('High');
                });
                expect(severityElement).toBeInTheDocument();
                
                // Check description
                const descriptionElement = screen.getByText((content, element) => {
                    return element.tagName.toLowerCase() === 'p' && 
                           element.textContent.includes('Test description');
                });
                expect(descriptionElement).toBeInTheDocument();
            });
        });
    });

    describe('Error Handling', () => {
        test('displays error message when search fails', async () => {
            medicalConditionService.searchMedicalConditions.mockRejectedValueOnce(new Error('API Error'));
            
            renderWithProviders(<SearchMedicalCondition />);
            fireEvent.click(screen.getByText('Search'));
            
            await waitFor(() => {
                expect(screen.getByText('Erro ao buscar condições médicas.')).toBeInTheDocument();
            });
        });

        test('displays not found message for 404 response', async () => {
            medicalConditionService.searchMedicalConditions.mockRejectedValueOnce({
                response: { status: 404 }
            });
            
            renderWithProviders(<SearchMedicalCondition />);
            fireEvent.click(screen.getByText('Search'));
            
            await waitFor(() => {
                expect(screen.getByText('Nenhuma condição médica encontrada.')).toBeInTheDocument();
            });
        });
    });

    describe('Modal Functionality', () => {
        test('opens modal when condition is clicked', async () => {
            const mockConditions = [
                { _id: '1', name: 'Test', severity: 'High', description: 'Test desc' }
            ];
            medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);
            
            renderWithProviders(<SearchMedicalCondition />);
            fireEvent.click(screen.getByText('Search'));
            
            await waitFor(() => {
                fireEvent.click(screen.getByText('Test'));
                expect(screen.getByText('Condition Details')).toBeInTheDocument();
            });
        });

        test('closes modal when close button is clicked', async () => {
            const mockConditions = [
                { _id: '1', name: 'Test', severity: 'High', description: 'Test desc' }
            ];
            medicalConditionService.searchMedicalConditions.mockResolvedValueOnce(mockConditions);
            
            renderWithProviders(<SearchMedicalCondition />);
            fireEvent.click(screen.getByText('Search'));
            
            await waitFor(async () => {
                fireEvent.click(screen.getByText('Test'));
                fireEvent.click(screen.getByText('Close'));
                
                await waitFor(() => {
                    expect(screen.queryByText('Condition Details')).not.toBeInTheDocument();
                });
            });
        });
    });
});