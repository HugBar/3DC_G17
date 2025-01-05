import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import SearchAllergy from './SearchAllergy';
import allergyService from '../../../api/Allergy/allergyService';
import { AllergyDTO } from '../../../dtos/AllergyDTO';
import { AuthProvider } from '../../../context/AuthContext';

// Mock the API service, DTO and Auth context
jest.mock('../../../api/Allergy/allergyService');
jest.mock('../../../dtos/AllergyDTO');
jest.mock('../../../context/AuthContext', () => ({
  AuthProvider: ({ children }) => children,
  useAuth: () => ({
    isAdmin: false,
    user: { id: '1', name: 'Test User' }
  })
}));

describe('SearchAllergy Component', () => {
    beforeEach(() => {
        jest.clearAllMocks();
    });

    const renderWithProviders = (component) => {
        return render(
            <AuthProvider>
                <MemoryRouter>
                    {component}
                </MemoryRouter>
            </AuthProvider>
        );
    };

    test('renders the component with initial state', () => {
        renderWithProviders(<SearchAllergy />);

        expect(screen.getByText('Search Allergies')).toBeInTheDocument();
        expect(screen.getByPlaceholderText('Allergen Name')).toBeInTheDocument();
        expect(screen.getByText('Select Severity')).toBeInTheDocument();
        expect(screen.getByText('Search')).toBeInTheDocument();
        expect(screen.getByText('Clear Filters')).toBeInTheDocument();
    });

    test('loads initial allergies on mount', async () => {
        const mockResponse = [
            { allergen: 'Peanuts', severity: 'High', description: 'Severe peanut allergy' },
        ];
        allergyService.searchAllergies.mockResolvedValue(mockResponse);
        AllergyDTO.fromResponse.mockImplementation((data) => data);

        renderWithProviders(<SearchAllergy />);

        expect(allergyService.searchAllergies).toHaveBeenCalledWith({});
        await waitFor(() => expect(screen.getByText('Peanuts')).toBeInTheDocument());
        expect(screen.getByText('Severity: High')).toBeInTheDocument();
    });

    test('displays an error message if the API call fails', async () => {
        allergyService.searchAllergies.mockRejectedValue(new Error('API Error'));

        renderWithProviders(<SearchAllergy />);

        await waitFor(() => expect(screen.getByText('Error loading allergies')).toBeInTheDocument());
    });

    test('updates filters and URL on filter change', async () => {
        renderWithProviders(<SearchAllergy />);

        const allergenInput = screen.getByPlaceholderText('Allergen Name');
        const severitySelect = screen.getByTestId('severity-select');

        fireEvent.change(allergenInput, { target: { name: 'allergen', value: 'Peanuts' } });
        fireEvent.change(severitySelect, { target: { name: 'severity', value: 'High' } });

        expect(allergenInput.value).toBe('Peanuts');
        expect(severitySelect.value).toBe('High');
    });

    test('searches allergies with active filters', async () => {
        const mockResponse = [
            { allergen: 'Peanuts', severity: 'High', description: 'Severe peanut allergy' },
        ];
        allergyService.searchAllergies.mockResolvedValue(mockResponse);
        AllergyDTO.fromResponse.mockImplementation((data) => data);

        renderWithProviders(<SearchAllergy />);

        const allergenInput = screen.getByPlaceholderText('Allergen Name');
        const searchButton = screen.getByText('Search');

        fireEvent.change(allergenInput, { target: { name: 'allergen', value: 'Peanuts' } });
        fireEvent.click(searchButton);

        await waitFor(() => expect(allergyService.searchAllergies).toHaveBeenCalledWith({
            allergen: 'Peanuts',
            severity: '',
        }));
        expect(screen.getByText('Peanuts')).toBeInTheDocument();
    });

    test('clears filters and resets state', () => {
        renderWithProviders(<SearchAllergy />);

        const allergenInput = screen.getByPlaceholderText('Allergen Name');
        const clearButton = screen.getByText('Clear Filters');

        fireEvent.change(allergenInput, { target: { name: 'allergen', value: 'Peanuts' } });
        expect(allergenInput.value).toBe('Peanuts');

        fireEvent.click(clearButton);
        expect(allergenInput.value).toBe('');
    });

    test('displays modal with selected allergy details', async () => {
        const mockResponse = [
            { allergen: 'Peanuts', severity: 'High', description: 'Severe peanut allergy' },
        ];
        allergyService.searchAllergies.mockResolvedValue(mockResponse);
        AllergyDTO.fromResponse.mockImplementation((data) => data);

        renderWithProviders(<SearchAllergy />);

        await waitFor(() => screen.getByText('Peanuts'));
        fireEvent.click(screen.getByText('Peanuts'));

        expect(screen.getByText('Allergy Details')).toBeInTheDocument();
        expect(screen.getByText('Allergen:')).toBeInTheDocument();
        expect(screen.getByText('Severity: High')).toBeInTheDocument();
    });
});
