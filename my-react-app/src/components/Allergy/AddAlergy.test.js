import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import AddAllergy from './AddAllergy';
import allergyService from '../../api/allergyService';

// Mock the allergyService
jest.mock('../../api/allergyService');

describe('AddAllergy Component', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});
    });

    afterEach(() => {
        console.error.mockRestore();
    });

    test('renders allergy form with all fields', () => {
        render(<AddAllergy />);
        
        expect(screen.getByText('Add New Allergy')).toBeInTheDocument();
        expect(screen.getByLabelText('Allergen:')).toBeInTheDocument();
        expect(screen.getByLabelText('Severity:')).toBeInTheDocument();
        expect(screen.getByLabelText('Description:')).toBeInTheDocument();
        expect(screen.getByRole('button', { name: 'Add Allergy' })).toBeInTheDocument();
    });

    test('validates required fields', () => {
        render(<AddAllergy />);
        
        const submitButton = screen.getByText('Add Allergy');
        fireEvent.click(submitButton);
        
        expect(screen.getByLabelText('Allergen:')).toBeRequired();
        expect(screen.getByLabelText('Severity:')).toBeRequired();
        expect(screen.getByLabelText('Description:')).toBeRequired();
    });

    test('handles input changes correctly', () => {
        render(<AddAllergy />);
        
        const allergenInput = screen.getByLabelText('Allergen:');
        const severitySelect = screen.getByLabelText('Severity:');
        const descriptionInput = screen.getByLabelText('Description:');
        
        fireEvent.change(allergenInput, { target: { value: 'Peanuts' } });
        fireEvent.change(severitySelect, { target: { value: 'High' } });
        fireEvent.change(descriptionInput, { target: { value: 'Severe peanut allergy' } });
        
        expect(allergenInput.value).toBe('Peanuts');
        expect(severitySelect.value).toBe('High');
        expect(descriptionInput.value).toBe('Severe peanut allergy');
    });

    test('submits form successfully', async () => {
        allergyService.addAllergy.mockResolvedValue({});
        
        render(<AddAllergy />);
        
        const allergenInput = screen.getByLabelText('Allergen:');
        const severitySelect = screen.getByLabelText('Severity:');
        const descriptionInput = screen.getByLabelText('Description:');
        
        fireEvent.change(allergenInput, { target: { value: 'Peanuts' } });
        fireEvent.change(severitySelect, { target: { value: 'High' } });
        fireEvent.change(descriptionInput, { target: { value: 'Severe peanut allergy' } });
        
        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Add Allergy' }));
        });
        
        expect(allergyService.addAllergy).toHaveBeenCalledWith({
            allergen: 'Peanuts',
            severity: 'High',
            description: 'Severe peanut allergy'
        });
        
        expect(screen.getByText('Allergy added successfully')).toBeInTheDocument();
    });

    test('handles submission error', async () => {
        const errorMessage = 'Error adding allergy';
        allergyService.addAllergy.mockRejectedValue({
            response: {
                data: {
                    message: errorMessage
                }
            }
        });
        
        render(<AddAllergy />);
        
        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Add Allergy' }));
        });
        
        expect(screen.getByText(errorMessage)).toBeInTheDocument();
    });

    test('clears form after successful submission', async () => {
        allergyService.addAllergy.mockResolvedValue({});
        
        render(<AddAllergy />);
        
        const allergenInput = screen.getByLabelText('Allergen:');
        const descriptionInput = screen.getByLabelText('Description:');
        
        fireEvent.change(allergenInput, { target: { value: 'Peanuts' } });
        fireEvent.change(descriptionInput, { target: { value: 'Test description' } });
        
        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Add Allergy' }));
        });
        
        expect(allergenInput.value).toBe('');
        expect(screen.getByLabelText('Severity:').value).toBe('Low');
        expect(descriptionInput.value).toBe('');
    });
});