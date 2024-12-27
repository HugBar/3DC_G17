import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import AddAllergy from './AddAllergy';
import allergyService from '../../api/Allergy/allergyService';
import { AllergyDTO } from '../../dtos/AllergyDTO';

describe('AddAllergy Component', () => {
    test('renders form with all fields and button', () => {
        render(<AddAllergy />);
        expect(screen.getByText('Add New Allergy')).toBeInTheDocument();
        expect(screen.getByLabelText('Allergen:')).toBeInTheDocument();
        expect(screen.getByLabelText('Severity:')).toBeInTheDocument();
        expect(screen.getByLabelText('Description:')).toBeInTheDocument();
        expect(screen.getByRole('button', { name: 'Add Allergy' })).toBeInTheDocument();
    });

    test('renders with default form values', () => {
        render(<AddAllergy />);
        expect(screen.getByLabelText('Allergen:').value).toBe('');
        expect(screen.getByLabelText('Severity:').value).toBe('Low');
        expect(screen.getByLabelText('Description:').value).toBe('');
    });
    
    test('updates form state on input change', () => {
        render(<AddAllergy />);
    
        const allergenInput = screen.getByLabelText('Allergen:');
        const severitySelect = screen.getByLabelText('Severity:');
        const descriptionInput = screen.getByLabelText('Description:');
    
        fireEvent.change(allergenInput, { target: { value: 'Peanuts' } });
        fireEvent.change(severitySelect, { target: { value: 'High' } });
        fireEvent.change(descriptionInput, { target: { value: 'Severe allergy' } });
    
        expect(allergenInput.value).toBe('Peanuts');
        expect(severitySelect.value).toBe('High');
        expect(descriptionInput.value).toBe('Severe allergy');
    });
    
    test('clears form after successful submission', async () => {
        // Mocking the service for isolation
        jest.spyOn(allergyService, 'addAllergy').mockResolvedValue({});
    
        render(<AddAllergy />);
    
        const allergenInput = screen.getByLabelText('Allergen:');
        const descriptionInput = screen.getByLabelText('Description:');
    
        fireEvent.change(allergenInput, { target: { value: 'Peanuts' } });
        fireEvent.change(descriptionInput, { target: { value: 'Test description' } });
    
        await act(async () => {
            fireEvent.submit(screen.getByRole('button', { name: 'Add Allergy' }));
        });
    
        // Verifica se o formulário foi limpo
        expect(allergenInput.value).toBe('');
        expect(descriptionInput.value).toBe('');
        expect(screen.getByLabelText('Severity:').value).toBe('Low');
    });

    test('displays success message after submission', async () => {
        jest.spyOn(allergyService, 'addAllergy').mockResolvedValue({});
    
        render(<AddAllergy />);
    
        fireEvent.change(screen.getByLabelText('Allergen:'), { target: { value: 'Peanuts' } });
        fireEvent.change(screen.getByLabelText('Description:'), { target: { value: 'Test' } });
    
        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Add Allergy' }));
        });
    
        expect(screen.getByText('Allergy added successfully')).toBeInTheDocument();
    });

    test('displays error message on submission failure', async () => {
        jest.spyOn(allergyService, 'addAllergy').mockRejectedValue({
            response: {
                data: { message: 'Error adding allergy' }
            }
        });
    
        render(<AddAllergy />);
    
        fireEvent.change(screen.getByLabelText('Allergen:'), { target: { value: 'Peanuts' } });
    
        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Add Allergy' }));
        });
    
        expect(screen.getByText('Error adding allergy')).toBeInTheDocument();
    });

    test('converts data correctly to request format', () => {
        const allergyDto = new AllergyDTO('Peanuts', 'High', 'Severe allergy');
        expect(allergyDto.toRequest()).toEqual({
            allergen: 'Peanuts',
            severity: 'High',
            description: 'Severe allergy'
        });
    });

});