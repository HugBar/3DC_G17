import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import AddMedicalCondition from './AddMedicalCondition';
import medicalConditionService from '../../../api/MedicalCondition/medicalConditionService';
import { MedicalConditionDTO } from '../../../dtos/MedicalConditionDTO';

describe('AddMedicalCondition Component', () => {
    test('renders form with all fields and button', () => {
        render(<AddMedicalCondition />);
        expect(screen.getByText('Add New Medical Condition')).toBeInTheDocument();
        expect(screen.getByLabelText('Condition Name:')).toBeInTheDocument();
        expect(screen.getByLabelText('Severity:')).toBeInTheDocument();
        expect(screen.getByLabelText('Description:')).toBeInTheDocument();
        expect(screen.getByRole('button', { name: 'Add Medical Condition' })).toBeInTheDocument();
    });

    test('renders with default form values', () => {
        render(<AddMedicalCondition />);
        expect(screen.getByLabelText('Condition Name:').value).toBe('');
        expect(screen.getByLabelText('Severity:').value).toBe('Low');
        expect(screen.getByLabelText('Description:').value).toBe('');
    });

    test('updates form state on input change', () => {
        render(<AddMedicalCondition />);

        const nameInput = screen.getByLabelText('Condition Name:');
        const severitySelect = screen.getByLabelText('Severity:');
        const descriptionInput = screen.getByLabelText('Description:');

        fireEvent.change(nameInput, { target: { value: 'Diabetes' } });
        fireEvent.change(severitySelect, { target: { value: 'High' } });
        fireEvent.change(descriptionInput, { target: { value: 'Type 1 Diabetes' } });

        expect(nameInput.value).toBe('Diabetes');
        expect(severitySelect.value).toBe('High');
        expect(descriptionInput.value).toBe('Type 1 Diabetes');
    });

    test('clears form after successful submission', async () => {
        jest.spyOn(medicalConditionService, 'addMedicalCondition').mockResolvedValue({});

        render(<AddMedicalCondition />);

        const nameInput = screen.getByLabelText('Condition Name:');
        const descriptionInput = screen.getByLabelText('Description:');

        fireEvent.change(nameInput, { target: { value: 'Diabetes' } });
        fireEvent.change(descriptionInput, { target: { value: 'Test description' } });

        await act(async () => {
            fireEvent.submit(screen.getByRole('button', { name: 'Add Medical Condition' }));
        });

        expect(nameInput.value).toBe('');
        expect(descriptionInput.value).toBe('');
        expect(screen.getByLabelText('Severity:').value).toBe('Low');
    });

    test('displays success message after submission', async () => {
        jest.spyOn(medicalConditionService, 'addMedicalCondition').mockResolvedValue({});

        render(<AddMedicalCondition />);

        fireEvent.change(screen.getByLabelText('Condition Name:'), { target: { value: 'Diabetes' } });
        fireEvent.change(screen.getByLabelText('Description:'), { target: { value: 'Test' } });

        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Add Medical Condition' }));
        });

        expect(screen.getByText('Medical condition added successfully!')).toBeInTheDocument();
    });

    test('displays error message on submission failure', async () => {
        jest.spyOn(medicalConditionService, 'addMedicalCondition').mockRejectedValue({
            response: {
                data: { message: 'Error adding medical condition' }
            }
        });

        render(<AddMedicalCondition />);

        fireEvent.change(screen.getByLabelText('Condition Name:'), { target: { value: 'Diabetes' } });

        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Add Medical Condition' }));
        });

        expect(screen.getByText('Error adding medical condition')).toBeInTheDocument();
    });

    test('converts data correctly to request format', () => {
        const medicalConditionDto = new MedicalConditionDTO('Diabetes', 'High', 'Type 1 Diabetes');
        expect(medicalConditionDto.toRequest()).toEqual({
            name: 'Diabetes',
            severity: 'High',
            description: 'Type 1 Diabetes'
        });
    });
});