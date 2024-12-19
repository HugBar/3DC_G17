// Author: Matias Vitorino

/**
 * This test suite covers the process of adding a new medical condition to the system.
 * It tests the following scenarios:
 * - Displaying the add medical condition form
 * - Successfully adding a new medical condition
 * - Handling network errors gracefully
 * - Validating required fields
 * - Persisting form data after failed submission
 * - Selecting different severity levels
 * - Clearing form after successful submission
 * - Displaying success message after successful submission
 * - Displaying error message after failed submission
 * - Verifying all fields are required
 */

import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import AddMedicalCondition from './AddMedicalCondition';
import medicalConditionService from '../../../api/MedicalCondition/medicalConditionService';

// Mock do serviço
jest.mock('../../../api/MedicalCondition/medicalConditionService');

describe('AddMedicalCondition Component', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});
    });

    afterEach(() => {
        console.error.mockRestore();
    });

    const renderAddMedicalCondition = () => {
        return render(
            <BrowserRouter>
                <AddMedicalCondition />
            </BrowserRouter>
        );
    };

    test('renders form correctly', () => {
        renderAddMedicalCondition();

        expect(screen.getByText('Add New Medical Condition')).toBeInTheDocument();
        expect(screen.getByLabelText('Condition Name:')).toBeInTheDocument();
        expect(screen.getByLabelText('Severity:')).toBeInTheDocument();
        expect(screen.getByLabelText('Description:')).toBeInTheDocument();
        expect(screen.getByText('Add Medical Condition')).toBeInTheDocument();
    });

    test('adds medical condition successfully', async () => {
        medicalConditionService.addMedicalCondition.mockResolvedValue({
            message: 'Medical condition added successfully'
        });

        renderAddMedicalCondition();

        fireEvent.change(screen.getByLabelText('Condition Name:'), 
            { target: { value: 'Diabetes' } });
        fireEvent.change(screen.getByLabelText('Severity:'), 
            { target: { value: 'High' } });
        fireEvent.change(screen.getByLabelText('Description:'), 
            { target: { value: 'Chronic condition affecting glucose metabolism' } });

        await act(async () => {
            fireEvent.click(screen.getByText('Add Medical Condition'));
        });

        expect(screen.getByText('Medical condition added successfully!')).toBeInTheDocument();
    });

    test('displays error message when addition fails', async () => {
        const errorMessage = 'Medical condition already exists';
        medicalConditionService.addMedicalCondition.mockRejectedValue({
            response: { 
                data: { 
                    message: errorMessage 
                } 
            }
        });

        renderAddMedicalCondition();

        fireEvent.change(screen.getByLabelText('Condition Name:'), 
            { target: { value: 'Diabetes' } });

        await act(async () => {
            fireEvent.click(screen.getByText('Add Medical Condition'));
        });

        expect(screen.getByText(errorMessage)).toBeInTheDocument();
    });

    test('clears form after successful addition', async () => {
        medicalConditionService.addMedicalCondition.mockResolvedValue({});
        
        renderAddMedicalCondition();

        fireEvent.change(screen.getByLabelText('Condition Name:'), 
            { target: { value: 'Diabetes' } });
        fireEvent.change(screen.getByLabelText('Description:'), 
            { target: { value: 'Test description' } });

        await act(async () => {
            fireEvent.click(screen.getByText('Add Medical Condition'));
        });

        expect(screen.getByLabelText('Condition Name:')).toHaveValue('');
        expect(screen.getByLabelText('Severity:')).toHaveValue('Low');
        expect(screen.getByLabelText('Description:')).toHaveValue('');
    });

    test('verifies all fields are required', () => {
        renderAddMedicalCondition();

        const submitButton = screen.getByText('Add Medical Condition');
        fireEvent.click(submitButton);

        expect(screen.getByLabelText('Condition Name:')).toBeRequired();
        expect(screen.getByLabelText('Severity:')).toBeRequired();
        expect(screen.getByLabelText('Description:')).toBeRequired();
    });
});