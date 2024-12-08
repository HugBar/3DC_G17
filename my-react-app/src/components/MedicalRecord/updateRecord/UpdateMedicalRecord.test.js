import React from 'react';
import { render, screen, fireEvent, act, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
import UpdateMedicalRecord from './UpdateMedicalRecord';
import medicalRecordService from '../../../api/medicalRecordService';

// Mock the service module
jest.mock('../../../api/medicalRecordService', () => ({
    getAllMedicalConditions: jest.fn(),
    getAllAllergies: jest.fn(),
    getMedicalRecord: jest.fn(),
    updateMedicalRecord: jest.fn()
}));

describe('UpdateMedicalRecord Component', () => {
    const mockAvailableConditions = [
        { _id: '1', name: 'Hypertension', severity: 'High' },
        { _id: '2', name: 'Diabetes', severity: 'Medium' }
    ];

    const mockAvailableAllergies = [
        { _id: '1', name: 'Peanuts', severity: 'High' },
        { _id: '2', name: 'Dust', severity: 'Low' }
    ];

    beforeEach(() => {
        jest.clearAllMocks();
        medicalRecordService.getAllMedicalConditions.mockResolvedValue(mockAvailableConditions);
        medicalRecordService.getAllAllergies.mockResolvedValue(mockAvailableAllergies);
    });

    test('should load and display initial form', async () => {
        await act(async () => {
            render(<UpdateMedicalRecord />);
        });
        
        expect(screen.getByText('Update Medical Record')).toBeInTheDocument();
        expect(screen.getByRole('textbox', { name: /patient medical number/i })).toBeInTheDocument();
        expect(screen.getByText('Search Patient')).toBeInTheDocument();
    });

    test('should handle patient search successfully', async () => {
        const mockRecord = {
            patientId: 'TEST123',
            conditions: [{ name: 'Hypertension', severity: 'High' }],
            allergies: [{ name: 'Peanuts', severity: 'High' }]
        };

        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);

        await act(async () => {
            render(<UpdateMedicalRecord />);
        });
        
        const input = screen.getByRole('textbox', { name: /patient medical number/i });
        await act(async () => {
            fireEvent.change(input, { target: { value: 'TEST123' } });
            fireEvent.click(screen.getByText('Search Patient'));
        });

        await waitFor(() => {
            expect(screen.getByText('Hypertension')).toBeInTheDocument();
            expect(screen.getByText('Peanuts')).toBeInTheDocument();
        });
    });

    test('should handle adding and removing conditions', async () => {
        const mockRecord = { patientId: 'TEST123', conditions: [], allergies: [] };
        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);

        render(<UpdateMedicalRecord />);
        
        // Search for patient first
        fireEvent.change(screen.getByLabelText('Patient Medical Number:'), { 
            target: { value: 'TEST123' } 
        });
        fireEvent.click(screen.getByText('Search Patient'));

        await waitFor(() => {
            expect(screen.getByText('Select a condition')).toBeInTheDocument();
        });

        // Add condition
        const conditionSelect = screen.getByText('Select a condition').parentElement;
        fireEvent.change(conditionSelect, { target: { value: 'Hypertension' } });

        await waitFor(() => {
            expect(screen.getByText('Hypertension')).toBeInTheDocument();
        });

        // Remove condition
        fireEvent.click(screen.getByText('Remove'));

        await waitFor(() => {
            expect(screen.queryByText('Hypertension')).not.toBeInTheDocument();
        });
    });

    test('should handle successful record update', async () => {
        const mockRecord = { patientId: 'TEST123', conditions: [], allergies: [] };
        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);
        medicalRecordService.updateMedicalRecord.mockResolvedValue({
            message: 'Medical record updated successfully',
            record: mockRecord
        });

        await act(async () => {
            render(<UpdateMedicalRecord />);
        });

        // Search for patient
        const input = screen.getByRole('textbox', { name: /patient medical number/i });
        await act(async () => {
            fireEvent.change(input, { target: { value: 'TEST123' } });
            fireEvent.click(screen.getByText('Search Patient'));
        });

        // Add a condition
        const conditionSelect = screen.getByRole('combobox', { name: /add medical condition/i });
        await act(async () => {
            fireEvent.change(conditionSelect, { target: { value: 'Hypertension' } });
        });

        const updateButton = screen.getByRole('button', { name: /update medical record/i });
        await act(async () => {
            fireEvent.click(updateButton);
        });

        await waitFor(() => {
            const successMessage = screen.getByText('Medical record updated successfully');
            expect(successMessage).toHaveClass('success-message');
            expect(successMessage).toHaveAttribute('role', 'alert');
        });
    });

    test('should handle update error', async () => {
        const mockRecord = { patientId: 'TEST123', conditions: [], allergies: [] };
        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);
        medicalRecordService.updateMedicalRecord.mockRejectedValue(new Error('Update failed'));

        await act(async () => {
            render(<UpdateMedicalRecord />);
        });

        // Search for patient
        const input = screen.getByRole('textbox', { name: /patient medical number/i });
        await act(async () => {
            fireEvent.change(input, { target: { value: 'TEST123' } });
            fireEvent.click(screen.getByText('Search Patient'));
        });

        // Update record
        const updateButton = screen.getByRole('button', { name: /update medical record/i });
        await act(async () => {
            fireEvent.click(updateButton);
        });

        await waitFor(() => {
            const errorMessage = screen.getByRole('alert');
            expect(errorMessage).toHaveTextContent('Failed to update medical record');
            expect(errorMessage).toHaveClass('error-message');
        });
    });
}); 