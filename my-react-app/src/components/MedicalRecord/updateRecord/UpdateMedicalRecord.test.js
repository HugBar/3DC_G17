import React from 'react';
import { render, screen, fireEvent, act, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
import { MemoryRouter, Routes, Route } from 'react-router-dom';
import UpdateMedicalRecord from './UpdateMedicalRecord';
import medicalRecordService from '../../../api/medicalRecordService';

// Mock the service module
jest.mock('../../../api/medicalRecordService', () => ({
    getAllMedicalConditions: jest.fn(),
    getAllAllergies: jest.fn(),
    getMedicalRecord: jest.fn(),
    updateMedicalRecord: jest.fn()
}));

const renderWithRouter = (patientId = 'TEST123') => {
    return render(
        <MemoryRouter initialEntries={[`/update/${patientId}`]}>
            <Routes>
                <Route path="/update/:patientId" element={<UpdateMedicalRecord />} />
            </Routes>
        </MemoryRouter>
    );
};

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
        const mockRecord = {
            patientId: 'TEST123',
            conditions: [],
            allergies: []
        };
        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);

        await act(async () => {
            renderWithRouter();
        });
        
        expect(screen.getByRole('heading', { name: 'Update Medical Record' })).toBeInTheDocument();
        expect(screen.getByText('Patient ID: TEST123')).toBeInTheDocument();
    });

    test('should display medical record data when loaded', async () => {
        const mockRecord = {
            patientId: 'TEST123',
            conditions: [{ name: 'Hypertension', severity: 'High' }],
            allergies: [{ name: 'Peanuts', severity: 'High' }]
        };

        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);

        await act(async () => {
            renderWithRouter();
        });

        await waitFor(() => {
            expect(screen.getByText('Hypertension')).toBeInTheDocument();
            expect(screen.getByText('Peanuts')).toBeInTheDocument();
        });
    });

    test('should handle adding and removing conditions', async () => {
        const mockRecord = { 
            patientId: 'TEST123', 
            conditions: [], 
            allergies: [] 
        };
        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);

        await act(async () => {
            renderWithRouter();
        });

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
        const mockRecord = { 
            patientId: 'TEST123', 
            conditions: [], 
            allergies: [] 
        };
        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);
        medicalRecordService.updateMedicalRecord.mockResolvedValue({
            message: 'Medical record updated successfully',
            record: mockRecord
        });

        await act(async () => {
            renderWithRouter();
        });

        // Add a condition
        await waitFor(() => {
            const conditionSelect = screen.getByRole('combobox', { name: /add medical condition/i });
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
        const mockRecord = { 
            patientId: 'TEST123', 
            conditions: [], 
            allergies: [] 
        };
        medicalRecordService.getMedicalRecord.mockResolvedValue(mockRecord);
        medicalRecordService.updateMedicalRecord.mockRejectedValue(new Error('Update failed'));

        await act(async () => {
            renderWithRouter();
        });

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