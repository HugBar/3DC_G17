import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
import SearchMedicalRecord from './SearchMedicalRecord';
import medicalRecordService from '../../../api/medicalRecordService';

// Mock the medicalRecordService
jest.mock('../../../api/medicalRecordService');

describe('SearchMedicalRecord', () => {
    const mockRecord = {
        _id: '1',
        patientId: 'TEST123',
        conditions: [
            { name: 'Asthma', severity: 'High' }
        ],
        allergies: [
            { name: 'Peanuts', severity: 'High' }
        ]
    };

    beforeEach(() => {
        jest.clearAllMocks();
    });

    test('renders search form with all inputs', () => {
        render(<SearchMedicalRecord />);
        
        expect(screen.getByLabelText(/Patient Medical Number/i)).toBeInTheDocument();
        expect(screen.getByLabelText(/Condition Name/i)).toBeInTheDocument();
        expect(screen.getByLabelText(/Allergy Name/i)).toBeInTheDocument();
        expect(screen.getByRole('button', { name: /search/i })).toBeInTheDocument();
    });

    test('search button should be disabled without patient ID', () => {
        render(<SearchMedicalRecord />);
        
        const searchButton = screen.getByRole('button', { name: /search/i });
        expect(searchButton).toBeDisabled();
    });

    test('should enable search button when patient ID is entered', () => {
        render(<SearchMedicalRecord />);
        
        const patientIdInput = screen.getByLabelText(/Patient Medical Number/i);
        fireEvent.change(patientIdInput, { target: { value: 'TEST123' } });
        
        const searchButton = screen.getByRole('button', { name: /search/i });
        expect(searchButton).not.toBeDisabled();
    });

    test('should display medical record when search is successful', async () => {
        medicalRecordService.searchMedicalRecord.mockResolvedValueOnce(mockRecord);
        
        render(<SearchMedicalRecord />);
        
        const patientIdInput = screen.getByLabelText(/Patient Medical Number/i);
        fireEvent.change(patientIdInput, { target: { value: 'TEST123' } });
        
        const searchButton = screen.getByRole('button', { name: /search/i });
        fireEvent.click(searchButton);
        
        await waitFor(() => {
            expect(screen.getByText('Asthma - Severity: High')).toBeInTheDocument();
            expect(screen.getByText('Peanuts - Severity: High')).toBeInTheDocument();
        });
    });

    test('should display error message when record not found', async () => {
        medicalRecordService.searchMedicalRecord.mockRejectedValueOnce(new Error('Not found'));
        
        render(<SearchMedicalRecord />);
        
        const patientIdInput = screen.getByLabelText(/Patient Medical Number/i);
        fireEvent.change(patientIdInput, { target: { value: 'INVALID123' } });
        
        const searchButton = screen.getByRole('button', { name: /search/i });
        fireEvent.click(searchButton);
        
        await waitFor(() => {
            expect(screen.getByText('Medical record not found')).toBeInTheDocument();
        });
    });

    test('should handle search with only condition name', async () => {
        const conditionOnlyRecord = {
            ...mockRecord,
            allergies: undefined
        };
        
        medicalRecordService.searchMedicalRecord.mockResolvedValueOnce(conditionOnlyRecord);
        
        render(<SearchMedicalRecord />);
        
        fireEvent.change(screen.getByLabelText(/Patient Medical Number/i), { target: { value: 'TEST123' } });
        fireEvent.change(screen.getByLabelText(/Condition Name/i), { target: { value: 'Asthma' } });
        
        fireEvent.click(screen.getByRole('button', { name: /search/i }));
        
        await waitFor(() => {
            expect(screen.getByText('Asthma - Severity: High')).toBeInTheDocument();
            expect(screen.queryByText('Allergies')).not.toBeInTheDocument();
        });
    });
}); 