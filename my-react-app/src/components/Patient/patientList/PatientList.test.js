import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import PatientList from './PatientList';
import patientService from '../../../api/patientService';
import { act } from 'react';

// Mock the patientService
jest.mock('../../../api/patientService');

// Mock useNavigate
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
    ...jest.requireActual('react-router-dom'),
    useNavigate: () => mockNavigate
}));

const mockPatients = [
    {
        id: 1,
        firstName: 'John',
        lastName: 'Doe',
        email: 'john@example.com',
        medicalNr: '12345',
        phoneNumber: '123-456-7890',
        dateOfBirth: '1990-01-01'
    }
];

describe('PatientList Component', () => {
    beforeEach(() => {
        // Clear all mocks before each test
        jest.clearAllMocks();
        
        // Setup default mock implementation
        patientService.getAllPatients.mockResolvedValue({
            items: mockPatients,
            totalPages: 1
        });
    });

    test('renders patient list successfully', async () => {
        render(
            <BrowserRouter>
                <PatientList />
            </BrowserRouter>
        );

        // Wait for the patient data to be loaded
        await waitFor(() => {
            expect(screen.getByText('John Doe')).toBeInTheDocument();
        });

        // Use more specific queries that match the actual DOM structure
        expect(screen.getByText('Medical Record #:')).toBeInTheDocument();
        expect(screen.getByText('12345')).toBeInTheDocument();
        expect(screen.getByText('Email:')).toBeInTheDocument();
        expect(screen.getByText('john@example.com')).toBeInTheDocument();
    });

    test('handles filter changes', async () => {
        render(
            <BrowserRouter>
                <PatientList />
            </BrowserRouter>
        );

        const firstNameInput = screen.getByPlaceholderText('First Name');
        await act(async () => {
            fireEvent.change(firstNameInput, { target: { value: 'John' } });
        });

        expect(mockNavigate).toHaveBeenCalledWith(
            expect.stringContaining('firstName=John'),
            expect.any(Object)
        );
    });

    test('handles clear filters', async () => {
        render(
            <BrowserRouter>
                <PatientList />
            </BrowserRouter>
        );

        const clearButton = screen.getByText('Clear Filters');
        await act(async () => {
            fireEvent.click(clearButton);
        });

        expect(mockNavigate).toHaveBeenCalledWith('/patient/list');
    });

    test('handles pagination', async () => {
        patientService.getAllPatients.mockResolvedValue({
            items: mockPatients,
            totalPages: 2
        });

        render(
            <BrowserRouter>
                <PatientList />
            </BrowserRouter>
        );

        await waitFor(() => {
            expect(screen.getByText('Page 1 of 2')).toBeInTheDocument();
        });

        const nextButton = screen.getByText('Next');
        await act(async () => {
            fireEvent.click(nextButton);
        });

        expect(mockNavigate).toHaveBeenCalledWith(
            expect.stringContaining('page=2'),
            expect.any(Object)
        );
    });

    test('handles patient selection', async () => {
        patientService.getPatientById.mockResolvedValue(mockPatients[0]);
        const mockOnSelectPatient = jest.fn();

        render(
            <BrowserRouter>
                <PatientList onSelectPatient={mockOnSelectPatient} />
            </BrowserRouter>
        );

        const patientCard = await screen.findByTestId('patient-card');
        await act(async () => {
            fireEvent.click(patientCard);
        });

        await waitFor(() => {
            expect(patientService.getPatientById).toHaveBeenCalledWith(1);
        });
        expect(mockOnSelectPatient).toHaveBeenCalledWith(1);
    });

    test('handles error state', async () => {
        patientService.getAllPatients.mockRejectedValue({
            response: { status: 404 }
        });

        render(
            <BrowserRouter>
                <PatientList />
            </BrowserRouter>
        );

        await waitFor(() => {
            expect(screen.getByText('No patients found.')).toBeInTheDocument();
        });
    });
});