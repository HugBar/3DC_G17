import React from 'react';
import { render, screen, fireEvent, waitFor , within} from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import PatientList from './PatientList';
import patientService from '../../../api/patientService';
import userEvent from '@testing-library/user-event';
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

    /*test('handles patient selection', async () => {
        // Mock the getPatientById function
        patientService.getPatientById.mockResolvedValue(mockPatients[0]);
        
        const mockOnSelectPatient = jest.fn();

        render(
            <BrowserRouter>
                <PatientList onSelectPatient={mockOnSelectPatient} />
            </BrowserRouter>
        );

        // Wait for the patient card to be present
        const patientCard = await screen.findByTestId('patient-card');
        await act(async () => {
            fireEvent.click(patientCard);
        });

        // Wait for the async operation to complete
        await waitFor(() => {
            expect(patientService.getPatientById).toHaveBeenCalledWith(1);
        });
        expect(mockOnSelectPatient).toHaveBeenCalledWith(1);
    });
    */

    test('displays patient details when clicking on a patient', async () => {
        // Mock the getPatientById response
        patientService.getPatientById.mockResolvedValue(mockPatients[0]);
        
        render(
            <BrowserRouter>
                <PatientList />
            </BrowserRouter>
        );

        const user = userEvent.setup();

        // Wait for the patient card to be rendered
        const patientCard = await screen.findByTestId('patient-card');
        await user.click(patientCard);

        // Wait for the modal content to appear and make assertions
        const modalContent = await screen.findByTestId('modal-content');
        expect(modalContent).toBeInTheDocument();

        // Check modal content
        expect(within(modalContent).getByText('John Doe')).toBeInTheDocument();
        expect(within(modalContent).getByText('john@example.com')).toBeInTheDocument();
        expect(within(modalContent).getByText('12345')).toBeInTheDocument();
        expect(within(modalContent).getByText('123-456-7890')).toBeInTheDocument();
        expect(within(modalContent).getByText('1990-01-01')).toBeInTheDocument();

        // Close the modal
    });

    test('shows no results message when no patients found', async () => {
        // Mock the service to return empty array
        patientService.getAllPatients.mockResolvedValue({
            items: [],
            totalPages: 0,
            currentPage: 1,
            pageSize: 10,
            totalCount: 0
        });

        render(
            <BrowserRouter>
                <PatientList />
            </BrowserRouter>
        );

        // Wait for and verify the no results message
        await waitFor(() => {
            expect(screen.getByText('No patients found.')).toBeInTheDocument();
        });
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