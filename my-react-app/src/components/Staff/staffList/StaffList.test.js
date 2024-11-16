import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import StaffList from './StaffList';
import staffService from '../../../api/staffService';
import { act } from 'react';
import userEvent from '@testing-library/user-event';

// Mock the staffService
jest.mock('../../../api/staffService');

// Mock useNavigate
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
    ...jest.requireActual('react-router-dom'),
    useNavigate: () => mockNavigate,
    useLocation: () => ({
        search: '',
        pathname: '/staff/filter'
    })
}));

const mockStaffMembers = [
    {
        id: 1,
        firstName: 'John',
        lastName: 'Doe',
        email: 'john@example.com',
        specialization: 'Cardiology',
        phoneNumber: '123456789',
        licenseNumber: 'LIC123',
        availabilitySlots: [
            {
                startTime: '2024-03-20T10:00:00',
                endTime: '2024-03-20T12:00:00'
            }
        ]
    }
];

describe('StaffList Component', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        staffService.getAllStaff.mockResolvedValue({
            items: mockStaffMembers,
            totalPages: 1
        });
    });

    test('renders staff list successfully', async () => {
        render(
            <BrowserRouter>
                <StaffList />
            </BrowserRouter>
        );

        await waitFor(() => {
            expect(screen.getByText('John Doe')).toBeInTheDocument();
        });

        expect(screen.getByText('Email:')).toBeInTheDocument();
        expect(screen.getByText('john@example.com')).toBeInTheDocument();
        expect(screen.getByText('Specialization:')).toBeInTheDocument();
        expect(screen.getByText('Cardiology')).toBeInTheDocument();
    });

    test('handles filter changes', async () => {
        render(
            <BrowserRouter>
                <StaffList />
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
                <StaffList />
            </BrowserRouter>
        );

        const clearButton = screen.getByText('Clear Filters');
        await act(async () => {
            fireEvent.click(clearButton);
        });

        expect(mockNavigate).toHaveBeenCalledWith('/staff/filter');
    });

    test('handles pagination', async () => {
        staffService.getAllStaff.mockResolvedValue({
            items: mockStaffMembers,
            totalPages: 2
        });

        render(
            <BrowserRouter>
                <StaffList />
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

    test('handles staff selection and modal display', async () => {
        staffService.getStaffById.mockResolvedValue(mockStaffMembers[0]);
        const mockOnSelectStaff = jest.fn();

        render(
            <BrowserRouter>
                <StaffList onSelectStaff={mockOnSelectStaff} />
            </BrowserRouter>
        );

        await waitFor(() => {
            expect(screen.getByText('John Doe')).toBeInTheDocument();
        });

        await act(async () => {
            fireEvent.click(screen.getByText('John Doe'));
        });

        expect(screen.getByText('Staff Details')).toBeInTheDocument();
        expect(screen.getByText('Update Staff')).toBeInTheDocument();
        expect(screen.getByText('Deactivate Staff')).toBeInTheDocument();
    });

    test('handles error state', async () => {
        staffService.getAllStaff.mockRejectedValue({
            response: { status: 404 }
        });

        render(
            <BrowserRouter>
                <StaffList />
            </BrowserRouter>
        );

        await waitFor(() => {
            expect(screen.getByText('No staff members found.')).toBeInTheDocument();
        });
    });

    test('handles update staff button click', async () => {
        staffService.getStaffById.mockResolvedValue(mockStaffMembers[0]);
        const mockOnSelectStaff = jest.fn();

        render(
            <BrowserRouter>
                <StaffList onSelectStaff={mockOnSelectStaff} />
            </BrowserRouter>
        );

        await waitFor(() => {
            expect(screen.getByText('John Doe')).toBeInTheDocument();
        });

        await act(async () => {
            fireEvent.click(screen.getByText('John Doe'));
        });

        await act(async () => {
            fireEvent.click(screen.getByText('Update Staff'));
        });

        expect(mockOnSelectStaff).toHaveBeenCalledWith(1);
    });

    test('handles deactivate staff button click', async () => {
        staffService.getStaffById.mockResolvedValue(mockStaffMembers[0]);
        const mockOnDeactivateStaff = jest.fn();

        render(
            <BrowserRouter>
                <StaffList onDeactivateStaff={mockOnDeactivateStaff} />
            </BrowserRouter>
        );

        await waitFor(() => {
            expect(screen.getByText('John Doe')).toBeInTheDocument();
        });

        await act(async () => {
            fireEvent.click(screen.getByText('John Doe'));
        });

        await act(async () => {
            fireEvent.click(screen.getByText('Deactivate Staff'));
        });

        expect(mockOnDeactivateStaff).toHaveBeenCalledWith(1);
    });
});