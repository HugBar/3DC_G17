import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter, useNavigate, useParams } from 'react-router-dom';
import RemoveSpecialization from './RemoveSpecialization';
import specializationService from '../../../api/specializationService';

// Mock dependencies
jest.mock('react-router-dom', () => ({
    ...jest.requireActual('react-router-dom'),
    useNavigate: jest.fn(),
    useParams: jest.fn()
}));

jest.mock('../../../api/specializationService');

describe('RemoveSpecialization Component', () => {
    const mockNavigate = jest.fn();
    
    beforeEach(() => {
        useNavigate.mockReturnValue(mockNavigate);
        jest.clearAllMocks();
        // Mock setTimeout
        jest.useFakeTimers();
    });

    afterEach(() => {
        // Cleanup timers
        jest.useRealTimers();
    });

    test('renders confirmation message and buttons', () => {
        useParams.mockReturnValue({ id: '123' });
        
        render(
            <MemoryRouter>
                <RemoveSpecialization />
            </MemoryRouter>
        );

        expect(screen.getByText('Delete Specialization')).toBeInTheDocument();
        expect(screen.getByText('Are you sure you want to delete this specialization?')).toBeInTheDocument();
        expect(screen.getByText('Yes, Delete')).toBeInTheDocument();
        expect(screen.getByText('Cancel')).toBeInTheDocument();
    });

    test('handles invalid specialization ID', async () => {
        useParams.mockReturnValue({ id: undefined });
        
        render(
            <MemoryRouter>
                <RemoveSpecialization />
            </MemoryRouter>
        );

        await waitFor(() => {
            expect(screen.getByText('Invalid specialization ID')).toBeInTheDocument();
        });
        
        jest.advanceTimersByTime(2000);
        expect(mockNavigate).toHaveBeenCalledWith('/specializations/search');
    });

    test('handles successful deletion', async () => {
        useParams.mockReturnValue({ id: '123' });
        specializationService.deleteSpecialization.mockResolvedValueOnce({ success: true });
        
        render(
            <MemoryRouter>
                <RemoveSpecialization />
            </MemoryRouter>
        );

        const deleteButton = screen.getByText('Yes, Delete');
        fireEvent.click(deleteButton);

        await waitFor(() => {
            expect(screen.getByText('Specialization successfully deleted.')).toBeInTheDocument();
        });
        
        jest.advanceTimersByTime(3000);
        expect(mockNavigate).toHaveBeenCalledWith('/specializations/search');
        expect(specializationService.deleteSpecialization).toHaveBeenCalledWith('123');
    });

    test('handles deletion error', async () => {
        useParams.mockReturnValue({ id: '123' });
        specializationService.deleteSpecialization.mockRejectedValueOnce(new Error('Delete failed'));
        
        const mockAlert = jest.spyOn(window, 'alert').mockImplementation(() => {});
        
        render(
            <MemoryRouter>
                <RemoveSpecialization />
            </MemoryRouter>
        );

        const deleteButton = screen.getByText('Yes, Delete');
        fireEvent.click(deleteButton);

        await waitFor(() => {
            expect(mockAlert).toHaveBeenCalledWith('Error deleting specialization.');
        });
    });

    test('navigates on cancel button click', () => {
        useParams.mockReturnValue({ id: '123' });
        
        render(
            <MemoryRouter>
                <RemoveSpecialization />
            </MemoryRouter>
        );

        const cancelButton = screen.getByText('Cancel');
        fireEvent.click(cancelButton);

        expect(mockNavigate).toHaveBeenCalledWith('/specializations/search');
    });
});