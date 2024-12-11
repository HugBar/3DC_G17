// Author: Matias Vitorino

/**
 * This test suite covers the process of removing a specialization from the system.
 * It tests the following scenarios:
 * - Displaying the delete confirmation modal
 * - Successfully deleting a specialization
 * - Canceling the deletion process
 * - Handling invalid specialization ID
 * - Handling deletion error
 * - Navigating back after deletion
 */

import React from 'react';
import { render, screen, fireEvent, act } from '@testing-library/react';
import { useNavigate, useParams } from 'react-router-dom';
import RemoveSpecialization from './RemoveSpecialization';
import specializationService from '../../../api/specializationService';

// Mock dependencies
jest.mock('react-router-dom', () => ({
    useNavigate: jest.fn(),
    useParams: jest.fn()
}));

jest.mock('../../../api/specializationService', () => ({
    deleteSpecialization: jest.fn()
}));

describe('RemoveSpecialization Component', () => {
    const mockNavigate = jest.fn();
    const validId = '123';
    
    beforeAll(() => {
        jest.spyOn(console, 'error').mockImplementation(() => {});
        jest.spyOn(console, 'log').mockImplementation(() => {});
    });

    afterAll(() => {
        console.error.mockRestore();
        console.log.mockRestore();
    });

    beforeEach(() => {
        jest.clearAllMocks();
        useNavigate.mockReturnValue(mockNavigate);
        useParams.mockReturnValue({ id: validId });
    });

    test('renders confirmation dialog', () => {
        render(<RemoveSpecialization />);
        
        expect(screen.getByText('Delete Specialization')).toBeInTheDocument();
        expect(screen.getByText('Are you sure you want to delete this specialization?')).toBeInTheDocument();
        expect(screen.getByRole('button', { name: 'Yes, Delete' })).toBeInTheDocument();
        expect(screen.getByRole('button', { name: 'Cancel' })).toBeInTheDocument();
    });

    test('handles successful deletion', async () => {
        specializationService.deleteSpecialization.mockResolvedValue({});
        render(<RemoveSpecialization />);

        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Yes, Delete' }));
        });

        expect(specializationService.deleteSpecialization).toHaveBeenCalledWith(validId);
        expect(screen.getByText('Specialization successfully deleted.')).toBeInTheDocument();
        
        // Wait for navigation timeout
        await act(async () => {
            await new Promise(resolve => setTimeout(resolve, 3000));
        });
        
        expect(mockNavigate).toHaveBeenCalledWith('/specializations/search');
    });

    test('handles deletion error', async () => {
        const mockAlert = jest.spyOn(window, 'alert').mockImplementation(() => {});
        specializationService.deleteSpecialization.mockRejectedValue(new Error('Failed to delete'));
        
        render(<RemoveSpecialization />);

        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Yes, Delete' }));
        });

        expect(specializationService.deleteSpecialization).toHaveBeenCalledWith(validId);
        expect(mockAlert).toHaveBeenCalledWith('Error deleting specialization.');
        
        mockAlert.mockRestore();
    });

    test('navigates back on cancel', () => {
        render(<RemoveSpecialization />);
        
        fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
        
        expect(mockNavigate).toHaveBeenCalledWith('/specializations/search');
    });

    test('handles invalid specialization ID', async () => {
        useParams.mockReturnValue({ id: undefined });
        
        render(<RemoveSpecialization />);

        expect(screen.getByText('Invalid specialization ID')).toBeInTheDocument();
        
        await act(async () => {
            await new Promise(resolve => setTimeout(resolve, 2000));
        });
        
        expect(mockNavigate).toHaveBeenCalledWith('/specializations/search');
    });
});