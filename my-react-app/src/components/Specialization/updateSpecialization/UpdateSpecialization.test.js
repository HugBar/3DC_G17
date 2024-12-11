import React from 'react';
import { render, screen, fireEvent, act, waitFor } from '@testing-library/react';
import { MemoryRouter, Routes, Route } from 'react-router-dom';
import UpdateSpecialization from './UpdateSpecialization';
import specializationService from '../../../api/specializationService';

// Mock the specializationService
jest.mock('../../../api/specializationService');

const mockSpecialization = {
    _id: '123',
    name: 'Cardiology',
    description: 'Heart specialist'
};

describe('UpdateSpecialization Component', () => {
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
    });

    const renderUpdateSpecialization = (name = 'Cardiology') => {
        return render(
            <MemoryRouter initialEntries={[`/specialization/update/${encodeURIComponent(name)}`]}>
                <Routes>
                    <Route path="/specialization/update/:id" element={<UpdateSpecialization />} />
                </Routes>
            </MemoryRouter>
        );
    };

    test('renders update form with specialization data', async () => {
        specializationService.searchSpecializations.mockResolvedValue([mockSpecialization]);
        
        renderUpdateSpecialization();

        await waitFor(() => {
            expect(screen.getByLabelText('Specialization Name:')).toHaveValue(mockSpecialization.name);
            expect(screen.getByLabelText('Description:')).toHaveValue(mockSpecialization.description);
        });
    });

    test('handles form submission successfully', async () => {
        specializationService.searchSpecializations.mockResolvedValue([mockSpecialization]);
        specializationService.updateSpecialization.mockResolvedValue(mockSpecialization);
        
        renderUpdateSpecialization();

        await waitFor(() => {
            expect(screen.getByLabelText('Specialization Name:')).toHaveValue(mockSpecialization.name);
        });

        // Update form fields
        await act(async () => {
            fireEvent.change(screen.getByLabelText('Specialization Name:'), {
                target: { value: 'Updated Cardiology' }
            });
            fireEvent.change(screen.getByLabelText('Description:'), {
                target: { value: 'Updated description' }
            });
        });

        // Submit form
        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Update Specialization' }));
        });

        expect(screen.getByText('Specialization updated successfully!')).toBeInTheDocument();
    });

    test('displays error message when specialization not found', async () => {
        specializationService.searchSpecializations.mockResolvedValue([]);
        
        renderUpdateSpecialization();

        await waitFor(() => {
            expect(screen.getByText('Specialization not found')).toBeInTheDocument();
        });
    });

    test('handles update error', async () => {
        specializationService.searchSpecializations.mockResolvedValue([mockSpecialization]);
        specializationService.updateSpecialization.mockRejectedValue(new Error('Update failed'));
        
        renderUpdateSpecialization();

        await waitFor(() => {
            expect(screen.getByLabelText('Specialization Name:')).toHaveValue(mockSpecialization.name);
        });

        // Submit form
        await act(async () => {
            fireEvent.click(screen.getByRole('button', { name: 'Update Specialization' }));
        });

        expect(screen.getByText('Error updating specialization')).toBeInTheDocument();
    });

    test('handles form field changes', async () => {
        specializationService.searchSpecializations.mockResolvedValue([mockSpecialization]);
        
        renderUpdateSpecialization();

        await waitFor(() => {
            expect(screen.getByLabelText('Specialization Name:')).toHaveValue(mockSpecialization.name);
        });

        // Update name field
        await act(async () => {
            fireEvent.change(screen.getByLabelText('Specialization Name:'), {
                target: { value: 'New Name' }
            });
        });

        expect(screen.getByLabelText('Specialization Name:')).toHaveValue('New Name');
    });
}); 