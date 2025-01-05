/**
 * Author: João Morais
 * Test suite for the MedicalRecordService
 * Contains unit tests for medical record management functionality including:
 * - Retrieving medical records
 * - Updating medical records
 * - Searching medical records
 * - Authentication token handling
 * - Error handling scenarios
 */

import axios from 'axios';
import medicalRecordService from './medicalRecordService';

// Mock axios to avoid actual API calls during tests
jest.mock('axios');

describe('MedicalRecordService', () => {
    // Mock localStorage for testing
    let localStorageMock;

    // Reset state and mocks before each test
    beforeEach(() => {
        // Setup localStorage mock implementation
        localStorageMock = {
            getItem: jest.fn(() => 'test-auth-token'),
            setItem: jest.fn(),
            clear: jest.fn()
        };
        // Override window.localStorage with mock
        Object.defineProperty(window, 'localStorage', {
            value: localStorageMock,
            writable: true
        });
        jest.clearAllMocks();
    });

    // Test data setup
    const mockAuthToken = 'test-auth-token';
    const mockPatientId = '12345';
    const mockMedicalRecord = {
        patientId: mockPatientId,
        conditions: ['Asthma'],
        allergies: ['Peanuts']
    };
    const mockApiResponse = {
        data: mockMedicalRecord
    };

    // Tests for getMedicalRecord functionality
    describe('getMedicalRecord', () => {
        test('should successfully retrieve medical record', async () => {
            axios.get.mockResolvedValueOnce(mockApiResponse);

            const result = await medicalRecordService.getMedicalRecord(mockPatientId);

            expect(axios.get).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001'}/medical-records/${mockPatientId}`,
                {
                    headers: {
                        'Authorization': `Bearer ${mockAuthToken}`
                    }
                }
            );
            expect(result).toEqual(mockApiResponse.data);
        });

        test('should handle API error', async () => {
            const errorMessage = 'Failed to fetch medical record';
            axios.get.mockRejectedValueOnce(new Error(errorMessage));

            await expect(medicalRecordService.getMedicalRecord(mockPatientId))
                .rejects.toThrow(errorMessage);
        });
    });

    // Tests for updateMedicalRecord functionality
    describe('updateMedicalRecord', () => {
        const mockUpdateData = {
            conditions: ['Asthma', 'Diabetes'],
            allergies: ['Peanuts', 'Shellfish']
        };

        test('should successfully update medical record', async () => {
            const mockUpdateResponse = {
                data: { ...mockMedicalRecord, ...mockUpdateData }
            };
            axios.put.mockResolvedValueOnce(mockUpdateResponse);

            const result = await medicalRecordService.updateMedicalRecord(mockPatientId, mockUpdateData);

            expect(axios.put).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001'}/medical-records/update/${mockPatientId}`,
                mockUpdateData,
                {
                    headers: {
                        'Authorization': `Bearer ${mockAuthToken}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockUpdateResponse.data);
        });
    });

    // Tests for searchMedicalRecord functionality
    describe('searchMedicalRecord', () => {
        test('should search with only patientId', async () => {
            axios.get.mockResolvedValueOnce(mockApiResponse);

            await medicalRecordService.searchMedicalRecord(mockPatientId);

            expect(axios.get).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001'}/medical-records/search?patientId=${mockPatientId}`,
                expect.any(Object)
            );
        });

        test('should search with all filters', async () => {
            const condition = 'Asthma';
            const allergy = 'Peanuts';
            axios.get.mockResolvedValueOnce(mockApiResponse);

            await medicalRecordService.searchMedicalRecord(mockPatientId, condition, allergy);

            expect(axios.get).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001'}/medical-records/search?patientId=${mockPatientId}&conditionName=${condition}&allergyName=${allergy}`,
                expect.any(Object)
            );
        });
    });

    // Tests for verifyPatient functionality
    describe('verifyPatient', () => {
        test('should return true for existing patient', async () => {
            axios.get.mockResolvedValueOnce({ status: 200 });

            const result = await medicalRecordService.verifyPatient(mockPatientId);

            expect(result).toBe(true);
        });

        test('should return false for non-existing patient', async () => {
            axios.get.mockRejectedValueOnce(new Error('Patient not found'));

            const result = await medicalRecordService.verifyPatient(mockPatientId);

            expect(result).toBe(false);
        });
    });

    // Tests for getAllMedicalConditions functionality
    describe('getAllMedicalConditions', () => {
        test('should successfully retrieve all conditions', async () => {
            const mockConditions = {
                data: ['Asthma', 'Diabetes', 'Hypertension']
            };
            axios.get.mockResolvedValueOnce(mockConditions);

            const result = await medicalRecordService.getAllMedicalConditions();

            expect(axios.get).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001'}/medical-conditions/getConditionDetails`,
                expect.any(Object)
            );
            expect(result).toEqual(mockConditions.data);
        });
    });

    // Tests for getAllAllergies functionality
    describe('getAllAllergies', () => {
        test('should successfully retrieve all allergies', async () => {
            const mockAllergies = {
                data: ['Peanuts', 'Shellfish', 'Penicillin']
            };
            axios.get.mockResolvedValueOnce(mockAllergies);

            const result = await medicalRecordService.getAllAllergies();

            expect(axios.get).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001'}/allergies/getAllergyDetails`,
                expect.any(Object)
            );
            expect(result).toEqual(mockAllergies.data);
        });

        test('should handle and log error when fetching allergies fails', async () => {
            const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
            const error = new Error('Failed to fetch allergies');
            axios.get.mockRejectedValueOnce(error);

            await expect(medicalRecordService.getAllAllergies()).rejects.toThrow(error);
            expect(consoleSpy).toHaveBeenCalled();

            consoleSpy.mockRestore();
        });
    });
}); 