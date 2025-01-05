/**
 * Author: Matias Vitorino
 * Test suite for the MedicalConditionService class
 * Contains unit tests for medical condition management functionality including:
 * - Adding new medical conditions
 * - Searching existing medical conditions 
 * - Authentication token handling
 * - Error handling scenarios
 */

import axios from 'axios';
import medicalConditionService from './medicalConditionService';

// Mock axios to avoid actual API calls during tests
jest.mock('axios');

describe('MedicalConditionService', () => {
    // Mock localStorage for testing
    let localStorageMock;

    // Reset state and mocks before each test
    beforeEach(() => {
        // Setup localStorage mock implementation
        localStorageMock = {
            getItem: jest.fn(() => 'fake-token'),
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

    // Test constructor initialization
    describe('constructor', () => {
        it('should set correct base URL', () => {
            expect(medicalConditionService.baseUrl).toBe('http://localhost:3001/medical-conditions');
        });
    });

    // Test authentication token retrieval
    describe('getAuthToken', () => {
        it('should get token from localStorage', () => {
            const token = medicalConditionService.getAuthToken();
            expect(localStorageMock.getItem).toHaveBeenCalledWith('authToken');
            expect(token).toBe('fake-token');
        });
    });

    // Tests for addMedicalCondition functionality
    describe('addMedicalCondition', () => {
        it('should successfully add medical condition', async () => {
            // Setup test data
            const mockData = { name: 'Test Condition' };
            const mockResponse = { data: { id: 1, ...mockData } };
            
            // Mock successful API response
            axios.post.mockResolvedValue(mockResponse);

            const result = await medicalConditionService.addMedicalCondition(mockData);

            // Verify correct API call
            expect(axios.post).toHaveBeenCalledWith(
                'http://localhost:3001/medical-conditions/add-medical-condition',
                mockData,
                {
                    headers: {
                        'Authorization': 'Bearer fake-token',
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockResponse.data);
        });
        
        // Test error handling during medical condition addition
        it('should handle error when adding medical condition', async () => {
            const mockError = new Error('Network error');
            axios.post.mockRejectedValue(mockError);

            await expect(
                medicalConditionService.addMedicalCondition({})
            ).rejects.toThrow('Network error');
        });
    });

    // Tests for searchMedicalConditions functionality 
    describe('searchMedicalConditions', () => {
        it('should successfully search medical conditions', async () => {
            // Setup test data
            const mockParams = { query: 'test' };
            const mockResponse = { 
                data: [
                    { id: 1, name: 'Test Condition 1' },
                    { id: 2, name: 'Test Condition 2' }
                ]
            };

            // Mock successful API response
            axios.get.mockResolvedValue(mockResponse);

            const result = await medicalConditionService.searchMedicalConditions(mockParams);

            // Verify correct API call
            expect(axios.get).toHaveBeenCalledWith(
                'http://localhost:3001/medical-conditions/search',
                {
                    params: mockParams,
                    headers: {
                        'Authorization': 'Bearer fake-token',
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockResponse.data);
        });

        // Test error handling during medical condition search
        it('should handle error when searching medical conditions', async () => {
            const mockError = new Error('Search failed');
            axios.get.mockRejectedValue(mockError);

            await expect(
                medicalConditionService.searchMedicalConditions({})
            ).rejects.toThrow('Search failed');
        });
    });
});
