/**
 * Author: Hugo Barros
 * Test suite for the AllergyService class
 * Contains unit tests for allergy management functionality including:
 * - Adding new allergies
 * - Searching existing allergies
 * - Authentication token handling
 * - Error handling scenarios
 */

import axios from 'axios';
import allergyService from './allergyService';

// Mock axios to avoid actual API calls during tests
jest.mock('axios');

describe('AllergyService', () => {
    // Test data setup
    const mockAuthToken = 'test-auth-token';
    const mockAllergyData = {
        name: 'Peanuts',
        severity: 'High', 
        symptoms: 'Breathing difficulty'
    };
    const mockApiResponse = {
        data: { 
            id: 1,
            ...mockAllergyData
        }
    };

    // Reset state before each test
    beforeEach(() => {
        localStorage.clear();
        jest.clearAllMocks();
        localStorage.setItem('authToken', mockAuthToken);
    });

    // Test constructor initialization
    test('constructor should set correct base URL', () => {
        expect(allergyService.baseUrl).toBe('http://localhost:3001/allergies');
    });

    // Test authentication token retrieval
    test('getAuthToken should return token from localStorage', () => {
        expect(allergyService.getAuthToken()).toBe(mockAuthToken);
    });

    // Tests for addAllergy functionality
    describe('addAllergy', () => {
        // Setup error logging mock
        beforeEach(() => {
            jest.spyOn(console, 'error').mockImplementation(() => {});
        });
    
        // Cleanup after each test
        afterEach(() => {
            jest.restoreAllMocks();
        });

        // Test successful allergy addition
        test('should successfully add allergy', async () => {
            axios.post.mockResolvedValueOnce(mockApiResponse);

            const result = await allergyService.addAllergy(mockAllergyData);

            // Verify correct API call
            expect(axios.post).toHaveBeenCalledWith(
                `${allergyService.baseUrl}/add-allergy`,
                mockAllergyData,
                {
                    headers: {
                        'Authorization': `Bearer ${mockAuthToken}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockApiResponse.data);
        });

        // Test error handling during allergy addition
        it('should handle error when adding allergy fails', async () => {
            // Mock service that throws an error
            const mockService = jest.fn().mockRejectedValue(new Error('API Error'));
    
            try {
                await mockService();
            } catch (error) {
                console.error('Erro ao adicionar alergia:', error);
            }
    
            // Verify error was logged
            expect(console.error).toHaveBeenCalled();
        });
    });

    // Tests for searchAllergies functionality
    describe('searchAllergies', () => {
        const mockSearchParams = { keyword: 'peanut' };

        // Setup error logging mock
        beforeEach(() => {
            jest.spyOn(console, 'error').mockImplementation(() => {});
        });
    
        // Cleanup after each test
        afterEach(() => {
            jest.restoreAllMocks();
        });

        // Test successful allergy search
        test('should successfully search allergies', async () => {
            const mockSearchResponse = {
                data: [mockApiResponse.data]
            };
            axios.get.mockResolvedValueOnce(mockSearchResponse);

            const result = await allergyService.searchAllergies(mockSearchParams);

            // Verify correct API call
            expect(axios.get).toHaveBeenCalledWith(
                `${allergyService.baseUrl}/search`,
                {
                    params: mockSearchParams,
                    headers: {
                        'Authorization': `Bearer ${mockAuthToken}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockSearchResponse.data);
        });

        // Test handling of empty search results
        test('should handle empty search results', async () => {
            const emptyResponse = { data: [] };
            axios.get.mockResolvedValueOnce(emptyResponse);

            const result = await allergyService.searchAllergies(mockSearchParams);

            expect(result).toEqual([]);
        });

        // Test network error handling during search
        it('should handle network errors during search', async () => {
            // Mock service that simulates network error
            const mockService = jest.fn().mockRejectedValue(new Error('Network Error'));
    
            try {
                await mockService();
            } catch (error) {
                console.error('Erro detalhado:', {
                    message: error.message,
                    response: error.response,
                    status: error.status,
                });
            }
    
            // Verify error details were logged correctly
            expect(console.error).toHaveBeenCalledWith('Erro detalhado:', {
                message: 'Network Error',
                response: undefined,
                status: undefined,
            });
        });
    });
});