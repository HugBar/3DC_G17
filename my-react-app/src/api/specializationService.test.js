import axios from 'axios';
import specializationService from './specializationService';

// Mock axios to avoid actual API calls during tests
jest.mock('axios');

describe('SpecializationService', () => {
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
    const mockSpecializationId = '1';
    const mockSpecializationData = {
        id: mockSpecializationId,
        name: 'Cardiology',
        description: 'Heart specialist'
    };
    const mockApiResponse = {
        data: mockSpecializationData
    };

    // Tests for getAllSpecializations functionality
    describe('getAllSpecializations', () => {
        test('should successfully retrieve all specializations', async () => {
            axios.get.mockResolvedValueOnce({ data: [mockSpecializationData] });

            const result = await specializationService.getAllSpecializations();

            expect(axios.get).toHaveBeenCalledWith(`${process.env.REACT_APP_API_URL || 'http://localhost:3001/api'}/specializations`);
            expect(result).toEqual([mockSpecializationData]);
        });

        test('should handle API error', async () => {
            const errorMessage = 'Failed to fetch specializations';
            axios.get.mockRejectedValueOnce(new Error(errorMessage));

            await expect(specializationService.getAllSpecializations()).rejects.toThrow(errorMessage);
        });
    });

    // Tests for addSpecialization functionality
    describe('addSpecialization', () => {
        test('should successfully add a specialization', async () => {
            axios.post.mockResolvedValueOnce(mockApiResponse);

            const result = await specializationService.addSpecialization(mockSpecializationData);

            expect(axios.post).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001/api'}/specializations`,
                mockSpecializationData
            );
            expect(result).toEqual(mockApiResponse.data);
        });

        test('should handle API error', async () => {
            const errorMessage = 'Failed to add specialization';
            axios.post.mockRejectedValueOnce(new Error(errorMessage));

            await expect(specializationService.addSpecialization(mockSpecializationData)).rejects.toThrow(errorMessage);
        });
    });

    // Tests for searchSpecializations functionality
    describe('searchSpecializations', () => {
        const mockSearchParams = { name: 'Cardiology' };

        test('should successfully search specializations', async () => {
            axios.get.mockResolvedValueOnce({ data: [mockSpecializationData] });

            const result = await specializationService.searchSpecializations(mockSearchParams);

            expect(axios.get).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001/api'}/specializations/search`,
                { params: mockSearchParams }
            );
            expect(result).toEqual([mockSpecializationData]);
        });

        test('should handle API error', async () => {
            const errorMessage = 'Search failed';
            axios.get.mockRejectedValueOnce(new Error(errorMessage));

            await expect(specializationService.searchSpecializations(mockSearchParams)).rejects.toThrow(errorMessage);
        });
    });

    // Tests for deleteSpecialization functionality
    describe('deleteSpecialization', () => {
        test('should successfully delete a specialization', async () => {
            axios.delete.mockResolvedValueOnce(mockApiResponse);

            const result = await specializationService.deleteSpecialization(mockSpecializationId);

            expect(axios.delete).toHaveBeenCalledWith(
                `${process.env.REACT_APP_API_URL || 'http://localhost:3001/api'}/specializations/${mockSpecializationId}`,
                {
                    headers: {
                        'Authorization': `Bearer ${mockAuthToken}`,
                        'Content-Type': 'application/json'
                    }
                }
            );
            expect(result).toEqual(mockApiResponse.data);
        });

        test('should handle API error', async () => {
            const errorMessage = 'Failed to delete specialization';
            axios.delete.mockRejectedValueOnce(new Error(errorMessage));

            await expect(specializationService.deleteSpecialization(mockSpecializationId)).rejects.toThrow(errorMessage);
        });
    });
}); 