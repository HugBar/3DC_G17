// Author: João Morais

/**
 * This test suite validates the functionality of the SpecializationController.
 * It includes comprehensive tests for:
 * - Adding new specializations
 * - Retrieving all specializations
 * - Fetching specific specializations by ID
 * The tests use Jest's mocking capabilities to isolate the controller layer.
 */

const SpecializationController = require('./SpecializationController');
const SpecializationService = require('../services/SpecializationService');
const SpecializationDto = require('../dtos/SpecializationDto');
const SpecializationSearchDto = require('../dtos/SearchSpecializationDto')

// Mock the SpecializationService to isolate tests
jest.mock('../services/SpecializationService');

/**
 * Main test suite for SpecializationController
 * Contains all test cases organized by controller method
 */
describe('SpecializationController', () => {
    let mockReq;
    let mockRes;

    /**
     * Setup before each test
     * Creates fresh mock response object and clears previous mocks
     */
    beforeEach(() => {
        // Mock response object with common Express.js methods
        mockRes = {
            status: jest.fn().mockReturnThis(),
            json: jest.fn()
        };
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});
    });

    /**
     * Cleanup after each test
     * Restores all mocked implementations
     */
    afterEach(() => {
        jest.restoreAllMocks();
    });

    /**
     * Test suite for addSpecialization endpoint
     * Validates creation of new specializations and error handling
     */
    describe('addSpecialization', () => {
        beforeEach(() => {
            // Setup mock request with sample specialization data
            mockReq = {
                body: {
                    name: 'Cardiology',
                    description: 'Heart specialist'
                }
            };
        });

        /**
         * Tests successful creation of a new specialization
         * Verifies proper status code and response format
         */
        test('should create and return a new specialization', async () => {
            const mockSpecialization = { id: 1, ...mockReq.body };
            SpecializationService.addSpecialization.mockResolvedValue(mockSpecialization);

            await SpecializationController.addSpecialization(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Specialization added successfully',
                specialization: mockSpecialization
            });
            expect(SpecializationService.addSpecialization).toHaveBeenCalledWith(
                expect.any(SpecializationDto)
            );
        });

        /**
         * Tests handling of duplicate specialization attempts
         * Verifies proper conflict status code and error message
         */
        test('should return 409 when specialization already exists', async () => {
            SpecializationService.addSpecialization.mockRejectedValue(
                new Error('Specialization already exists')
            );

            await SpecializationController.addSpecialization(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(409);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Specialization already exists'
            });
        });

        /**
         * Tests handling of internal server errors
         * Verifies proper error status code and message
         */
        test('should return 500 on internal server error', async () => {
            SpecializationService.addSpecialization.mockRejectedValue(
                new Error('Database error')
            );

            await SpecializationController.addSpecialization(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    /**
     * Test suite for getAllSpecializations endpoint
     * Validates retrieval of all specializations and error handling
     */
    describe('getAllSpecializations', () => {
        /**
         * Tests successful retrieval of all specializations
         * Verifies proper response format and status code
         */
        test('should return all specializations', async () => {
            const mockSpecializations = [
                { id: 1, name: 'Cardiology', description: 'Heart specialist' },
                { id: 2, name: 'Neurology', description: 'Brain specialist' }
            ];
            SpecializationService.getAllSpecializations.mockResolvedValue(mockSpecializations);

            await SpecializationController.getAllSpecializations(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockSpecializations);
        });

        /**
         * Tests handling of internal server errors
         * Verifies proper error status code and message
         */
        test('should return 500 on internal server error', async () => {
            SpecializationService.getAllSpecializations.mockRejectedValue(
                new Error('Database error')
            );

            await SpecializationController.getAllSpecializations(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    /**
     * Test suite for getSpecializationById endpoint
     * Validates retrieval of specific specializations and error handling
     */
    describe('getSpecializationById', () => {
        beforeEach(() => {
            // Setup mock request with sample specialization ID
            mockReq = {
                params: { id: '1' }
            };
        });

        /**
         * Tests successful retrieval of specialization by ID
         * Verifies proper response format and status code
         */
        test('should return specialization by id', async () => {
            const mockSpecialization = { id: 1, name: 'Cardiology', description: 'Heart specialist' };
            SpecializationService.getSpecializationById.mockResolvedValue(mockSpecialization);

            await SpecializationController.getSpecializationById(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockSpecialization);
        });

        /**
         * Tests handling of non-existent specialization requests
         * Verifies proper not found status code and error message
         */
        test('should return 404 when specialization not found', async () => {
            SpecializationService.getSpecializationById.mockRejectedValue(
                new Error('Specialization not found')
            );

            await SpecializationController.getSpecializationById(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(404);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Specialization not found'
            });
        });

        /**
         * Tests handling of internal server errors
         * Verifies proper error status code and message
         */
        test('should return 500 on internal server error', async () => {
            SpecializationService.getSpecializationById.mockRejectedValue(
                new Error('Database error')
            );

            await SpecializationController.getSpecializationById(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    describe('searchSpecialization', () => {
        beforeEach(() => {
            mockReq = {
                query: {}
            };
        });
    
        test('should return specializations with name filter', async () => {
            const mockFilter = { name: 'Cardio' };
            mockReq.query = mockFilter;
            
            const mockSpecializations = [
                { _id: "1", name: 'Cardiology', description: 'Heart specialist' },
                { _id: "2", name: 'Cardio Surgery', description: 'Heart surgery' }
            ];
            
            SpecializationService.searchSpecializations.mockResolvedValue(mockSpecializations);
    
            await SpecializationController.searchSpecialization(mockReq, mockRes);
    
            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockSpecializations);
            expect(SpecializationService.searchSpecializations).toHaveBeenCalledWith(
                expect.any(SpecializationSearchDto)
            );
        });
    
        test('should return all specializations without filters', async () => {
            // Setup mock data
            const mockSpecializations = [
                { _id: "1", name: 'Cardiology', description: 'Heart specialist' },
                { _id: "2", name: 'Neurology', description: 'Brain specialist' }
            ];
    
            // Setup service mock with proper resolution
            SpecializationService.searchSpecializations = jest.fn().mockResolvedValue(mockSpecializations);
    
            // Call controller
            await SpecializationController.searchSpecialization(mockReq, mockRes);
    
            // Verify response
            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockSpecializations);
            expect(SpecializationService.searchSpecializations).toHaveBeenCalledWith(
                expect.any(SpecializationSearchDto)
            );
        });
    
        test('should handle search errors', async () => {
            SpecializationService.searchSpecializations.mockRejectedValue(
                new Error('Search failed')
            );
    
            await SpecializationController.searchSpecialization(mockReq, mockRes);
    
            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                error: 'Search failed'
            });
            expect(console.error).toHaveBeenCalledWith(
                'Error searching specializations:',
                expect.any(Error)
            );
        });
    });

    /**
     * Test suite for deleteSpecialization endpoint
     * Validates deletion of specializations and error handling
         */
    describe('deleteSpecialization', () => {
        let mockReq;
        let mockRes;
    
        beforeEach(() => {
            mockReq = {
                params: { id: '123' }
            };
            mockRes = {
                status: jest.fn().mockReturnThis(),
                json: jest.fn()
            };
        });
    
        /**
         * Tests successful deletion of a specialization
         * Verifies proper response format and status code
         */
        test('should successfully delete specialization', async () => {
            const mockSpecialization = {
                id: '123',
                name: 'Cardiology',
                description: 'Heart specialist'
            };
    
            SpecializationService.deleteSpecialization.mockResolvedValue(mockSpecialization);
    
            await SpecializationController.deleteSpecialization(mockReq, mockRes);
    
            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Specialization deleted successfully',
                specialization: mockSpecialization
            });
        });
    
        /**
         * Tests handling of non-existent specialization requests
         * Verifies proper not found status code and error message
         */
        test('should return 404 when specialization not found', async () => {
            SpecializationService.deleteSpecialization.mockRejectedValue(
                new Error('Specialization not found')
            );
    
            await SpecializationController.deleteSpecialization(mockReq, mockRes);
    
            expect(mockRes.status).toHaveBeenCalledWith(404);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Specialization not found'
            });
        });
    
        /**
         * Tests handling of internal server errors
         * Verifies proper error status code and message
         */
        test('should return 500 on internal server error', async () => {
            SpecializationService.deleteSpecialization.mockRejectedValue(
                new Error('Database error')
            );
    
            await SpecializationController.deleteSpecialization(mockReq, mockRes);
    
            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    describe('updateSpecialization', () => {
        beforeEach(() => {
            mockReq = {
                params: { id: '123' },
                body: {
                    name: 'Updated Cardiology',
                    description: 'Updated heart specialist'
                }
            };
        });

        /**
         * Tests successful update of a specialization
         * Verifies proper status code and response format
         */
        test('should update and return the modified specialization', async () => {
            const mockUpdatedSpecialization = { 
                id: mockReq.params.id,
                ...mockReq.body,
                updatedAt: new Date()
            };
            SpecializationService.updateSpecialization.mockResolvedValue(mockUpdatedSpecialization);

            await SpecializationController.updateSpecialization(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Specialization updated successfully',
                specialization: mockUpdatedSpecialization
            });
            expect(SpecializationService.updateSpecialization).toHaveBeenCalledWith(
                mockReq.params.id,
                expect.any(SpecializationDto)
            );
        });

        /**
         * Tests handling of non-existent specialization updates
         * Verifies proper not found status code and error message
         */
        test('should return 404 when specialization not found', async () => {
            SpecializationService.updateSpecialization.mockRejectedValue(
                new Error('Specialization not found')
            );

            await SpecializationController.updateSpecialization(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(404);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Specialization not found'
            });
        });

        /**
         * Tests handling of duplicate name conflicts during update
         * Verifies proper conflict status code and error message
         */
        test('should return 409 when updating to existing name', async () => {
            SpecializationService.updateSpecialization.mockRejectedValue(
                new Error('Specialization with this name already exists')
            );

            await SpecializationController.updateSpecialization(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(409);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Specialization with this name already exists'
            });
        });

        /**
         * Tests handling of unexpected errors
         * Verifies proper internal error status code and generic error message
         */
        test('should return 500 for unexpected errors', async () => {
            SpecializationService.updateSpecialization.mockRejectedValue(
                new Error('Unexpected error')
            );

            await SpecializationController.updateSpecialization(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

});