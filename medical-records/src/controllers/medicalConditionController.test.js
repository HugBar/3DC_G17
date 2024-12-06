// Author: João Morais

/**
 * Test suite for MedicalConditionController
 * Tests the API endpoints for medical condition operations including:
 * - Adding new conditions to patient records
 * - Adding conditions to the system catalog
 * - Searching for conditions
 * - Error handling for various scenarios
 */

const MedicalConditionController = require('./medicalConditionController');
const MedicalConditionService = require('../services/MedicalConditionService');
const MedicalConditionDto = require('../dtos/MedicalConditionDto');
const SearchMedicalConditionDto = require('../dtos/SearchMedicalConditionDto');

// Mock the service layer
jest.mock('../services/MedicalConditionService');

describe('MedicalConditionController', () => {
    let mockReq;
    let mockRes;

    /**
     * Common setup for all tests
     * Creates mock request and response objects
     */
    beforeEach(() => {
        mockRes = {
            status: jest.fn().mockReturnThis(),
            json: jest.fn()
        };
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});
    });

    /**
     * Cleanup after tests
     * Restores all mocked functions
     */
    afterEach(() => {
        jest.restoreAllMocks();
    });

    describe('addMedicalCondition', () => {
        /**
         * Setup specific to addMedicalCondition tests
         * Creates mock request with patient ID and condition details
         */
        beforeEach(() => {
            mockReq = {
                params: { patientId: '123' },
                body: {
                    name: 'Diabetes',
                    severity: 'High',
                    description: 'Chronic condition'
                }
            };
        });

        /**
         * Tests successful creation of a medical condition
         * Verifies response status and body
         */
        test('should create and return a new medical condition', async () => {
            const mockMedicalCondition = { id: 1, ...mockReq.body };
            MedicalConditionService.addMedicalCondition.mockResolvedValue(mockMedicalCondition);

            await MedicalConditionController.addMedicalCondition(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical condition added successfully',
                medicalCondition: mockMedicalCondition
            });
            expect(MedicalConditionService.addMedicalCondition).toHaveBeenCalledWith(
                expect.any(MedicalConditionDto)
            );
        });

        /**
         * Tests handling of non-existent medical record
         * Verifies 404 response
         */
        test('should return 404 when medical record not found', async () => {
            MedicalConditionService.addMedicalCondition.mockRejectedValue(
                new Error('Medical record not found')
            );

            await MedicalConditionController.addMedicalCondition(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(404);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical record not found'
            });
        });

        /**
         * Tests internal server error handling
         * Verifies 500 response
         */
        test('should return 500 on internal server error', async () => {
            MedicalConditionService.addMedicalCondition.mockRejectedValue(
                new Error('Database error')
            );

            await MedicalConditionController.addMedicalCondition(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    describe('addMedicalConditionModel', () => {
        /**
         * Setup specific to addMedicalConditionModel tests
         * Creates mock request with condition details
         */
        beforeEach(() => {
            mockReq = {
                body: {
                    name: 'Hypertension',
                    severity: 'Moderate',
                    description: 'High blood pressure'
                }
            };
        });

        /**
         * Tests successful creation of a medical condition model
         * Verifies response status and body
         */
        test('should create and return a new medical condition model', async () => {
            const mockMedicalConditionModel = { id: 1, ...mockReq.body };
            MedicalConditionService.addMedicalConditionModel.mockResolvedValue(mockMedicalConditionModel);

            await MedicalConditionController.addMedicalConditionModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical condition added successfully',
                medicalCondition: mockMedicalConditionModel
            });
            expect(MedicalConditionService.addMedicalConditionModel).toHaveBeenCalledWith(
                expect.any(MedicalConditionDto)
            );
        });

        /**
         * Tests handling of duplicate medical condition
         * Verifies 409 conflict response
         */
        test('should return 409 when medical condition already exists', async () => {
            MedicalConditionService.addMedicalConditionModel.mockRejectedValue(
                new Error('Medical condition already exists')
            );

            await MedicalConditionController.addMedicalConditionModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(409);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical condition already exists'
            });
        });

        /**
         * Tests internal server error handling
         * Verifies 500 response
         */
        test('should return 500 on internal server error', async () => {
            MedicalConditionService.addMedicalConditionModel.mockRejectedValue(
                new Error('Database error')
            );

            await MedicalConditionController.addMedicalConditionModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    describe('searchMedicalConditions', () => {
        /**
         * Tests successful search with filters
         * Verifies response status and body
         */
        test('should return medical conditions with filters', async () => {
            const mockFilters = { name: 'Diabetes', severity: 'High' };
            mockReq = { query: mockFilters };
            const mockConditions = [{ id: 1, ...mockFilters }];

            MedicalConditionService.searchMedicalConditions.mockResolvedValue(mockConditions);

            await MedicalConditionController.searchMedicalConditions(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockConditions);
            expect(MedicalConditionService.searchMedicalConditions).toHaveBeenCalledWith(
                expect.any(SearchMedicalConditionDto)
            );
        });

        /**
         * Tests search without filters
         * Verifies all conditions are returned
         */
        test('should return all medical conditions without filters', async () => {
            mockReq = { query: {} };
            const mockConditions = [{ id: 1, name: 'Diabetes' }];

            MedicalConditionService.searchMedicalConditions.mockResolvedValue(mockConditions);

            await MedicalConditionController.searchMedicalConditions(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockConditions);
            expect(MedicalConditionService.searchMedicalConditions).toHaveBeenCalledWith(
                expect.any(SearchMedicalConditionDto)
            );
        });

        /**
         * Tests error handling in search
         * Verifies 500 response
         */
        test('should handle search errors', async () => {
            mockReq = { query: {} };
            MedicalConditionService.searchMedicalConditions.mockRejectedValue(
                new Error('Search failed')
            );

            await MedicalConditionController.searchMedicalConditions(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                error: 'Search failed'
            });
        });
    });
});