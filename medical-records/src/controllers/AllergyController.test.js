// Author: Hugo Barros

/**
 * Test suite for the AllergyController module.
 * Contains unit tests for allergy management functionality including
 * adding new allergies and searching existing allergies.
 * Tests verify proper handling of successful operations and error cases.
 */

const AllergyController = require('./AllergyController');
const AllergyService = require('../services/AllergyService');
const AllergyDto = require('../dtos/AllergyDto');
const AllergySearchDto = require('../dtos/AllergySearchDto');
const CreateAllergyDto = require('../dtos/CreatAllergyDto');

jest.mock('../services/AllergyService');

describe('AllergyController', () => {
    let mockReq;
    let mockRes;

    // Set up mock response object before each test
    beforeEach(() => {
        mockRes = {
            status: jest.fn().mockReturnThis(),
            json: jest.fn()
        };
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});

    });

    // Clean up mocks after each test
    afterEach(() => {
        jest.restoreAllMocks();
    });

    /**
     * Tests for addAllergyModel method
     * Verifies creation of new allergy records and error handling
     */
    describe('addAllergyModel', () => {
        beforeEach(() => {
            mockReq = {
                body: {
                    allergen: 'Peanuts',
                    severity: 'High',
                    diagnosedDate: '2024-03-19',
                    notes: 'Severe reaction'
                }
            };
        });

        // Test successful allergy creation
        test('should create and return a new allergy model', async () => {
            const mockAllergyModel = { id: 1, ...mockReq.body };
            AllergyService.addAllergyModel.mockResolvedValue(mockAllergyModel);

            await AllergyController.addAllergyModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Allergy added successfully',
                allergy: mockAllergyModel
            });
            expect(AllergyService.addAllergyModel).toHaveBeenCalledWith(
                expect.any(CreateAllergyDto)
            );
        });

        // Test duplicate allergy handling
        test('should return 409 when allergy already exists', async () => {
            AllergyService.addAllergyModel.mockRejectedValue(new Error('Allergy already exists'));

            await AllergyController.addAllergyModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(409);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Allergy already exists'
            });
        });

        // Test internal server error handling
        test('should return 500 on internal server error', async () => {
            AllergyService.addAllergyModel.mockRejectedValue(new Error('Database error'));

            await AllergyController.addAllergyModel(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
        });
    });

    /**
     * Tests for searchAllergies method
     * Verifies allergy search functionality with and without filters
     */
    describe('searchAllergies', () => {
        // Test search with filters
        test('should return allergies with filters', async () => {
            const mockFilters = { allergen: 'Peanuts', severity: 'High' };
            mockReq = { query: mockFilters };
            const mockAllergies = [{ id: 1, ...mockFilters }];
            
            AllergyService.searchAllergies.mockResolvedValue(mockAllergies);

            await AllergyController.searchAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockAllergies);
            expect(AllergyService.searchAllergies).toHaveBeenCalledWith(mockFilters);
        });

        // Test search without filters
        test('should return all allergies without filters', async () => {
            mockReq = { query: {} };
            const mockAllergies = [{ id: 1, allergen: 'Peanuts' }];
            
            AllergyService.searchAllergies.mockResolvedValue(mockAllergies);

            await AllergyController.searchAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith(mockAllergies);
            expect(AllergyService.searchAllergies).toHaveBeenCalledWith({});
        });

        // Test error handling during search
        test('should handle search errors', async () => {
            mockReq = { query: {} };
            AllergyService.searchAllergies.mockRejectedValue(new Error('Search failed'));

            await AllergyController.searchAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                error: 'Search failed'
            });
        });
    });
});