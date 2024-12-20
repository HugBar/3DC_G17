// Author: João Morais

/**
 * This test suite validates the functionality of the SpecializationService.
 * It includes comprehensive tests for:
 * - Adding new specializations
 * - Retrieving all specializations 
 * - Fetching specific specializations by ID
 * The tests use Jest's mocking capabilities to isolate the service layer.
 */

const SpecializationService = require('./SpecializationService');
const Specialization = require('../models/Specialization');
const SpecializationDto = require('../dtos/SpecializationDto');
const SpecializationRepository = require('../repositories/SpecializationRepository');

// Mock dependencies to isolate tests
jest.mock('../models/Specialization');
jest.mock('../repositories/SpecializationRepository');

/**
 * Main test suite for SpecializationService
 * Contains all test cases organized by service method
 */
describe('SpecializationService', () => {
    beforeEach(() => {
        jest.clearAllMocks();
    });

    /**
     * Test suite for addSpecialization method
     * Validates creation of new specializations and error handling
     */
    describe('addSpecialization', () => {
        const mockSpecializationDto = {
            name: 'Cardiology',
            description: 'Heart specialist'
        };

        /**
         * Tests successful creation of a new specialization
         * Verifies proper repository calls and response format
         */
        test('should add new specialization when it does not exist', async () => {
            const mockAddedSpecialization = { id: '123', ...mockSpecializationDto };
            
            SpecializationRepository.findByName.mockResolvedValue(null);
            SpecializationRepository.create.mockResolvedValue(mockAddedSpecialization);

            const result = await SpecializationService.addSpecialization(mockSpecializationDto);

            expect(SpecializationRepository.findByName).toHaveBeenCalledWith(mockSpecializationDto.name);
            expect(SpecializationRepository.create).toHaveBeenCalledWith({
                name: mockSpecializationDto.name,
                description: mockSpecializationDto.description
            });
            expect(result).toEqual(mockAddedSpecialization);
        });

        /**
         * Tests handling of duplicate specialization creation attempts
         * Verifies proper error is thrown
         */
        test('should throw error when specialization already exists', async () => {
            SpecializationRepository.findByName.mockResolvedValue({ 
                id: '123', 
                name: 'Cardiology' 
            });

            await expect(SpecializationService.addSpecialization(mockSpecializationDto))
                .rejects
                .toThrow('Specialization already exists');
        });

        /**
         * Tests handling of repository errors
         * Verifies errors are properly propagated
         */
        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            SpecializationRepository.findByName.mockRejectedValue(mockError);

            await expect(SpecializationService.addSpecialization(mockSpecializationDto))
                .rejects
                .toThrow('Database error');
        });
    });

    /**
     * Test suite for getAllSpecializations method
     * Validates retrieval of all specializations and error handling
     */
    describe('getAllSpecializations', () => {
        /**
         * Tests successful retrieval of all specializations
         * Verifies proper repository calls and response format
         */
        test('should return all specializations', async () => {
            const mockSpecializations = [
                { id: '1', name: 'Cardiology', description: 'Heart specialist' },
                { id: '2', name: 'Neurology', description: 'Brain specialist' }
            ];

            SpecializationRepository.findAll.mockResolvedValue(mockSpecializations);

            const result = await SpecializationService.getAllSpecializations();

            expect(SpecializationRepository.findAll).toHaveBeenCalled();
            expect(result).toEqual(mockSpecializations);
        });

        /**
         * Tests handling of empty result sets
         * Verifies empty array is returned
         */
        test('should handle empty results', async () => {
            SpecializationRepository.findAll.mockResolvedValue([]);

            const result = await SpecializationService.getAllSpecializations();

            expect(result).toEqual([]);
        });

        /**
         * Tests handling of repository errors
         * Verifies errors are properly propagated
         */
        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            SpecializationRepository.findAll.mockRejectedValue(mockError);

            await expect(SpecializationService.getAllSpecializations())
                .rejects
                .toThrow('Database error');
        });
    });

    /**
     * Test suite for getSpecializationById method
     * Validates retrieval of specific specializations and error handling
     */
    describe('getSpecializationById', () => {
        const mockId = '123';

        /**
         * Tests successful retrieval of specialization by ID
         * Verifies proper repository calls and response format
         */
        test('should return specialization when found', async () => {
            const mockSpecialization = { 
                id: mockId, 
                name: 'Cardiology', 
                description: 'Heart specialist' 
            };

            SpecializationRepository.findById.mockResolvedValue(mockSpecialization);

            const result = await SpecializationService.getSpecializationById(mockId);

            expect(SpecializationRepository.findById).toHaveBeenCalledWith(mockId);
            expect(result).toEqual(mockSpecialization);
        });

        /**
         * Tests handling of non-existent specialization requests
         * Verifies proper error is thrown
         */
        test('should throw error when specialization not found', async () => {
            SpecializationRepository.findById.mockResolvedValue(null);

            await expect(SpecializationService.getSpecializationById(mockId))
                .rejects
                .toThrow('Specialization not found');
        });

        /**
         * Tests handling of repository errors
         * Verifies errors are properly propagated
         */
        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            SpecializationRepository.findById.mockRejectedValue(mockError);

            await expect(SpecializationService.getSpecializationById(mockId))
                .rejects
                .toThrow('Database error');
        });
    });
    
    describe('searchSpecializations', () => {
        beforeEach(() => {
            jest.clearAllMocks();
        });
    
        test('should return mapped specializations when found', async () => {
            const mockSearchDto = {
                name: 'Cardio',
                description: 'Heart'
            };
    
            const mockSpecializations = [
                { 
                    _id: '1', 
                    name: 'Cardiology', 
                    description: 'Heart specialist' 
                },
                { 
                    _id: '2', 
                    name: 'Cardio Surgery', 
                    description: 'Heart surgery' 
                }
            ];
    
            SpecializationRepository.searchSpecialization.mockResolvedValue(mockSpecializations);
    
            const result = await SpecializationService.searchSpecializations(mockSearchDto);
    
            expect(SpecializationRepository.searchSpecialization).toHaveBeenCalledWith(mockSearchDto);
            expect(result).toHaveLength(mockSpecializations.length);
            // Remove SpecializationDto instance check since we're returning plain objects
            expect(result[0]).toEqual({
                _id: mockSpecializations[0]._id,
                name: mockSpecializations[0].name,
                description: mockSpecializations[0].description
            });
        });
    
        test('should handle empty search results', async () => {
            const mockSearchDto = {
                name: 'NonExistent'
            };
    
            SpecializationRepository.searchSpecialization.mockResolvedValue([]);
    
            const result = await SpecializationService.searchSpecializations(mockSearchDto);
    
            expect(result).toEqual([]);
            expect(SpecializationRepository.searchSpecialization).toHaveBeenCalledWith(mockSearchDto);
        });
    
        test('should propagate repository errors', async () => {
            const mockSearchDto = {
                name: 'Test'
            };
            const mockError = new Error('Database error');
            
            SpecializationRepository.searchSpecialization.mockRejectedValue(mockError);
    
            await expect(SpecializationService.searchSpecializations(mockSearchDto))
                .rejects
                .toThrow('Database error');
        });
    });

    /**
     * Test suite for deleteSpecialization method
     * Validates deletion of specializations and error handling
     */
    describe('deleteSpecialization', () => {
        const mockId = '123';
    
        /**
         * Tests successful deletion of a specialization
         * Verifies proper repository calls and response format
         */
        test('should delete specialization when found', async () => {
            const mockSpecialization = { 
                id: mockId, 
                name: 'Cardiology', 
                description: 'Heart specialist' 
            };
    
            SpecializationRepository.findById.mockResolvedValue(mockSpecialization);
            SpecializationRepository.delete.mockResolvedValue(mockSpecialization);
    
            const result = await SpecializationService.deleteSpecialization(mockId);
    
            expect(SpecializationRepository.findById).toHaveBeenCalledWith(mockId);
            expect(SpecializationRepository.delete).toHaveBeenCalledWith(mockId);
            expect(result).toEqual(mockSpecialization);
        });
    
        /**
         * Tests handling of non-existent specialization requests
         * Verifies proper error is thrown
         */
        test('should throw error when specialization not found', async () => {
            SpecializationRepository.findById.mockResolvedValue(null);
    
            await expect(SpecializationService.deleteSpecialization(mockId))
                .rejects
                .toThrow('Specialization not found');
            
            expect(SpecializationRepository.delete).not.toHaveBeenCalled();
        });

        /**
         * Tests handling of repository errors
         * Verifies errors are properly propagated
         */ 
        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            SpecializationRepository.findById.mockRejectedValue(mockError);
    
            await expect(SpecializationService.deleteSpecialization(mockId))
                .rejects
                .toThrow('Database error');
        });
    });

    describe('updateSpecialization', () => {
        const mockId = '123';
        const mockSpecializationDto = {
            name: 'Updated Cardiology',
            description: 'Updated heart specialist'
        };

        beforeEach(() => {
            jest.clearAllMocks();
        });

        /**
         * Tests successful update of a specialization with valid data
         * Verifies:
         * - Proper repository method calls
         * - Correct data passed to update
         * - Expected response format
         */
        test('should successfully update specialization when valid data provided', async () => {
            const existingSpecialization = {
                _id: mockId,
                name: 'Cardiology',
                description: 'Heart specialist'
            };

            const updatedSpecialization = {
                _id: mockId,
                ...mockSpecializationDto,
                updatedAt: new Date()
            };

            SpecializationRepository.findById.mockResolvedValue(existingSpecialization);
            SpecializationRepository.findByName.mockResolvedValue(null);
            SpecializationRepository.update.mockResolvedValue(updatedSpecialization);

            const result = await SpecializationService.updateSpecialization(mockId, mockSpecializationDto);

            expect(result).toEqual(updatedSpecialization);
            expect(SpecializationRepository.findById).toHaveBeenCalledWith(mockId);
            expect(SpecializationRepository.update).toHaveBeenCalledWith(
                mockId,
                expect.objectContaining({
                    name: mockSpecializationDto.name,
                    description: mockSpecializationDto.description,
                    updatedAt: expect.any(Date)
                })
            );
        });

        /**
         * Tests handling of non-existent specialization updates
         * Verifies:
         * - Error is thrown for non-existent specialization
         * - Update repository method is not called
         */
        test('should throw error when specialization not found', async () => {
            SpecializationRepository.findById.mockResolvedValue(null);

            await expect(SpecializationService.updateSpecialization(mockId, mockSpecializationDto))
                .rejects
                .toThrow('Specialization not found');

            expect(SpecializationRepository.update).not.toHaveBeenCalled();
        });

        /**
         * Tests duplicate name validation during update
         * Verifies:
         * - Error is thrown when updating to an existing name
         * - Update repository method is not called
         * - Proper error message is returned
         */
        test('should throw error when updating to existing name', async () => {
            const existingSpecialization = {
                _id: mockId,
                name: 'Cardiology',
                description: 'Heart specialist'
            };

            const duplicateSpecialization = {
                _id: '456',
                name: mockSpecializationDto.name,
                description: 'Another description'
            };

            SpecializationRepository.findById.mockResolvedValue(existingSpecialization);
            SpecializationRepository.findByName.mockResolvedValue(duplicateSpecialization);

            await expect(SpecializationService.updateSpecialization(mockId, mockSpecializationDto))
                .rejects
                .toThrow('Specialization with this name already exists');

            expect(SpecializationRepository.update).not.toHaveBeenCalled();
        });

        /**
         * Tests optimization when name is unchanged during update
         * Verifies:
         * - Duplicate name check is skipped
         * - Update proceeds without additional validation
         * - Update repository method is called
         */
        test('should not check for duplicate name if name unchanged', async () => {
            const existingSpecialization = {
                _id: mockId,
                name: mockSpecializationDto.name,
                description: 'Old description'
            };

            const updatedSpecialization = {
                _id: mockId,
                ...mockSpecializationDto,
                updatedAt: new Date()
            };

            SpecializationRepository.findById.mockResolvedValue(existingSpecialization);
            SpecializationRepository.update.mockResolvedValue(updatedSpecialization);

            await SpecializationService.updateSpecialization(mockId, mockSpecializationDto);

            expect(SpecializationRepository.findByName).not.toHaveBeenCalled();
            expect(SpecializationRepository.update).toHaveBeenCalled();
        });

        /**
         * Tests handling of repository errors during update
         * Verifies:
         * - Database errors are properly propagated
         * - Error message is preserved
         */
        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            SpecializationRepository.findById.mockRejectedValue(mockError);

            await expect(SpecializationService.updateSpecialization(mockId, mockSpecializationDto))
                .rejects
                .toThrow('Database error');
        });
    });
});