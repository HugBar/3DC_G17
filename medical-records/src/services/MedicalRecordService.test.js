// Author: Joao Morais

/**
 * Test suite for MedicalRecordService
 * Tests the business logic for managing medical records including:
 * - Updating existing records
 * - Creating new records
 * - Error handling
 */

const MedicalRecordService = require('./MedicalRecordService');
const MedicalRecordRepository = require('../repositories/MedicalRecordRepository');
const UpdateMedicalRecordDto = require('../dtos/UpdateMedicalRecordDto');

// Mock the repository
jest.mock('../repositories/MedicalRecordRepository');

describe('MedicalRecordService', () => {
    beforeEach(() => {
        jest.clearAllMocks();
    });

    describe('updatePatientConditionsAndAllergies', () => {
        const mockPatientId = '123';
        const mockUpdateDto = {
            conditions: [
                { name: 'Hypertension', severity: 'High' },
                { name: 'Diabetes', severity: 'Moderate' }
            ],
            allergies: [
                { name: 'Peanuts', severity: 'Severe' },
                { name: 'Shellfish', severity: 'Moderate' }
            ]
        };

        /**
         * Tests updating an existing medical record
         * Verifies:
         * - Existing record is found and updated
         * - Update contains correct conditions and allergies
         * - Repository methods are called with correct parameters
         */
        test('should update existing medical record', async () => {
            // Mock existing record
            const mockExistingRecord = {
                _id: '1',
                patientId: mockPatientId,
                conditions: [],
                allergies: []
            };

            // Mock updated record
            const mockUpdatedRecord = {
                _id: '1',
                patientId: mockPatientId,
                conditions: mockUpdateDto.conditions,
                allergies: mockUpdateDto.allergies,
                lastUpdated: expect.any(Date)
            };

            MedicalRecordRepository.findByPatientId.mockResolvedValue(mockExistingRecord);
            MedicalRecordRepository.update.mockResolvedValue(mockUpdatedRecord);

            const result = await MedicalRecordService.updatePatientConditionsAndAllergies(
                mockPatientId,
                mockUpdateDto
            );

            expect(result).toEqual(mockUpdatedRecord);
            expect(MedicalRecordRepository.findByPatientId).toHaveBeenCalledWith(mockPatientId);
            expect(MedicalRecordRepository.update).toHaveBeenCalledWith(
                mockPatientId,
                expect.objectContaining({
                    conditions: mockUpdateDto.conditions,
                    allergies: mockUpdateDto.allergies,
                    lastUpdated: expect.any(Date)
                })
            );
        });

        /**
         * Tests error handling when repository operations fail
         * Verifies:
         * - Database errors are properly propagated
         * - Error message is preserved
         */
        test('should throw error when medical record not found', async () => {
            MedicalRecordRepository.findByPatientId.mockResolvedValue(null);

            await expect(
                MedicalRecordService.updatePatientConditionsAndAllergies(mockPatientId, mockUpdateDto)
            ).rejects.toThrow('Medical record not found');
        });

        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            MedicalRecordRepository.findByPatientId.mockRejectedValue(mockError);

            await expect(
                MedicalRecordService.updatePatientConditionsAndAllergies(mockPatientId, mockUpdateDto)
            ).rejects.toThrow('Database error');
        });
    });

    describe('searchMedicalRecord', () => {
        const mockPatientId = 'TEST123';
        const mockSearchDto = {
            patientId: mockPatientId,
            conditionName: 'Asthma',
            allergyName: 'Peanuts',
            validate: jest.fn()
        };

        test('should filter medical record by condition and allergy', async () => {
            const mockRecord = {
                _id: '1',
                patientId: mockPatientId,
                conditions: [
                    { name: 'Asthma', severity: 'High' },
                    { name: 'Diabetes', severity: 'Medium' }
                ],
                allergies: [
                    { name: 'Peanuts', severity: 'High' },
                    { name: 'Shellfish', severity: 'Low' }
                ],
                lastUpdated: new Date(),
                createdAt: new Date(),
                updatedAt: new Date()
            };

            MedicalRecordRepository.findByPatientId.mockResolvedValue(mockRecord);

            const result = await MedicalRecordService.searchMedicalRecord(mockSearchDto);

            expect(result.conditions).toHaveLength(1);
            expect(result.conditions[0].name).toBe('Asthma');
            expect(result.allergies).toHaveLength(1);
            expect(result.allergies[0].name).toBe('Peanuts');
        });

        test('should return null when record not found', async () => {
            MedicalRecordRepository.findByPatientId.mockResolvedValue(null);

            const result = await MedicalRecordService.searchMedicalRecord(mockSearchDto);

            expect(result).toBeNull();
        });

        test('should handle validation errors', async () => {
            mockSearchDto.validate.mockImplementation(() => {
                throw new Error('Validation error');
            });

            await expect(MedicalRecordService.searchMedicalRecord(mockSearchDto))
                .rejects.toThrow('Error searching medical record: Validation error');
        });

        test('should return only conditions when searching by condition name', async () => {
            const mockRecord = {
                _id: '1',
                patientId: mockPatientId,
                conditions: [
                    { name: 'Asthma', severity: 'High' },
                    { name: 'Diabetes', severity: 'Medium' }
                ],
                allergies: [
                    { name: 'Peanuts', severity: 'High' }
                ]
            };

            MedicalRecordRepository.findByPatientId.mockResolvedValue(mockRecord);
            
            const result = await MedicalRecordService.searchMedicalRecord({
                patientId: mockPatientId,
                conditionName: 'Asthma',
                allergyName: null,
                validate: jest.fn()
            });

            expect(result.conditions).toBeDefined();
            expect(result.conditions).toHaveLength(1);
            expect(result.conditions[0].name).toBe('Asthma');
            expect(result.allergies).toEqual([]);
        });
    });
});