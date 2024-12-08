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
         * Tests creating a new medical record when none exists
         * Verifies:
         * - New record is created with empty conditions/allergies
         * - Record is then updated with provided data
         * - Repository methods are called in correct order
         */
        test('should create new medical record when none exists', async () => {
            // Mock new record
            const mockNewRecord = {
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

            MedicalRecordRepository.findByPatientId.mockResolvedValue(null);
            MedicalRecordRepository.create.mockResolvedValue(mockNewRecord);
            MedicalRecordRepository.update.mockResolvedValue(mockUpdatedRecord);

            const result = await MedicalRecordService.updatePatientConditionsAndAllergies(
                mockPatientId,
                mockUpdateDto
            );

            expect(result).toEqual(mockUpdatedRecord);
            expect(MedicalRecordRepository.findByPatientId).toHaveBeenCalledWith(mockPatientId);
            expect(MedicalRecordRepository.create).toHaveBeenCalledWith({
                patientId: mockPatientId,
                conditions: [],
                allergies: []
            });
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
        test('should propagate repository errors', async () => {
            const mockError = new Error('Database error');
            MedicalRecordRepository.findByPatientId.mockRejectedValue(mockError);

            await expect(
                MedicalRecordService.updatePatientConditionsAndAllergies(mockPatientId, mockUpdateDto)
            ).rejects.toThrow('Database error');
        });
    });
});