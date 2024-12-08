// Author: Joao Morais

/**
 * Test suite for MedicalRecordController
 * Tests the API endpoints for managing medical records
 */

const MedicalRecordController = require('./medicalRecordController');
const MedicalRecordService = require('../services/MedicalRecordService');
const MedicalRecordRepository = require('../repositories/MedicalRecordRepository');
const UpdateMedicalRecordDto = require('../dtos/UpdateMedicalRecordDto');

// Mock dependencies
jest.mock('../services/MedicalRecordService');
jest.mock('../repositories/MedicalRecordRepository');

describe('MedicalRecordController', () => {
    let mockReq;
    let mockRes;

    beforeEach(() => {
        mockRes = {
            status: jest.fn().mockReturnThis(),
            json: jest.fn()
        };
        jest.clearAllMocks();
        jest.spyOn(console, 'error').mockImplementation(() => {});
    });

    afterEach(() => {
        jest.restoreAllMocks();
    });

    describe('createMedicalRecord', () => {
        beforeEach(() => {
            mockReq = {
                body: {
                    patientId: '123',
                    conditions: ['Hypertension'],
                    allergies: ['Peanuts']
                }
            };
        });

        test('should create and return a new medical record', async () => {
            const mockRecord = {
                _id: '1',
                patientId: '123',
                conditions: ['Hypertension'],
                allergies: ['Peanuts'],
                createdAt: new Date(),
                updatedAt: new Date()
            };
            MedicalRecordRepository.create = jest.fn().mockResolvedValue(mockRecord);

            await MedicalRecordController.createMedicalRecord(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith(mockRecord);
            expect(MedicalRecordRepository.create).toHaveBeenCalledWith(mockReq.body);
        });
    });

    describe('getAllMedicalRecords', () => {
        test('should return all medical records', async () => {
            const mockRecords = [
                {
                    _id: '1',
                    patientId: '123',
                    conditions: ['Hypertension'],
                    allergies: [],
                    createdAt: new Date(),
                    updatedAt: new Date()
                },
                {
                    _id: '2',
                    patientId: '456',
                    conditions: ['Diabetes'],
                    allergies: [],
                    createdAt: new Date(),
                    updatedAt: new Date()
                }
            ];
            MedicalRecordRepository.findAll = jest.fn().mockResolvedValue(mockRecords);

            await MedicalRecordController.getAllMedicalRecords(mockReq, mockRes);

            expect(mockRes.json).toHaveBeenCalledWith(mockRecords);
        });

        test('should handle database errors', async () => {
            MedicalRecordRepository.findAll = jest.fn().mockRejectedValue(new Error('Database error'));

            await MedicalRecordController.getAllMedicalRecords(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                error: 'Database error'
            });
        });
    });

    describe('getMedicalRecordByPatientId', () => {
        beforeEach(() => {
            mockReq = {
                params: { patientId: '123' }
            };
        });

        test('should return medical record for valid patient ID', async () => {
            const mockRecord = {
                _id: '1',
                patientId: '123',
                conditions: ['Hypertension'],
                allergies: [],
                createdAt: new Date(),
                updatedAt: new Date()
            };
            MedicalRecordRepository.findByPatientId = jest.fn().mockResolvedValue(mockRecord);

            await MedicalRecordController.getMedicalRecordByPatientId(mockReq, mockRes);

            expect(mockRes.json).toHaveBeenCalledWith(mockRecord);
        });

        test('should handle database errors', async () => {
            MedicalRecordRepository.findByPatientId = jest.fn().mockRejectedValue(new Error('Database error'));

            await MedicalRecordController.getMedicalRecordByPatientId(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                error: 'Database error'
            });
        });
    });

    describe('updatePatientConditionsAndAllergies', () => {
        beforeEach(() => {
            mockReq = {
                params: { patientId: '123' },
                body: {
                    conditions: ['Hypertension', 'Diabetes'],
                    allergies: ['Peanuts', 'Shellfish']
                }
            };
        });

        test('should update existing medical record', async () => {
            const mockUpdatedRecord = { 
                _id: '1', 
                patientId: '123',
                conditions: mockReq.body.conditions,
                allergies: mockReq.body.allergies,
                createdAt: new Date(),
                updatedAt: new Date()
            };
            
            MedicalRecordRepository.findByPatientId = jest.fn().mockResolvedValue({ _id: '1', patientId: '123' });
            MedicalRecordService.updatePatientConditionsAndAllergies = jest.fn().mockResolvedValue(mockUpdatedRecord);

            await MedicalRecordController.updatePatientConditionsAndAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(200);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical record updated successfully',
                record: mockUpdatedRecord
            });
        });

        test('should create new medical record when none exists', async () => {
            const mockNewRecord = { 
                _id: '1', 
                patientId: '123',
                conditions: mockReq.body.conditions,
                allergies: mockReq.body.allergies,
                createdAt: new Date(),
                updatedAt: new Date()
            };
            
            MedicalRecordRepository.findByPatientId = jest.fn().mockResolvedValue(null);
            MedicalRecordService.updatePatientConditionsAndAllergies = jest.fn().mockResolvedValue(mockNewRecord);

            await MedicalRecordController.updatePatientConditionsAndAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(201);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Medical record created successfully',
                record: mockNewRecord
            });
        });

        test('should handle service errors', async () => {
            MedicalRecordService.updatePatientConditionsAndAllergies = jest.fn()
                .mockRejectedValue(new Error('Service error'));

            await MedicalRecordController.updatePatientConditionsAndAllergies(mockReq, mockRes);

            expect(mockRes.status).toHaveBeenCalledWith(500);
            expect(mockRes.json).toHaveBeenCalledWith({
                message: 'Internal server error'
            });
            expect(console.error).toHaveBeenCalledWith('Error updating medical record:', expect.any(Error));
        });
    });
}); 