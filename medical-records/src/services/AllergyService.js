const MedicalRecord = require('../models/MedicalRecord');
const Allergy = require('../models/Allergy');
const AllergyDto = require('../dtos/AllergyDto');
const AlergyRepository = require('../repositories/AllergyRepositorie');


class AllergyService {
    async addAllergy(allergyDto) {
        try {
            const medicalRecord = await MedicalRecord.findOne({ patientId });
            console.log(medicalRecord);
            
            if (!medicalRecord) {
                throw new Error('Medical record not found');
            }

            const newAllergy = {
                allergen: allergyDto.allergen,
                severity: allergyDto.severity,
                description: allergyDto.desription
            };

            const allergy = await AlergyRepository.addAllergy(newAllergy);        
            
            return allergy;
        } catch (error) {
            throw error;
        }
    }

    async addAllergyModel(allergyDto) {
        try {
            const allergy = await Allergy.findOne({ allergen: allergyDto.allergen });
            if (allergy) {
                throw new Error('Allergy already exists');
            }

            const newAllergy = new Allergy({
                allergen: allergyDto.allergen,
                severity: allergyDto.severity,
                diagnosedDate: allergyDto.diagnosedDate,
                notes: allergyDto.notes
            });

            const addedAllergy = await AlergyRepository.addAllergyModel(newAllergy);

            return addedAllergy;

        } catch (error) {
            throw error;
        }
    }

    // Get all allergies from allergy model
    async searchAllergies(allergySearchDto) {
        try {
           

            const allergies = await AlergyRepository.findByFilters(allergySearchDto);
            return allergies.map(allergy => new AllergyDto(
            allergy.allergen,
            allergy.severity,
            allergy.description
        ));
        } catch (error) {
            throw error;
        }
    }

    async getAllAllergies() {
        try {
            const allergies = await AlergyRepository.getAllAllergiesWithDetails();
            if (!allergies || allergies.length === 0) {
                return [];
            }
            return allergies;
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new AllergyService();
