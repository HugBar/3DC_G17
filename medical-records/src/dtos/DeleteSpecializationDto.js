// Author: Matias Vitorino

/**
 * Data Transfer Object for deleting medical specializations
 * Used to transfer deletion requests between layers of the application
 * Contains only the ID of the specialization to be deleted
 */
class DeleteSpecializationDto {
    /**
     * Creates a new DeleteSpecializationDto
     * @param {string} id - The unique identifier of the specialization to delete
     */
    constructor(id) {
        this.id = id;
    }
}

module.exports = DeleteSpecializationDto;