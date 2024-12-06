/**
 * Data Transfer Object (DTO) for deleting a specialization
 * @param {string} id - The ID of the specialization to delete
 */
class DeleteSpecializationDto {
    constructor(id) {
        this.id = id;
    }
}

module.exports = DeleteSpecializationDto;
    