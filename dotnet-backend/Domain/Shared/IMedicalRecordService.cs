using System.Threading.Tasks;

public interface IMedicalRecordService
{
    Task CreateBlankMedicalRecord(string patientId);
}