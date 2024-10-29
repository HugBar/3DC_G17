using System;

public class MedicalRecordNumber
{
    private static int _sequence = 1;
    private static string _currentYearMonth = DateTime.Now.ToString("yyyyMM");

    public static string Generate()
    {
        string yearMonth = DateTime.Now.ToString("yyyyMM");

        if (_currentYearMonth != yearMonth)
        {
            _currentYearMonth = yearMonth;
            _sequence = 1;
        }

        string sequenceStr = _sequence.ToString("D6"); // Pad with zeros to ensure 6 digits
        _sequence++;

        return $"{yearMonth}{sequenceStr}";
    }
}