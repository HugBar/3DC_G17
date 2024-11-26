using System;

namespace DDDSample1.Domain.OperationTypeData
{
    public class OperationPhases
    {
        public TimeSpan AnesthesiaPreparation { get; private set; }
        public TimeSpan Surgery { get; private set; }
        public TimeSpan Cleaning { get; private set; }

        public OperationPhases(TimeSpan anesthesiaPreparation, TimeSpan surgery, TimeSpan cleaning)
        {
            AnesthesiaPreparation = anesthesiaPreparation;
            Surgery = surgery;
            Cleaning = cleaning;
        }

    }
}