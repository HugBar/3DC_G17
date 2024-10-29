using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.OperationTypeData
{
    public class OperationTypeId : EntityId
    {   
        [JsonConstructor]
        public OperationTypeId(Guid value) : base(value)
        {

        }
       
        public OperationTypeId(String value) : base(value)
        {
        }

        public override string AsString()
        {
            Guid guid = (Guid) base.ObjValue;

            return guid.ToString();
        }

        protected override object createFromString(string text)
        {
            return new Guid(text);
        }
        public Guid asGuid()
        {
            return (Guid) base.ObjValue;
        }
        
    }
}
