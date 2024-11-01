using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

public class OperationTypeFilterDto
{
    public string NameFilter { get; set; }
    public string SpecializationFilter { get; set; }
    public bool? IsActiveFilter { get; set; }
    public int PageNumber { get; set; } = 1;
    public int PageSize { get; set; } = 5;

    public OperationTypeFilterDto(){
        
    }
}