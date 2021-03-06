package org.ohdsi.webapi.evidence;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.math.BigDecimal;

/**
 *
 * @author rkboyce and m_rasteger
 */
@JsonInclude(Include.NON_NULL)
public class DrugHoiEvidence {
    @JsonProperty("EVIDENCE")
    public String evidence;
    
    @JsonProperty("MODALITY")
    public boolean modality;
    
    @JsonProperty("LINKOUT")
    public String linkout;
    
    @JsonProperty("STATISTIC_TYPE")
    public String statisticType;
 
    @JsonProperty("COUNT")
    public Integer count;	

    @JsonProperty("VALUE")
    public BigDecimal value;
}
