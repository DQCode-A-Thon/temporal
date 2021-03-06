--persons in cohort from cohort start time to cohort end time, in 30d increments
select cast(hr1.stratum_1 as int) as month_year, 
  hr1.count_value as count_value, 
round(1.0*hr1.count_value / denom.count_value,5) as percent_value
from (select * from heracles_results where analysis_id = 1804 and cohort_definition_id in (@cohortDefinitionId)) hr1,
(select count_value from @ohdsi_database_schema.heracles_results where analysis_id = 1 and cohort_definition_id in (@cohortDefinitionId)) denom
order by hr1.stratum_1 asc