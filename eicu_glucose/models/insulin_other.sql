
select distinct patientunitstayid,
  time,
  lower(treatmentstring) as drugname,
  'treatment' as eicu_source
from {{ ref('stg_treatment') }}
where lower(treatmentstring) not like '%subcutaneous%' and 
      lower(treatmentstring) not like '%continuous%'

