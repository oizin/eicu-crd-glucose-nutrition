
select distinct patientunitstayid,
  time,
  lower(treatmentstring) as drugname,
  (case when lower(treatmentstring) like '%longer%' then 'long'
      when lower(treatmentstring) like '%regular%' then 'regular' 
      else NULL
  end) as insulin_acting,
  NULL as dose,
  1 as source_treatment
from {{ ref('stg_treatment') }}
where lower(treatmentstring) like '%subcutaneous%'

