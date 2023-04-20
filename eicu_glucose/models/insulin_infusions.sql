
with tmp as (
  select patientunitstayid,
      time,
      drugname,
      coalesce(drugrate,cast(infusionrate as string)) as insulin_rate,
      NULL as insulin_total_ml,
      patientweight,
      1 as source_infusion_drug,
      0 as source_intakeoutput,
      0 as source_treatment
  from {{ ref('stg_infusiondrug') }}
  where category = 'insulin'
  
  union all
  
  select patientunitstayid,
    time,
    lower(celllabel) as drugname,
    NULL as insulin_rate,
    cellvaluenumeric as insulin_total_ml,
    NULL as patientweight,
    0 as source_infusion_drug,
    1 as source_intakeoutput,
    0 as source_treatment
  from {{ ref('stg_intakeoutput') }}
  where category = 'insulin'
  
  union all
  
  select patientunitstayid,
    time,
    lower(treatmentstring) as drugname,
    NULL as insulin_rate,
    NULL as insulin_total_ml,
    NULL as patientweight,
    0 as source_infusion_drug,
    0 as source_intakeoutput,
    1 as source_treatment
  from {{ ref('stg_treatment') }}
  where lower(treatmentstring) like '%continuous%'

)

select patientunitstayid,
      time,
      max(drugname) as drugname,
      max(insulin_rate) as insulin_rate,
      max(insulin_total_ml) as insulin_total_ml,
      max(patientweight) as patientweight,
      max(source_infusion_drug) as source_infusion_drug,
      max(source_intakeoutput) as source_intakeoutput,
      max(source_treatment) as source_treatment
from tmp
group by patientunitstayid,
      time
order by patientunitstayid,
      time
