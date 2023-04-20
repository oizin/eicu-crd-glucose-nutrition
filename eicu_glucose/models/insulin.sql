
with tmp as (
  select patientunitstayid,
      time,
      drugname,
      coalesce(drugrate,cast(infusionrate as string)) as insulin_rate,
      NULL as insulin_total_ml,
      NULL as insulin_dose,
      'iv' as route,
      'continuous' as frequency_english,
      NULL as times_per_day,
      NULL as insulin_type,
      NULL as insulin_acting,
      NULL as dose,
      NULL as dose_low,
      NULL as dose_high,
      patientweight,
      1 as infusion_drug,
      0 as medication,
      0 as intakeoutput
  from {{ ref('stg_infusiondrug') }}
  where category = 'insulin'
  
  union all
  
  select patientunitstayid,
      time,
      drugname,
      NULL as insulin_rate,
      NULL as insulin_total_ml,
      dosage as insulin_dose,
      route,
      frequency_english,
      times_per_day,
      insulin_type,
      insulin_acting,
      dose,
      dose_low,
      dose_high,
      NULL as patientweight,
      0 as infusion_drug,
      1 as medication,
      0 as intakeoutput
  from {{ ref('stg_medication') }}
  where category = 'insulin'
  
  union all 
  
  select patientunitstayid,
    time,
    NULL as drugname,
    NULL as insulin_rate,
    cellvaluenumeric as insulin_total_ml,
    NULL as insulin_dose,
    'iv' as route,
    'continuous' as frequency_english,
    NULL as times_per_day,
    NULL as insulin_type,
    NULL as insulin_acting,
    NULL as dose,
    NULL as dose_low,
    NULL as dose_high,
    NULL as patientweight,
    0 as infusion_drug,
    0 as medication,
    1 as intakeoutput
  from {{ ref('stg_intakeoutput') }}
  where category = 'insulin'

)

select patientunitstayid,
      time,
      max(drugname) as drugname,
      max(insulin_rate) as insulin_rate,
      max(insulin_total_ml) as insulin_total_ml,
      max(insulin_dose) as insulin_dose,
      max(route) as route,
      max(frequency_english) as frequency_english,
      max(times_per_day) as times_per_day,
      max(insulin_type) as insulin_type,
      max(dose) as dose,
      max(dose_low) as dose_low,
      max(dose_high) as dose_high,
      max(patientweight) as patientweight,
      max(infusion_drug) as source_infusion_drug,
      max(medication) as source_medication,
      max(intakeoutput) as source_intakeoutput
from tmp
group by patientunitstayid,
      time
order by patientunitstayid,
      time
