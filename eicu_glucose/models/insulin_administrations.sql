

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
  where lower(treatmentstring) like '%continuous%'and 
  category = 'insulin'

),
iv as (
  select patientunitstayid,
    time,
    max(drugname) as drugname,
    cast(NULL AS STRING) as insulin_acting,
    max(insulin_rate) as insulin_rate,
    max(insulin_total_ml) as insulin_total_ml,
    NULL as insulin_dose,
    max(patientweight) as patientweight,
    max(source_infusion_drug) as source_infusion_drug,
    max(source_intakeoutput) as source_intakeoutput,
    max(source_treatment) as source_treatment,
    'iv' as route
  from tmp
  group by patientunitstayid,
        time
  order by patientunitstayid,
        time
),
sc as (
  select distinct 
    patientunitstayid,
    time,
    lower(treatmentstring) as drugname,
    (case when lower(treatmentstring) like '%longer%' then 'long'
        when lower(treatmentstring) like '%regular%' then 'regular' 
        else NULL
    end) as insulin_acting,
    cast(NULL AS STRING) as insulin_rate,
    NULL as insulin_total_ml,
    NULL as insulin_dose,
    NULL as patientweight,
    0 as source_infusion_drug,
    0 as source_intakeoutput,
    1 as source_treatment,
    'sc' as route
  from {{ ref('stg_treatment') }}
  where lower(treatmentstring) like '%subcutaneous%' and 
  category = 'insulin'
),
other as (
  select distinct 
    patientunitstayid,
    time,
    lower(treatmentstring) as drugname,
    cast(NULL AS STRING) as insulin_acting,
    cast(NULL AS STRING) as insulin_rate,
    NULL as insulin_total_ml,
    NULL as insulin_dose,
    NULL as patientweight,
    0 as source_infusion_drug,
    0 as source_intakeoutput,
    1 as source_treatment,
    cast(NULL AS STRING) as route
  from {{ ref('stg_treatment') }}
  where lower(treatmentstring) not like '%subcutaneous%' and 
        lower(treatmentstring) not like '%continuous%' and 
        category = 'insulin'
)

select *
from iv

union distinct

select *
from sc

union distinct

select *
from other
