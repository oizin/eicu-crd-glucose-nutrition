
with lipid_times as(
  select patientunitstayid,time
  from {{ ref('stg_intakeoutput') }} 
  where category = 'nutrition-fat'
),
tmp_pivot as (
  select patientunitstayid,
        time,
        NULL as parenteral_total,
        NULL as parenteral_rate,
        cellvaluenumeric as parenteral_fat,
        celllabel as product,
        1 as source_intakeoutput,
        0 as source_medication,
        0 as source_infusiondrug,
        0 as source_treatment
  from {{ ref('stg_intakeoutput') }}
  where `group` not in ('nutrition') and 
    lower(celllabel) like '%iv%' and 
    time in (select time from lipid_times)
    
  union all
  
  select patientunitstayid,
        time,
        (case 
          when category in ('parenteral-intake','parenteral-insulin') 
            then cellvaluenumeric
        end) as parenteral_total,
        NULL as parenteral_rate,
        (case 
          when category = 'parenteral-fat' then cellvaluenumeric
        end) as parenteral_fat,
        celllabel as product,
        1 as source_intakeoutput,
        0 as source_medication,
        0 as source_infusiondrug,
        0 as source_treatment
    from {{ ref('stg_intakeoutput') }}
    where `group` = 'nutrition-parenteral'

  union all
  
  select patientunitstayid,
      time,
      NULL as parenteral_total,
      (case 
        when category in ('parenteral','parenteral-insulin') 
          then infusionrate
      end) as parenteral_rate,
      (case 
        when category = 'parenteral-fat' then infusionrate
      end) as parenteral_fat,
      drugname as product,
      0 as source_intakeoutput,
      0 as source_medication,
      1 as source_infusiondrug,
      0 as source_treatment
  from {{ ref('stg_infusiondrug') }}
  where `group` = 'nutrition-parenteral'
  
  union all
  
  select patientunitstayid,
    time,
    NULL as parenteral_total,
    NULL as parenteral_rate,
    NULL as parenteral_fat,
    dosage as product,
    0 as source_intakeoutput,
    1 as source_medication,
    0 as source_infusiondrug,
    0 as source_treatment
  from {{ ref('stg_medication') }}
  where `group` = 'nutrition-parenteral'
  
  union all
  
  select patientunitstayid,
    time,
    NULL as parenteral_total,
    NULL as parenteral_rate,
    NULL as parenteral_fat,
    treatmentstring as product,
    0 as source_intakeoutput,
    0 as source_medication,
    0 as source_infusiondrug,
    1 as source_treatment
  from {{ ref('stg_treatment') }}
  where `group` = 'nutrition-parenteral'

)

select patientunitstayid,
    time,
    max(parenteral_total) as parenteral_total,
    max(parenteral_rate) as parenteral_rate,
    max(parenteral_fat) as parenteral_fat,
    max(product) as product,
    max(source_intakeoutput) as source_intakeoutput,
    max(source_medication) as source_medication,
    max(source_infusiondrug) as source_infusiondrug,
    max(source_treatment) as source_treatment
from tmp_pivot
group by patientunitstayid,
    time
order by patientunitstayid,
    time


  