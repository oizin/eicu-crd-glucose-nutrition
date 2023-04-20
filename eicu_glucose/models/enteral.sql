
with tmp_union as (
  select patientunitstayid,
      category,
      celllabel as product,
      cellvaluenumeric as enteral_total,
      NULL as enteral_rate,
      time,
      1 as source_intakeoutput,
      0 as source_infusiondrug,
      0 as source_treatment
  from {{ ref('stg_intakeoutput') }}
  where `group` = 'nutrition-enteral'
  
  union all
  
  select patientunitstayid,
      category,
      drugname as product,
      NULL as enteral_total,
      infusionrate as enteral_rate,
      time,
      0 as source_intakeoutput,
      1 as source_infusiondrug,
      0 as source_treatment
  from {{ ref('stg_infusiondrug') }}
  where `group` = 'nutrition-enteral'
  
  union all
  
  select patientunitstayid,
      category,
      treatmentstring as product,
      NULL as enteral_total,
      NULL as enteral_rate,
      time,
      0 as source_intakeoutput,
      0 as source_infusiondrug,
      1 as source_treatment
  from {{ ref('stg_treatment') }}
  where `group` = 'nutrition-enteral'

),
tmp_pivot as (
  select 
    patientunitstayid,
    time,
    (case 
    when category in ('enteral-nonspecific-intake','enteral-product-intake') 
        then product else NULL 
    end) as product,
    (case 
    when category in ('enteral-nonspecific-intake','enteral-product-intake') and 
        source_intakeoutput = 1
        then enteral_total else NULL 
    end) as enteral_total,
    (case 
    when category in ('enteral-nonspecific-intake','enteral-product-intake') and
        source_infusiondrug = 1
        then enteral_rate else NULL 
    end) as enteral_rate,
    (case 
    when category in ('enteral-water-meds') then 1 else NULL 
    end) as enteral_water_meds,
    source_intakeoutput,
    source_infusiondrug,
    source_treatment
  from tmp_union
  order by patientunitstayid,time
)
select patientunitstayid,
    time,
    max(product) as product,
    max(enteral_total) as enteral_total,
    max(enteral_rate) as enteral_rate,
    max(enteral_water_meds) as enteral_water_meds,
    max(source_intakeoutput) as source_intakeoutput,
    max(source_infusiondrug) as source_infusiondrug,
    max(source_treatment) as source_treatment
from tmp_pivot
group by patientunitstayid,
    time
order by patientunitstayid,
    time


