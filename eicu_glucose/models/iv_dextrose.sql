
with tmp as (

  select patientunitstayid,
        time,
        NULL as infusion_volume,
        category as dextrose_percent,
        drugname as product,
        1 as infusion_drug,
        0 as medication,
        0 as intakeoutput
  from {{ ref('stg_infusiondrug') }}
  where `group` = 'iv-sugar'
  
  union all
  
  select patientunitstayid,
        time,
        NULL as infusion_volume,
        category as dextrose_percent,
        drugname as product,
        0 as infusion_drug,
        1 as medication,
        0 as intakeoutput
  from {{ ref('stg_medication') }}
  where `group` = 'iv-sugar'
  
  union all
  
  select patientunitstayid,
        time,
        cellvaluenumeric as infusion_volume,
        category as dextrose_percent,
        celllabel as product,
        0 as infusion_drug,
        0 as medication,
        1 as intakeoutput
  from {{ ref('stg_intakeoutput') }}
  where `group` = 'iv-sugar'

)

select *
from tmp
order by patientunitstayid,
      time



