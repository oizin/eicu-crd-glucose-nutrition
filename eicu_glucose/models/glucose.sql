
with lab as (
  select patientunitstayid
        ,time
        ,glucose_bedside
        ,glucose_lab
  from {{ ref('stg_lab') }}
  where glucose_bedside is not null or
      glucose_lab is not null
),
lab_nursing as (
  select patientunitstayid
      ,time
      ,glucose_bedside
      ,glucose_lab
  from lab 
  union all
  select patientunitstayid
      ,time
      ,CAST(glucose_bedside as FLOAT64)
      ,NULL as glucose_lab
  from {{ ref('stg_nursecharting') }}
)

select patientunitstayid
      ,time
      ,avg(glucose_bedside) as glucose_bedside
      ,avg(glucose_lab) as glucose_lab
from lab_nursing
group by patientunitstayid,
      time
order by patientunitstayid,
      time