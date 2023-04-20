
with treatment as (
  select * from {{ ref('_treatment')}}
)

select patientunitstayid,
      treatmentoffset as time,
      t1.treatmentstring,
      category,
      `group`
from `eicu_crd.treatment` t1
left join treatment t2
on trim(lower(t1.treatmentstring)) = trim(lower(t2.treatmentstring))