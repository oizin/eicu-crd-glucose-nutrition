
with categories1 as (
  select * from {{ ref('_intakeoutput_nutrition') }}
),
categories2 as (
  select * from {{ ref('_intakeoutput_nonnutrition') }}
)

select patientunitstayid,
    coalesce(t2.category,t3.category) as category,
    coalesce(t2.`group`,t3.`group`) as `group`,
    intakeoutputoffset as time,
    intaketotal,
    outputtotal,
    nettotal,
    intakeoutputentryoffset,
    trim(lower(t1.cellpath)) as cellpath,
    trim(lower(t1.celllabel)) as celllabel,
    cellvaluenumeric
from `eicu_crd.intakeoutput` t1
left join categories1 t2
on trim(lower(t1.celllabel)) = lower(t2.celllabel)
left join categories2 t3
on trim(lower(t1.celllabel)) = lower(t3.celllabel)
order by patientunitstayid,intakeoutputoffset
