
with categories as (
  select * from {{ ref('_infusiondrug') }}
)

select patientunitstayid,
      t2.category,
      t2.`group`,
      infusionoffset as time,
      lower(t1.drugname) as drugname,
      drugrate,
      infusionrate,
      drugamount,
      volumeoffluid,
      patientweight
from `eicu_crd.infusiondrug` t1
left join categories t2
on lower(t1.drugname) = lower(t2.drugname)
order by patientunitstayid,time