
with insulin_type as (
  select * from {{ ref('_medication_insulin_type') }}
),
frequencies as (
  select * from {{ ref('_medication_frequency')}}
),
dosages as (
  select * from {{ ref('_medication_dosage')}}
),
drugnames as (
  select * from {{ ref('_medication_drugname')}}
),
routes as (
  select * from {{ ref('_medication_routeadmin')}}
)


select patientunitstayid,
    `group`,
    category,
    drugstartoffset as time,
    drugstopoffset as stop_time,
    drugivadmixture,
    drugordercancelled,
    lower(t1.drugname) as drugname,
    t1.drughiclseqno,
    t1.dosage,
    lower(t1.routeadmin) as routeadmin,
    lower(t1.frequency) as frequency,
    loadingdose,
    lower(prn) as prn,
    gtc,
    insulin_type,
    insulin_acting,
    times_per_day,
    lower(frequency_english) as frequency_english,
    dose,
    dose_low,
    dose_high,
    route
from `eicu_crd.medication` t1
left join insulin_type t2
on trim(lower(t1.drugname)) = trim(lower(t2.drugname))
left join frequencies t3
on trim(lower(t1.frequency)) = trim(lower(t3.frequency))
left join dosages t4
on lower(t1.dosage) = lower(t4.dosage)
left join drugnames t5
on lower(t1.drugname) = lower(t5.drugname)
left join routes t6
on trim(lower(t1.routeadmin)) = trim(lower(t6.routeadmin))
--where lower(t2.category) != ''
order by patientunitstayid,time


