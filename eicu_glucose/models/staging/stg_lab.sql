
with tmp as (
  select patientunitstayid,labtypeid,labresultoffset as time,
          (case when lower(labname) = 'glucose' then labresult else null end) as glucose_lab,
          (case when lower(labname) = 'bedside glucose' then labresult else null end) as glucose_bedside,
          (case when lower(labname) = 'lactate' then labresult else null end) as lactate
  from `eicu_crd.lab`
  where (labresultoffset > -12*60) and lower(labname) in ('glucose','lactate','bedside glucose')
)
select patientunitstayid,
       time,
       avg(glucose_lab) as glucose_lab,
       avg(glucose_bedside) as glucose_bedside,
       avg(lactate) as lactate,
       min(labtypeid) as labtypeid
from tmp
group by patientunitstayid,time
order by patientunitstayid,time
