
select patientunitstayid,
      nursingchartoffset as time,
      nursingchartvalue as glucose_bedside
from `eicu_crd.nursecharting`
where lower(nursingchartcelltypevalname) like '%bedside glucose%' AND
    nursingchartoffset > -60*12
    
