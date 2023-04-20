
select patientunitstayid,
    category,
    celllabel as product,
    cellvaluenumeric as oral_volume,
    time,
    1 as source_intakeoutput
from {{ ref('stg_intakeoutput') }}
where `group` = 'nutrition-oral'