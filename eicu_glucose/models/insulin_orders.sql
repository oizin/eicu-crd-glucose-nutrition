
select patientunitstayid,
      time,
      stop_time,
      drugname,
      NULL as insulin_rate,
      NULL as insulin_total_ml,
      dosage as insulin_dose,
      route,
      frequency_english,
      times_per_day,
      insulin_type,
      insulin_acting,
      dose,
      dose_low,
      dose_high,
      prn
from {{ ref('stg_medication') }}
where category = 'insulin'