library(bigrquery)
library(data.table)
library(rjson)
library(ggplot2)

projectid <- fromJSON(file = ".projectid.json")$projectid
email <- fromJSON(file = ".projectid.json")$email
path <- fromJSON(file = ".projectid.json")$path

bigrquery::bq_auth(email=email,path=path)

## IMPORT DATA FROM SQL DATABASE ==============================================

## general patient information ------------------------------------------------
sql <- "select * from icu-data-260103.eicu_crd_derived.icustay_detail"
icustay <- bq_project_query(projectid, sql)
icustay <- bq_table_download(icustay, n_max = Inf)
setDT(icustay)
fwrite(intakeoutput,"data/raw/icustay.csv")

# intakeoutput table -----------------------------------------------------------
sql <- "SELECT patientunitstayid,intakeoutputoffset,intaketotal,outputtotal,
              nettotal,intakeoutputentryoffset,LOWER(cellpath) as cellpath,
              LOWER(celllabel) as celllabel,cellvaluenumeric
FROM `icu-data-260103.eicu_crd.intakeoutput`
WHERE LOWER(cellpath) like '%nutrition%'
ORDER BY patientunitstayid,intakeoutputoffset"
intakeoutput <- bq_project_query(projectid, sql)
intakeoutput <- bq_table_download(intakeoutput, n_max = Inf)
setDT(intakeoutput)
intakeoutput[,celllabel := stringr::str_trim(celllabel)]
# category data nutritional
intakeoutput_nutrition <- fread("data/item-counts/intakeoutput-nutrition.csv")
intakeoutput_nutrition <- unique(intakeoutput_nutrition[,.(celllabel,category,group)])
intakeoutput_nutrition <- intakeoutput_nutrition[category != ""]
intakeoutput_nonnutrition <- fread("data/item-counts/intakeoutput-nonnutrition.csv")
intakeoutput_nonnutrition <- unique(intakeoutput_nonnutrition[,.(celllabel,category,group)])
intakeoutput_nonnutrition <- intakeoutput_nonnutrition[group %in% c("nutrition-oral",
                                                                    "nutrition-enteral",
                                                                    "nutrition-parenteral",
                                                                    "nutrition-other")]
intakeoutput_nonnutrition[,celllabel := tolower(celllabel)]
lookup <- unique(rbind(intakeoutput_nutrition,intakeoutput_nonnutrition))
intakeoutput <- merge(intakeoutput[,-c('cellpath')],lookup,by="celllabel",all.x = TRUE)

# general
unique(intakeoutput[,.(patientunitstayid,group)])[,.N,by=group]

fwrite(intakeoutput,"data/raw/intakeoutput.csv")

# infusiondrug table  ----------------------------------------------------------
sql <- "SELECT patientunitstayid,infusionoffset as time,lower(drugname) as drugname,
              drugrate,infusionrate,drugamount,volumeoffluid,patientweight
FROM `icu-data-260103.eicu_crd.infusiondrug`
ORDER BY patientunitstayid,time"
infusiondrug <- bq_project_query(projectid, sql)
infusiondrug <- bq_table_download(infusiondrug, n_max = Inf)
setDT(infusiondrug)
# category data
infusiondrug_cats <- fread("data/item-counts/infusiondrug.csv")
infusiondrug_cats <- unique(infusiondrug_cats[,.(drugname=tolower(drugname),category,group)])
infusiondrug_cats <- infusiondrug_cats[category != ""]
# merge
infusiondrug <- merge(infusiondrug,infusiondrug_cats,by="drugname",all.x = TRUE)

fwrite(infusiondrug,"data/raw/infusiondrug.csv")

# medication table -------------------------------------------------------------
sql <- "select patientunitstayid,drugstartoffset as time,drugivadmixture,drugordercancelled,
lower(drugname) as drugname,drughiclseqno,dosage,lower(routeadmin) as routeadmin,
lower(frequency) as frequency,loadingdose,lower(prn) as prn,drugstopoffset,gtc
from `icu-data-260103.eicu_crd.medication`
order by patientunitstayid,time"
medication <- bq_project_query(projectid, sql)
medication <- bq_table_download(medication, n_max = Inf)
setDT(medication)
# category data
medication_cats <- fread("data/item-counts/medication.csv")
medication_cats <- unique(medication_cats[,.(drugname=tolower(drugname),category,group)])
medication_cats <- medication_cats[category != ""]
# merge
medication <- merge(medication,medication_cats,by="drugname",all.x = TRUE)

fwrite(medication,"data/raw/medication.csv")

# glucose data -----------------------------------------------------------------
sql <- "with tmp as (
  select patientunitstayid,labtypeid,labresultoffset as time,
          (case when lower(labname) = 'glucose' then labresult else null end) as glucose_lab,
          (case when lower(labname) = 'bedside glucose' then labresult else null end) as glucose_bedside,
          (case when lower(labname) = 'lactate' then labresult else null end) as lactate
  from `icu-data-260103.eicu_crd.lab`
  where (labresultoffset > -12*60) and lower(labname) in ('glucose','lactate','bedside glucose')
)
select patientunitstayid,time,
       avg(glucose_lab) as glucose_lab,
       avg(glucose_bedside) as glucose_bedside,
       avg(lactate) as lactate,
       min(labtypeid) as labtypeid
from tmp
group by patientunitstayid,time
order by patientunitstayid,time
"
tb <- bq_project_query(projectid, sql)
gluc_lact <- bq_table_download(tb, n_max = Inf)
setDT(gluc_lact)

sql <- "
SELECT patientunitstayid,nursingchartoffset as time,nursingchartvalue as glucose_bedside
FROM `icu-data-260103.eicu_crd.nursecharting`
WHERE lower(nursingchartcelltypevalname) like '%bedside glucose%' AND
    nursingchartoffset > -60*12"
tb <- bq_project_query(projectid, sql)
gluc_bed <- bq_table_download(tb, n_max = Inf)
setDT(gluc_bed)
gluc_bed[,glucose_bedside := as.numeric(glucose_bedside)]

gluc <- rbind(gluc_lact,gluc_bed,fill=TRUE)
gluc <- gluc[order(patientunitstayid,time )]
gluc <- gluc[,.(glucose_lab=mean(glucose_lab,na.rm=TRUE),
        glucose_bedside=mean(glucose_bedside,na.rm=TRUE),
        lactate=mean(lactate,na.rm=TRUE),
        labtypeid=mean(labtypeid,na.rm=TRUE)),by=.(patientunitstayid,time)]

fwrite(gluc,file = "data/raw/glucose_lactate.csv")
# 
# fwrite(gluc_lact,file = "data/eicu/glucose_lactate.csv")


