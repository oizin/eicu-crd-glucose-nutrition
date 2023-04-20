library(bigrquery)
library(data.table)
library(rjson)
library(ggplot2)
library(glue)

projectid <- fromJSON(file = ".projectid.json")$projectid
email <- fromJSON(file = ".projectid.json")$email
path <- fromJSON(file = ".projectid.json")$path

bigrquery::bq_auth(path=path,email=email)

## IMPORT DATA FROM SQL DATABASE ==============================================

################# base or derived eICU-CRD data ###############################
## hospital information
sql <- glue("select * from {projectid}.eicu_crd.hospital")
hospital <- bq_project_query(projectid, sql)
hospital <- bq_table_download(hospital, n_max = Inf)
setDT(hospital)
fwrite(hospital,"data/raw/hospital.csv")

## general patient information 
# icustay detail
sql <- glue("select * from {projectid}.eicu_crd_derived.icustay_detail")
icustay <- bq_project_query(projectid, sql)
icustay <- bq_table_download(icustay, n_max = Inf)
setDT(icustay)
fwrite(icustay,"data/raw/icustay_detail.csv")
# patient
sql <- glue("select * from {projectid}.eicu_crd.patient")
patient <- bq_project_query(projectid, sql)
patient <- bq_table_download(patient, n_max = Inf)
setDT(patient)
cols_to_convert <- grep("24", names(patient), value = TRUE)
patient[, (cols_to_convert) := lapply(.SD, as.character), .SDcols = cols_to_convert]
fwrite(patient,"data/raw/patient.csv")

## admission diagnosis
sql <- glue("
select patientunitstayid,admitdxenteredoffset,admitdxpath,admitdxname,admitdxtext 
from {projectid}.eicu_crd.admissiondx;")
sql <- glue(sql)
tb <- bq_project_query(projectid, sql)
admission_diag <- bq_table_download(tb, n_max = Inf,page_size=10000)
setDT(admission_diag)
# extract admission diagnosis, organ system, elective status
elective <- admission_diag[admitdxpath %like% "Elective"]
elective[,elective_admission := fifelse(admitdxname == "Yes",1,0)]
elective <- elective[,.(patientunitstayid,elective_admission)]
admission_diag <- admission_diag[!admitdxpath %like% "Elective"]
organ_system <- admission_diag[admitdxpath %like% "Organ System"]
organ_system <- organ_system[,.(patientunitstayid,organ_system = admitdxtext)]
admission_diag <- admission_diag[!admitdxpath %like% "Organ System"]
diagnosis <- admission_diag[admitdxpath %like% "All Diagnosis"]
diagnosis[,operative := ifelse(admitdxpath %like% "Operative",1,0)]
diagnosis[,apache_dx := admitdxtext]
diagnosis <- diagnosis[,.(patientunitstayid,operative,apache_dx)]
admission_diag <- admission_diag[!admitdxpath %like% "All Diagnosis"]
## DKA/glycaemic related stuff
# diabetic_ketoacidosis
diagnosis[,dka := fifelse(tolower(apache_dx) == "diabetic ketoacidosis",1,0)]
# hyperosmolar_hyperglycemia
diagnosis[,hhs := fifelse(tolower(apache_dx) == "hyperglycemic hyperosmolar",1,0)]
# acid_base_disturbance
diagnosis[,acid_base_disturbance := fifelse(tolower(apache_dx) 
                      %like% "acid-base/electrolyte disturbance",1,0)]
# hypoglycaemia
diagnosis[,hypoglycaemia := fifelse(tolower(apache_dx) %like% "hypoglycemia",1,0)]
# sepsis
diagnosis[,sepsis := fifelse(tolower(apache_dx) %like% "sepsis",1,0)]
# save 
fwrite(diagnosis,file = "data/raw/apache_diagnosis.csv")
fwrite(elective,file = "data/raw/apache_elective.csv")
fwrite(organ_system,file = "data/raw/apache_organ_system.csv")

# nurse care - nutrition
sql <- glue("select * from {projectid}.eicu_crd.nursecare 
            where lower(cellattributepath) like '%nutrition%'")
nursecare_nutrition <- bq_project_query(projectid, sql)
nursecare_nutrition <- bq_table_download(nursecare_nutrition, n_max = Inf)
setDT(nursecare_nutrition)
fwrite(nursecare_nutrition,file = "data/raw/nursecare_nutrition.csv")

################ curated eICU blood glucose etc data ###########################

read_save <- function(projectid,table,page_size=NULL) {
  sql <- glue("select * from {projectid}.eicu_crd_glucose.{table}")
  tab <- bq_project_query(projectid, sql)
  tab <- bq_table_download(tab, n_max = Inf,page_size=page_size)
  setDT(tab)
  fwrite(tab,glue("data/raw/{table}.csv"))
}

read_save(projectid,"glucose")
read_save(projectid,"diabetes")
read_save(projectid,"insulin_orders")
read_save(projectid,"insulin_administrations")
#read_save(projectid,"insulin_infusions")
#read_save(projectid,"insulin_sc")
#read_save(projectid,"insulin_other")
read_save(projectid,"enteral")
read_save(projectid,"parenteral")
read_save(projectid,"iv_dextrose",20000)
read_save(projectid,"oral_intake")


