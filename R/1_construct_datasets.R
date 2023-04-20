library(data.table)

## IMPORT DATA FROM LOCAL =====================================================

glucose <- fread("data/raw/glucose.csv")
insulin_orders <- fread("data/raw/insulin_orders.csv")
insulin_admin <- fread("data/raw/insulin_administrations.csv")
#insulin_infusions <- fread("data/raw/insulin_infusions.csv")
#insulin_sc <- fread("data/raw/insulin_sc.csv")
#insulin_other <- fread("data/raw/insulin_other.csv")
patient <- fread("data/raw/patient.csv")
icustay <- fread("data/raw/icustay.csv")
diabetes <- fread("data/raw/diabetes.csv")

################ Analysis datasets #############################################

## glucose
# join in other tables
patient[,unitadmittime24 := as.POSIXct(paste0("1970-01-01 ", unitadmittime24), 
                                       format = "%Y-%m-%d %H:%M:%S")]
glucose <- merge(glucose,patient,by="patientunitstayid")
glucose <- merge(glucose,diabetes,by="patientunitstayid",all.x=TRUE)
glucose <- merge(glucose,icustay[,.(patientunitstayid,icu_los_hours,hosp_mort,apache_iv)],
                 by="patientunitstayid")
glucose <- glucose[order(patientunitstayid,time)] 
# pre 12 hours
glucose <- glucose[time <= unitdischargeoffset & time > -12*60]
# add vars
# time
glucose[,dy := time / 60 / 24]
glucose[,dy0 := round(dy)]
glucose[,time_diff := (shift(time,-1) - time)/60,by=patientunitstayid]
glucose[,n := seq(1,.N),by=patientunitstayid]
glucose[,time_24 := unitadmittime24 + time*60]
glucose[,time_24_h := hour(time_24)]
# glucose
glucose[,glucose := fcoalesce(glucose_bedside,glucose_lab)]
glucose[,measure := fifelse(is.na(glucose_bedside),"lab","bedside")]
glucose[glucose <= 0,glucose := NA]
glucose[,day_1_hypo := fifelse(glucose < 70 & dy0 == 0,1,0)]
glucose[,day_1_hypo := max(day_1_hypo),by=patientunitstayid]
glucose[,day_1_hyper := fifelse(glucose > 180 & dy0 == 0,1,0)]
glucose[,day_1_hyper := max(day_1_hyper),by=patientunitstayid]
glucose[,change_glucose := (shift(glucose,-1) - glucose),by=patientunitstayid]
glucose[,change_glucose1 := log(shift(glucose,-1)/glucose),by=patientunitstayid]
insulin_pts <- unique(insulin$patientunitstayid)
glucose[,insulin := fifelse(patientunitstayid %in% insulin_pts,1,0)]
infusion_pts <- unique(insulin[insulin_rate > 0 | insulin_total_ml > 0 | route == "iv"]$patientunitstayid)
glucose[,insulin_infusion := fifelse(patientunitstayid %in% infusion_pts,1,0)]

fwrite(glucose,"data/analysis/glucose.csv")

## insulin orders
# join in other tables
insulin_orders <- merge(insulin_orders,patient,by="patientunitstayid")
insulin_orders <- merge(insulin_orders,diabetes,by="patientunitstayid",all.x=TRUE)
insulin_orders <- merge(insulin_orders,icustay[,.(patientunitstayid,icu_los_hours,hosp_mort,apache_iv)],
                 by="patientunitstayid")
insulin_orders <- insulin_orders[order(patientunitstayid,time)] 
# pre 12 hours
#insulin <- insulin[time <= unitdischargeoffset & time > -12*60]
fwrite(insulin_orders,"data/analysis/insulin_orders.csv")

## insulin administrations
# join in other tables
insulin_admin <- merge(insulin_admin,patient,by="patientunitstayid")
insulin_admin <- merge(insulin_admin,diabetes,by="patientunitstayid",all.x=TRUE)
insulin_admin <- merge(insulin_admin,icustay[,.(patientunitstayid,icu_los_hours,hosp_mort,apache_iv)],
                           by="patientunitstayid")
insulin_admin <- insulin_admin[order(patientunitstayid,time)] 
# pre 12 hours
#insulin <- insulin[time <= unitdischargeoffset & time > -12*60]
fwrite(insulin_admin,"data/analysis/insulin_administrations.csv")

# ## insulin infusions
# # join in other tables
# insulin_infusions <- merge(insulin_infusions,patient,by="patientunitstayid")
# insulin_infusions <- merge(insulin_infusions,diabetes,by="patientunitstayid",all.x=TRUE)
# insulin_infusions <- merge(insulin_infusions,icustay[,.(patientunitstayid,icu_los_hours,hosp_mort,apache_iv)],
#                         by="patientunitstayid")
# insulin_infusions <- insulin_infusions[order(patientunitstayid,time)] 
# # pre 12 hours
# #insulin <- insulin[time <= unitdischargeoffset & time > -12*60]
# fwrite(insulin_infusions,"data/analysis/insulin_infusions.csv")
# 
# ## insulin sc
# # join in other tables
# insulin_sc <- merge(insulin_sc,patient,by="patientunitstayid")
# insulin_sc <- merge(insulin_sc,diabetes,by="patientunitstayid",all.x=TRUE)
# insulin_sc <- merge(insulin_sc,icustay[,.(patientunitstayid,icu_los_hours,hosp_mort,apache_iv)],
#                            by="patientunitstayid")
# insulin_sc <- insulin_sc[order(patientunitstayid,time)] 
# # pre 12 hours
# #insulin <- insulin[time <= unitdischargeoffset & time > -12*60]
# fwrite(insulin_sc,"data/analysis/insulin_sc.csv")
# 
# ## insulin other
# # join in other tables
# insulin_other <- merge(insulin_other,patient,by="patientunitstayid")
# insulin_other <- merge(insulin_other,diabetes,by="patientunitstayid",all.x=TRUE)
# insulin_other <- merge(insulin_other,icustay[,.(patientunitstayid,icu_los_hours,hosp_mort,apache_iv)],
#                     by="patientunitstayid")
# insulin_other <- insulin_other[order(patientunitstayid,time)] 
# # pre 12 hours
# #insulin <- insulin[time <= unitdischargeoffset & time > -12*60]
# fwrite(insulin_other,"data/analysis/insulin_other.csv")




