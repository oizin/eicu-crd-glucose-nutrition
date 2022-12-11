library(data.table)
library(ggplot2)
library(scales)
library(stringr)
## data ## ---------------------------------------------------------------------
# patient
icustay <- fread("data/raw/icustay.csv")
pt_info <- fread("data/raw/diabetes.csv")
pt_info[,diabetes_f := fifelse(diabetes == 0,"Diabetic","Non-diabetic")]
glucose_lactate <- fread("data/raw/glucose_lactate.csv")
glucose_lactate[time >= 0 & !is.na(glucose_bedside),min_time := min(time),by=patientunitstayid]
glucose0 <- glucose_lactate[min_time == time]

# tables
infusiondrug <- fread("data/raw/infusiondrug.csv")
medication <- fread("data/raw/medication.csv")

# restrict to insulin
infusiondrug <- infusiondrug[group == "insulin"]
medication <- medication[group == "insulin"]

# move to construct

# join infusion details to patient details
infusiondrug <- merge(infusiondrug,icustay,by="patientunitstayid")
infusiondrug[,endtime := shift(time,-1),by=patientunitstayid]
infusiondrug[is.na(endtime),endtime := unitdischargeoffset]
infusiondrug <- merge(infusiondrug,pt_info,by="patientunitstayid")


## medication data fixes (move) ##
# join medication details to patient details
medication <- merge(medication,icustay,by="patientunitstayid")
# route
routeadmin <- fread("data/item-counts/routeadmin.csv")
medication <- merge(medication,routeadmin[,.(routeadmin,route)],by="routeadmin")
# insulin type
insulins <- fread("data/item-counts/drugname.csv")
medication <- merge(medication,insulins[,.(drugname,insulin_type,insulin_acting)],by="drugname")
medication[,insulin_acting := factor(insulin_acting,levels=c("short","intermediate","long"),ordered=TRUE)]

rec_dose <- grepl("[0-9]", medication$dosage)
medication[!rec_dose,.N,by=drugname][order(-N)]
# frequency info
frequency_info <- fread("data/item-counts/frequency.csv")
medication <- merge(medication,frequency_info[,.(frequency,times_per_day,frequency_english)],by="frequency",all.x=TRUE)
# dose info
dosage_info <- fread("data/item-counts/dosage.csv")
medication <- merge(medication,dosage_info[,.(dosage,dose,dose_low,dose_high)],by="dosage",all.x=TRUE)

# insulin patients
insulin_pts <- unique(c(infusiondrug$patientunitstayid,medication$patientunitstayid))
glucose0[,insulin := fifelse(patientunitstayid %in% insulin_pts,1,0)]
glucose0 <- merge(glucose0,pt_info,by="patientunitstayid")
icustay[,insulin := fifelse(patientunitstayid %in% insulin_pts,1,0)]


## insulin distributions -------------------------------------------------------

infusiondrug[,drugrate := as.numeric(drugrate)]
infusiondrug[,infusionrate := as.numeric(infusionrate)]
infusiondrug[,drugamount := as.numeric(drugamount)]
infusiondrug[,volumeoffluid := as.numeric(volumeoffluid)]

# how many missing?
summary(infusiondrug$drugrate)
summary(infusiondrug$infusionrate)
summary(infusiondrug$drugamount)
summary(infusiondrug$volumeoffluid)

# example infusions
set.seed(123)
infusion_max <- infusiondrug[,.(icu_los_hours=max(icu_los_hours),drugrate=max(drugrate)),by=patientunitstayid]
samp10 <- sample(infusion_max[icu_los_hours > 24 & drugrate < 20,patientunitstayid],10)
infusiondrug_samp <- infusiondrug[patientunitstayid %in% samp10]
ggplot(infusiondrug_samp,aes(x=time/60,y=drugrate)) +
  geom_segment(aes(xend=endtime/60,yend=drugrate)) +
  geom_step() +
  coord_cartesian(xlim=c(0,48)) +
  geom_rect(aes(xmin = time/60, xmax = endtime/60, 
                ymin = 0, ymax = drugrate), alpha = 0.7,
            fill="lightblue") +
  #scale_y_continuous(labels = label_comma(),n.breaks=6) +
  theme_bw(base_size=14) +
  facet_wrap(~patientunitstayid,ncol=2) +
  labs(x = "Time since ICU admission (hours)",y="Insulin infusion rate (units/hr??)")
# distribution of infusion rate
ggplot(infusiondrug[drugrate > 0],aes(x=drugrate)) +
  geom_histogram(binwidth=1,col="white",fill="lightblue") +
  coord_cartesian(xlim=c(0,20)) +
  facet_wrap(~diabetes_f,ncol=1,scales="free_y") +
  scale_y_continuous(labels = label_comma(),n.breaks=6) +
  theme_bw(base_size=14) +
  labs(x = "Insulin infusion rate (units/hr??)",y="Count")
# distribution of infusion subcutaneous
dose_time_insulin <- medication[,.N,by=.(dose,times_per_day,insulin_acting)]
medication[,dose_g := cut(dose,c(0,5,10,20,Inf))]
ggplot(medication[route=="sc" & dose > 0],aes(x=dose_g,fill=insulin_acting)) +
  geom_bar(position="dodge",col="white") +
  labs(x = "Units of insulin") +
  theme_bw(base_size=14)
ggplot(medication[route=="sc"],aes(x=times_per_day,fill=insulin_acting)) +
  geom_bar(position = position_dodge(preserve = "single"),col="white") +
  labs(x = "Injections per day") +
  coord_cartesian(xlim=c(0,7)) +
  theme_bw(base_size=14)

dose_low_high <- melt(medication[route=="sc"][,.(dose_low,dose_high)])
ggplot(dose_low_high,aes(x=variable,y=value)) +
  geom_boxplot(width=0.2) +
  theme_bw(base_size=14) 
# insulin by glucose
ggplot(glucose0,aes(x=glucose_bedside,y=insulin,col=diabetes_f)) +
  geom_smooth(method = 'gam',formula=y ~ s(x, bs = "cs"),method.args=list(family=binomial())) +
  scale_x_log10(limits=c(80,1000),n.breaks=10) +
  scale_y_continuous(limits=c(0,1),n.breaks=6) +
  labs(x = "Blood glucose (mg/dL)",y = "Probability of receiving insulin") +
  theme_bw(base_size=14) +
  scale_color_discrete(name="")
# subcutaneous
samp10 <- sample(unique(medication$patientunitstayid),10)
ggplot(medication[route == "sc" & patientunitstayid %in% samp10]) +
  geom_linerange(aes(xmin=time,xmax=drugstopoffset,y=insulin_type)) +
  facet_wrap(~patientunitstayid)


## data quality ---------------------------------------------------------------

medication[,drug_length := drugstopoffset - time]

ggplot(medication[drug_length > 0],aes(x = drug_length/60/24)) +
  geom_histogram(binwidth=1/12,color="white",fill="lightblue") +
  coord_cartesian(xlim = c(0,14)) +
  labs(x = "Length of prescription (days)",y = "Count") +
  scale_x_continuous(breaks = seq(0,18,by=2)) +
  theme_bw(base_size=14)


medication[,rec_dose := fifelse(grepl("[0-9]", dosage) & !grepl("ML", dosage),"Dose recorded","No dose recorded")]
medication[,rec_freq := fifelse(frequency != "" & 
  frequency != "pyxis" & 
  frequency != "per protocol" &
  frequency != "daily prn" &
  frequency != "prn","Frequency recorded","No frequency recorded")]
medication[,prn_no := fifelse(prn == "no","Not PRN","PRN")]
medication[,usable := fifelse(rec_dose == "Dose recorded" & rec_freq == "Frequency recorded" & prn_no == "Not PRN",
                              "usable","not usable")]
medication[route == "iv",usable := fifelse(rec_dose == "Dose recorded","usable","not usable")]
medication[route == "other",usable := "not usable"]

medication[,usable := factor(usable,levels = c("usable","not usable"),ordered=TRUE)]


medication[,info := fifelse(usable == "usable",1,0)]
med_by_hosp <- medication[,.(info = mean(info),.N),by=.(hospitalid,route)]
med_by_hosp[,N := sum(N),by=hospitalid]
ggplot(med_by_hosp[route != "other"],aes(x=N,y=info,col=route)) +
  facet_wrap(~route) +
  geom_point()


ggplot(medication[route %in% c("sc")],aes(x = prn_no,fill=rec_freq,col=usable)) +
  geom_bar(position = "dodge",size=1) +
  facet_wrap(~rec_dose) +
  scale_fill_discrete(name="") +
  theme_bw(base_size=14) +
  scale_color_manual(values = c("black",alpha("white",0.0))) +
  guides(color="none") +
  labs(x = "",y = "Count")

hospital_insulin <- icustay[,.(insulin=mean(insulin),.N),by=hospitalid]
ggplot(hospital_insulin,aes(x=N,y=insulin)) +
  geom_point()

# glucose available by frequency
glucose_lactate[,glucose := fcoalesce(glucose_bedside,glucose_lab)]
glucose <- glucose_lactate[!is.na(glucose)]
glucose <- merge(glucose,icustay,by="patientunitstayid",all.x=TRUE)
glucose[,day := floor(time/60/24)]

n_glucose_per_day <- glucose[,.N,by=.(patientunitstayid,day,hospitalid)]
medication[,day := floor(time/60/24)]
medication[,stop_day := floor(drugstopoffset/60/24)]

med_day <- medication[route=="sc",.(patientunitstayid,day,stop_day,times_per_day,hospitalid)]
setnames(med_day,old = "day",new = "start_day")
n_gluc_med_day <- n_glucose_per_day[med_day, on=.(day <= stop_day, day >= start_day, 
                                                  patientunitstayid == patientunitstayid,
                                                  hospitalid == hospitalid),
                  .(hospitalid,patientunitstayid,start_day,stop_day,day=x.day,N,times_per_day)]
n_gluc_med_day0 <- n_gluc_med_day[,.(times_per_day = sum(times_per_day,na.rm=TRUE),N = max(N)),by=.(hospitalid,patientunitstayid,day)]
n_gluc_med_day0[times_per_day == 0,times_per_day := NA]
n_gluc_med_day0[,.(rr = mean(N/times_per_day,na.rm=TRUE),.N),by=hospitalid]
ggplot(n_gluc_med_day0[,.(rr = mean(N/times_per_day,na.rm=TRUE),.N),by=hospitalid],
       aes(x=N,y=rr)) +
  geom_point() +
  geom_hline(yintercept = 1)



