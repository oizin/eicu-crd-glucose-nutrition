library(data.table)
library(ggplot2)

## icustay ## -----------------------------------------------------------------
icustay <- fread("data/raw/icustay.csv")
pt_info <- fread("data/raw/diabetes.csv")

## intakeoutput ## ------------------------------------------------------------
intakeoutput <- fread("data/raw/intakeoutput.csv")

## enteral ##
enteral_intakeoutput <- intakeoutput[category %in% c("enteral-nonspecific-intake",
                                                     "enteral-product-intake")]
enteral_intakeoutput <- enteral_intakeoutput[order(patientunitstayid,intakeoutputoffset)]
setnames(enteral_intakeoutput,c("intakeoutputoffset"),c("time"))
enteral_intakeoutput[,time_diff := time - shift(time,1),by=patientunitstayid]
# new/fix variables
# OVER 100
enteral_intakeoutput[cellvaluenumeric > 1000]
enteral_intakeoutput[,cellvaluenumeric1 := cellvaluenumeric]
enteral_intakeoutput[cellvaluenumeric > 100,cellvaluenumeric1 := cellvaluenumeric/time_diff*60]
enteral_intakeoutput[cellvaluenumeric1 > 200,cellvaluenumeric1 := NA]
enteral_intakeoutput[,cellvaluenumeric_g100:= fifelse(cellvaluenumeric > 100,1,0)]

## data quality ##
# data quality: generic and specific feeding?
tab <- enteral_intakeoutput[,.N,by=.(category,patientunitstayid)][order(patientunitstayid)]
mult_ids <- tab[,.N,by=patientunitstayid][N > 1]
length(unique(enteral_intakeoutput[patientunitstayid %in% mult_ids$patientunitstayid]$patientunitstayid))

# data quality: duplicated timestamps - zero
tab <- enteral_intakeoutput[,.N,by=.(time,patientunitstayid)][order(patientunitstayid)]
mult_ids <- tab[,.N,by=patientunitstayid][N > 1]
nrow(enteral_intakeoutput[patientunitstayid %in% mult_ids$patientunitstayid & 
                            time %in% mult_ids$time])

# data quality: cellvalue
quantile(enteral_intakeoutput$cellvaluenumeric,probs = seq(0,1,0.01))
ggplot(enteral_intakeoutput[cellvaluenumeric < 2000],aes(x=cellvaluenumeric)) +
  stat_ecdf(geom = "step") +
  scale_x_continuous(n.breaks=20)
ggplot(enteral_intakeoutput,aes(x=cellvaluenumeric1)) +
  stat_ecdf(geom = "step") +
  scale_x_continuous(n.breaks=20)
ggplot(enteral_intakeoutput[cellvaluenumeric < 2000],aes(x=cellvaluenumeric)) +
  geom_histogram(binwidth=7,col="white") +
  theme_bw(base_size=14)

## summary stats and plots ##
# samples trajectory plots
set.seed(123)
samp_ids <- sample(enteral_intakeoutput$patientunitstayid,10)
enteral_intakeoutput[,time_dy := time/(60*24)]
enteral_intakeoutput[,time_dy_next := shift(time_dy,-1),by=patientunitstayid]
p1 <- ggplot(enteral_intakeoutput[patientunitstayid %in% samp_ids & 
                              !is.na(cellvaluenumeric1)],
       aes(x=time_dy,y=cellvaluenumeric1,group = patientunitstayid)) +
  geom_rect(aes(xmin = time_dy, xmax = time_dy_next, 
                ymin = 0, ymax = cellvaluenumeric1), alpha = 0.7,
            fill="lightblue") +
  geom_step() +
  labs(x="Time since ICU admission (days)",y="Feeding rate (mL/hour)") +
  coord_cartesian(ylim=c(0,120),xlim=c(0,5)) +
  facet_wrap(~patientunitstayid,ncol=2,scales="free_x") +
  theme_bw(base_size = 14)
p1
ggsave("graphs/enteral_trajectory_samples.png",plot = p1,height = 12)
# time to enteral
time_to_enteral <- enteral_intakeoutput[,.(enteral_start = min(time_dy)),
                                        by=patientunitstayid]
p2 <- ggplot(time_to_enteral,aes(x = enteral_start)) +
  geom_histogram(binwidth=0.5,col="white",fill="lightblue") +
  labs(x = "Time since ICU admission (days)",y = "Count") +
  scale_x_continuous(breaks=seq(-2,14,by=2)) +
  coord_cartesian(xlim=c(-1,14)) +
  theme_bw(base_size=14)
p2
ggsave("graphs/time_to_enteral.png",plot=p2)
# feeding rate
p3 <- ggplot(enteral_intakeoutput,aes(x = cellvaluenumeric1)) +
  geom_histogram(col="white",fill="lightblue",bins=20) +
  labs(x = "Feeding rates (mL/hr)",y = "Count") +
  coord_cartesian(xlim=c(0,120)) +
  theme_bw(base_size=14)
ggsave("graphs/enteral_feeding_rate.png",plot=p3)
# total feeding
enteral_intakeoutput[,dy := floor(time_dy)]
enteral_intakeoutput[,dy_next := shift(dy,-1),by=patientunitstayid]
enteral_intakeoutput[,feeding_time := pmax(time_dy_next,dy_next)-time_dy]

enteral_intakeoutput[,amount := cellvaluenumeric1*feeding_time*24]
total_feeding <- enteral_intakeoutput[,.(total=sum(amount,na.rm=TRUE)),by=.(patientunitstayid,dy)]
p4 <- ggplot(total_feeding[total != 0],aes(x = total)) +
  geom_histogram(col="white",fill="lightblue",binwidth=200) +
  labs(x = "Feeding total (mL/day)",y = "Count") +
  coord_cartesian(xlim=c(0,2500)) +
  theme_bw(base_size=14)
p4
ggsave("graphs/enteral_total_day.png",plot=p4)
gridExtra::grid.arrange(p1,p2,p3,p4,layout_matrix=matrix(c(1,1,1,2,3,4),ncol=2))
p_all <- gridExtra::arrangeGrob(p1+labs(title="(A)"),
                                p2+labs(title="(B)"),
                                p3+labs(title="(C)"),
                                p4+labs(title="(D)"),layout_matrix=matrix(c(1,1,1,2,3,4),ncol=2))
ggsave("graphs/enteral.png",plot=p_all,height = 8,width = 9)

## infusiondrug ## ------------------------------------------------------------
## enteral ##
infusiondrug <- fread("data/raw/infusiondrug.csv")

enteral_infusiondrug <- infusiondrug[category %in% c("enteral-nonspecific-intake",
                                                     "enteral-product-intake")]
enteral_infusiondrug[,drugrate := as.numeric(drugrate)]
enteral_infusiondrug[,drugamount := as.numeric(drugamount)]

# STAT: samples
samp_ids <- sample(enteral_infusiondrug$patientunitstayid,10)
ggplot(enteral_infusiondrug[patientunitstayid %in% samp_ids],
       aes(x=time/(60*24),y=drugrate)) +
  geom_step() +
  facet_wrap(~patientunitstayid,ncol=2) +
  theme_bw(base_size = 14)

## join ## ---------------------------------------------------------------------

## all nutrition ## ------------------------------------------------------------
enteral_pts <- unique(enteral_intakeoutput$patientunitstayid)
icustay[,enteral := fifelse(patientunitstayid %in% enteral_pts,1,0)]
oral_pts <- unique(intakeoutput[group == "nutrition-oral",patientunitstayid])
icustay[,oral := fifelse(patientunitstayid %in% oral_pts,1,0)]
parenteral_pts <- unique(intakeoutput[group == "nutrition-parenteral",patientunitstayid])
icustay[,parenteral := fifelse(patientunitstayid %in% parenteral_pts,1,0)]
icustay[,ventday1 := fifelse(patientunitstayid %in% pt_info[ventday1==1,patientunitstayid],1,0)]

icustay[,los_dy := icu_los_hours/24]
icustay[,los_dy := round(los_dy)]

## data quality ##
# hospital with no nutrition at all
icustay[,any_nutrition := parenteral==1 | oral == 1 | enteral == 1]
nutrition_any_hosp <- icustay[,.(any_nutrition = mean(any_nutrition)),by=hospitalid]
nutrition_any_hosp[any_nutrition == 0,.N]
no_nutrition_hospitalid <- nutrition_any_hosp[any_nutrition == 0,hospitalid]
icustay[!hospitalid %in% no_nutrition_hospitalid,.N]
# los
dy_tab <- icustay[!hospitalid %in% no_nutrition_hospitalid,.(Nt=.N),by=.(los_dy)]
# summary table
nutrition_dy <- rbind(icustay[enteral==1,.N,by=.(los_dy)][,.(los_dy,N,group="Enteral")],
      icustay[oral==1,.N,by=.(los_dy)][,.(los_dy,N,group="Oral")],
      icustay[parenteral==1,.N,by=.(los_dy)][,.(los_dy,N,group="Parenteral")],
      icustay[parenteral==1 | oral == 1 | enteral == 1,.N,by=.(los_dy)][,.(los_dy,N,group="Any nutrition")])
nutrition_dy <- merge(nutrition_dy,dy_tab,by="los_dy")
nutrition_dy[,p := N/Nt]
# graph
p1 <- ggplot(nutrition_dy[los_dy < 15]) +
  geom_bar(aes(x=los_dy,y=p*100,fill=group),
           stat="identity",position="dodge") +
  coord_cartesian(ylim=c(0,80)) +
  scale_x_continuous(breaks = seq(0,14,by=2)) +
  labs(x="Length of ICU stay (days)",y="Intake via route (%)") +
  theme_bw(base_size=14) +
  scale_fill_brewer(type = "qual",name="")
p1
ggsave("graphs/nutrition_by_los.png",p1)
# nutrition by hosp
hosp_tab <- icustay[,.(Nt=.N),by=.(hospitalid)]
nutrition_hosp <- rbind(icustay[,.(p = mean(enteral),.N,group="Enteral"),by=hospitalid],
                        icustay[,.(p = mean(oral),.N,group="Oral"),by=hospitalid],
                        icustay[,.(p = mean(parenteral),.N,group="Parenteral"),by=hospitalid],
                        icustay[,.(p = mean(any_nutrition),.N,group="Any nutrition"),by=hospitalid])
p2 <- ggplot(nutrition_hosp,aes(x=N,y=p*100,col=p==0.0)) +
  geom_point() +
  facet_wrap(~group,scale="free_y",nrow=1) +
  geom_hline(data=nutrition_hosp[!hospitalid %in% no_nutrition_hospitalid,
                                 (mean(p*100)),by=group],aes(yintercept = V1), color="blue") +
  theme_bw(base_size=14) +
  scale_x_continuous(n.breaks = 4) +
  labs(x="ICU admissions",y="Intake via route (%)") +
  scale_color_manual(values=c("black","tomato")) +
  theme(legend.position = "none")
p2
ggsave("graphs/nutrition_by_hosp.png",p2)
# nutrition by hosp - ventilated on day 1
hosp_tab_v <- icustay[ventday1==1,.(Nt=.N),by=.(hospitalid)]
nutrition_hosp_v <- rbind(icustay[ventday1==1,.(p = mean(enteral),.N,group="Enteral"),by=hospitalid],
                        icustay[ventday1==1,.(p = mean(oral),.N,group="Oral"),by=hospitalid],
                        icustay[ventday1==1,.(p = mean(parenteral),.N,group="Parenteral"),by=hospitalid],
                        icustay[ventday1==1,.(p = mean(any_nutrition),.N,group="Any nutrition"),by=hospitalid])
p3 <- ggplot(nutrition_hosp_v,aes(x=N,y=p*100,col=p==0.0)) +
  geom_point() +
  facet_wrap(~group,scale="free_y",nrow=1) +
  geom_hline(data=nutrition_hosp_v[!hospitalid %in% no_nutrition_hospitalid,
                                   (mean(p*100)),by=group],aes(yintercept = V1), color="blue") +
  theme_bw(base_size=14) +
  scale_color_manual(values=c("black","tomato")) +
  theme(legend.position = "none") +
  labs(x="Ventilated ICU admissions",y="Intake via route (%)")
p3
ggsave("graphs/nutrition_by_hosp_ventilated.png",p3)
# apache score
apache_tab <- icustay[!hospitalid %in% no_nutrition_hospitalid,.(Nt=.N),by=.(apache_iv)]
nutrition_apache_iv <- rbind(icustay[enteral==1,.N,by=.(apache_iv)][,.(apache_iv,N,group="Enteral")],
                        icustay[oral==1,.N,by=.(apache_iv)][,.(apache_iv,N,group="Oral")],
                        icustay[parenteral==1,.N,by=.(apache_iv)][,.(apache_iv,N,group="Parenteral")],
                        icustay[ventday1==1 & (parenteral==1 | oral == 1 | enteral == 1),.N,by=.(apache_iv)][,.(apache_iv,N,group="Any nutrition")])
nutrition_apache_iv <- merge(nutrition_apache_iv,apache_tab,by="apache_iv")
nutrition_apache_iv[,p := N/Nt]
p4 <- ggplot(nutrition_apache_iv,aes(x=apache_iv,y=100*p)) +
  geom_point(aes(size=N)) +
  facet_wrap(~group,scale="free_y",nrow=1) +
  theme_bw(base_size=14) +
  labs(x="APACHE IV score",y="Intake via route (%)") +
  theme(legend.position = "none")
p4
ggsave("graphs/nutrition_by_apache.png",p4)
gridExtra::grid.arrange(p1,p2,p3,p4,ncol=1)
p_all <- gridExtra::arrangeGrob(p1+labs(title="(A)"),
                                p2+labs(title="(B)"),
                                p3+labs(title="(C)"),
                                p4+labs(title="(D)"),ncol=1)
ggsave("graphs/enteral_data_qual.png",plot=p_all,height = 10,width = 10)



