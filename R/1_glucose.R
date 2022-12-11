library(data.table)
library(ggplot2)
library(scales)

## data ## ---------------------------------------------------------------------
icustay <- fread("data/raw/icustay.csv")
pt_info <- fread("data/raw/diabetes.csv")
glucose_lactate <- fread("data/raw/glucose_lactate.csv")
glucose_lactate <- merge(glucose_lactate,icustay,by="patientunitstayid")
glucose_lactate <- merge(glucose_lactate,pt_info,by="patientunitstayid")

# vars
glucose_lactate[,dy := time / 60 / 24]
glucose_lactate[,dy1 := round(dy/0.5)*0.5]
glucose_lactate[,dy25 := round(dy/0.25)*0.25]
glucose_lactate[,dy0 := round(dy)]
glucose_lactate[,glucose := fcoalesce(glucose_bedside,glucose_lab)]
glucose_lactate[!is.na(glucose),measure := fifelse(is.na(glucose_bedside),"lab","bedside")]

## glucose distributions -------------------------------------------------------

# samples
set.seed(1234)
n_per_patient <- glucose_lactate[!is.na(glucose) & time > 0,
                                 .(.N,icu_los_hours=mean(icu_los_hours)),by=patientunitstayid]
samp10 <- sample(n_per_patient[N > 8 & icu_los_hours > 24 & icu_los_hours < 24*2,
                               patientunitstayid],10)
glucose_lactate_samp <- glucose_lactate[patientunitstayid %in% samp10]
p1 <- ggplot(glucose_lactate_samp[!is.na(glucose)],aes(x=time/60,y=glucose)) +
  geom_smooth(se=FALSE,col="gray50",size=0.5,span = 0.7) +
  geom_point(aes(col=measure)) +
  scale_y_log10(n.breaks=6) +
  scale_color_manual(values=c("black","deepskyblue")) +
  scale_x_continuous(limits=c(0,48.0)) +
  facet_wrap(~patientunitstayid,ncol=2) +
  annotate(geom = "rect",
           fill="pink",
           xmin = 0,
           xmax = 48,
           ymin = 100,
           ymax = 180,
           alpha = 0.25) +
  theme_bw(base_size=14) +
  theme(legend.position = "none") +
  labs(y = "Blood glucose (mg/dL)",x = "Time since ICU admission (hours)")
p1
ggsave("graphs/glucose_sample.png",p1)
p23 <- ggplot(glucose_lactate,
       aes(x=glucose,fill=measure)) +
  scale_x_log10(limits=c(30,1000)) +
  geom_histogram(col="white") +
  labs(x = "Blood glucose (mmol/L)",y="Count") +
  theme_bw(base_size=14)
p23
ggsave("graphs/glucose_bed_lab_dist.png",p23)
p2 <- ggplot(glucose_lactate,
       aes(x=glucose_bedside)) +
  scale_x_log10(limits=c(30,1000)) +
  scale_y_continuous(labels = label_comma()) +
  geom_histogram(col="white",fill="lightblue") +
  labs(x = "Blood glucose (mmol/L)",y="Count") +
  theme_bw(base_size=14)
p2
ggsave("graphs/glucose_bed_dist.png",p2)
p3 <- ggplot(glucose_lactate,
       aes(x=glucose_lab)) +
  scale_x_log10(limits=c(30,1000)) +
  scale_y_continuous(labels = label_comma()) +
  geom_histogram(col="white",fill="lightblue") +
  labs(x = "Blood glucose (mmol/L)",y="Count") +
  theme_bw(base_size=14)
p3
ggsave("graphs/glucose_lab_dist.png",p3)
# time between bedside
glucose_lactate[!is.na(glucose_bedside),time_diff := (shift(time,-1) - time)/60,by=patientunitstayid]
p4 <- ggplot(glucose_lactate[time_diff < 24],aes(x=time_diff)) +
  geom_histogram(binwidth=0.5,fill="lightblue",col="white") +
  scale_y_continuous(labels = label_comma()) +
  theme_bw(base_size=14) +
  labs(x="Time between measurements (hours)",y="Count") +
  coord_cartesian(xlim=c(0,16))
p4
ggsave("graphs/glucose_time_measure.png",p4)
# correlation between measures
glucose_lactate[,time_diff_all := (shift(time,-1) - time)/60,by=patientunitstayid]
glucose_lactate[,time_diff_05 := round(time_diff_all/0.5)*0.5,]
glucose_lactate[,glucose_next := shift(glucose,-1),by=patientunitstayid]
glucose_corr <- glucose_lactate[time_diff < 16,.(corr = cor(glucose,glucose_next,use = "pairwise.complete.obs")),by=time_diff_05]
p5 <- ggplot(glucose_corr,aes(x=time_diff_05,y=corr)) +
  geom_smooth(se=FALSE) +
  geom_point() +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  labs(x="Time between measurements (hours)",y="Correlation (Pearson)") +
  theme_bw(base_size=14) +
  coord_cartesian(ylim=c(0,1))
p5
ggsave("graphs/glucose_corr_time.png",p5)
# population dist over time
q10 <- seq(0.05, 0.95, by = 0.05)
qtab <- glucose_lactate[ ,lapply(.SD, quantile, prob = q10, na.rm = TRUE),.SDcols = c("glucose"),by=dy25]
qtab[,quantile := rep(q10,nrow(qtab)/length(q10))]
#qtab <- melt(qtab,id.vars = c("dy25","quantile"))
qtab[,low_high := rep(c(rep("lower",9),"median",rep("higher",9)),nrow(qtab)/length(q10))]
qtab[,group := rep(c(letters[1:9],"median",rev(letters[1:9])),nrow(qtab)/length(q10))]
qtab_lh <- tidyr::pivot_wider(qtab[low_high!="median",.(dy25,low_high,group,glucose)],names_from = "low_high",values_from="glucose")
setDT(qtab_lh)
qtab_m <- qtab[low_high=="median"]
p6 <- ggplot() +
  geom_ribbon(data=qtab_lh,aes(x=dy25,ymin=lower,ymax=higher,group=group),
              alpha=0.2) +
  geom_line(data=qtab_m,aes(x=dy25,y=glucose)) +
  scale_x_continuous(limits=c(0,5)) +
  scale_y_log10(limits=c(70,500),n.breaks=7) +
  labs(x = "Time since ICU admission (days)",y="Blood glucose (mmol/L)") +
  theme_bw(base_size=14)
p6
ggsave("graphs/glucose_pop_time.png",p6)

# admission glucose and hospital outcome
glucose_lactate[time > 0,min_time := min(time),by=patientunitstayid]
glucose_lactate[time == min_time]
cuts <- quantile(glucose_lactate$glucose,probs=q10,na.rm=TRUE)
cuts <- unique(round(cuts,-1))
glucose_lactate[,glucose_q := cut(glucose,c(0,cuts,Inf),ordered=TRUE)]
hosp_mort_gluc <- glucose_lactate[time == min_time & !is.na(glucose_q),
                                  .(hosp_mort=mean(hosp_mort,na.rm=TRUE),.N),by=.(glucose_q,diabetes)]

levels(hosp_mort_gluc$glucose_q) <- stringr::str_replace(levels(hosp_mort_gluc$glucose_q),",","-")
levels(hosp_mort_gluc$glucose_q) <- stringr::str_replace(levels(hosp_mort_gluc$glucose_q),"\\(","")
levels(hosp_mort_gluc$glucose_q) <- stringr::str_replace(levels(hosp_mort_gluc$glucose_q),"\\]","")
levels(hosp_mort_gluc$glucose_q) <- stringr::str_replace(levels(hosp_mort_gluc$glucose_q),"-Inf","+")
hosp_mort_gluc[,gluc := as.numeric(glucose_q)]
hosp_mort_gluc[,se := sqrt(hosp_mort*(1-hosp_mort)/N)]
p7 <- ggplot(hosp_mort_gluc,aes(x=glucose_q,y=100*hosp_mort,col=factor(diabetes))) +
  geom_pointrange(aes(ymin=100*(hosp_mort-2*se),ymax=100*(hosp_mort+2*se))) +
  geom_line(aes(x=gluc,alpha=0.2)) +
  labs(x = "Blood glucose (mg/dL)",y = "Hospital mortality (%)") +
  scale_color_discrete(name="",labels=c("Non-diabetic","Diabetic")) +
  scale_y_continuous(n.breaks=12,limits=c(0,22)) +
  guides(alpha="none")+
  theme_bw(base_size=14)+
  theme(axis.text.x = element_text(angle=-45,hjust = 0))
p7
ggsave("graphs/glucose_hos_mort.png",p7)

layout_matrix <- matrix(c(1,1,1,6,7,7,
                          2,3,4,5,7,7),ncol=2)
gridExtra::grid.arrange(p1+labs(title="(A)"),
                        p2+labs(title="(B)"),
                        p3+labs(title="(C)"),
                        p4+labs(title="(D)"),
                        p5+labs(title="(F)"),
                        p6+labs(title="(E)"),
                        p7+labs(title="(G)"),
                        layout_matrix=layout_matrix)
p_all <- gridExtra::arrangeGrob(p1+labs(title="(A)"),
                                p2+labs(title="(B)"),
                                p3+labs(title="(C)"),
                                p4+labs(title="(D)"),
                                p5+labs(title="(F)"),
                                p6+labs(title="(E)"),
                                p7+labs(title="(G)"),
                                layout_matrix=layout_matrix)
ggsave("graphs/glucose.png",p_all,width = 10,height = 12)


## glucose quality check -------------------------------------------------------

# glucose per patient
length(unique(glucose_lactate$patientunitstayid))
length(unique(glucose_lactate[measure=="bedside"]$patientunitstayid))
length(unique(glucose_lactate[measure=="lab"]$patientunitstayid))
# hospitals with bedside

# bedside per patient per day
glucose_lactate[,bedside := fifelse(measure=="bedside",1,0)]
tab_pt_dy <- glucose_lactate[,.(bedside=sum(bedside,na.rm=TRUE)),by=.(patientunitstayid,dy0)]
tab_pt_dy <- tab_pt_dy[bedside > 0,.(mean = mean(bedside)),by=dy0]












# 
# geom_density_ridges(aes(fill=Month),alpha=0.5) +
#   scale_y_discrete(limits=rev) +
#   labs(x="Retail gasoline price ($/gallon)",y="Counts")