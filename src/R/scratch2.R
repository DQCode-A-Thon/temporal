library("data.table")
library("lubridate")
library("magrittr")
source("src/R/sharedFuns.R")


# Read in the data. 
dat <- fread("data/dqcdm-temporal-summary/dqcdm_temporal_summary_subset_2.txt")

# Light munging. 
dat[ , prevalence := as.double(prevalence)]
dat[ , time_period := paste0(time_period, "01")]
dat[ , time_period := ymd(time_period)]
dat[ , month := month(time_period)]
dat[ , year := year(time_period)]



# Subset for test purposes. 
dat <- subset(dat, source_name=="JMDC" & concept_id==312664)

# Compute average year. 
avg_year <- summarySE(dat, measurevar="prevalence", groupvars="month", na.rm=TRUE)
avg_year[ , upper := prevalence + se]
avg_year[ , lower := prevalence - se]


# Compute control data: this is defined as the average year excluding the current
# year. 
control_dat <- data.table()
for (yr in unique(dat$year)){
  subset_dat <- subset(dat, year != yr)
  cdat <- summarySE(subset_dat, measurevar="prevalence", groupvars="month", na.rm=TRUE)
  cdat[ , control_year := yr]
  cdat[ , time_period := ymd(paste0(control_year, sprintf("%02d", month), "01"))]
  control_dat <- rbindlist(list(control_dat, cdat), use.names=TRUE, fill=TRUE)
}
# Rename variables.
control_dat[ , upper := prevalence + se]
control_dat[ , lower := prevalence - se]
setnames(control_dat, "prevalence", "control_prevalence")
setnames(control_dat, "control_year", "year")
setnames(control_dat, "N", "num_data_points")
control_dat <- subset(control_dat, select=c("month", "year", "time_period",
                                            "control_prevalence", 
                                            "num_data_points", "se",
                                            "upper", "lower"))
control_dat <-merge(dat, control_dat, all=TRUE, by=c("year", "month", "time_period"))
# Fill in NAs. 
control_dat[ , source_name := dat[ , unique(source_name)]]
control_dat[ , concept_name := dat[ , unique(concept_name)]]

# Compute flags on control data. 
control_dat[ , flag := prevalence > upper | prevalence < lower]
control_dat[is.na(flag), flag := FALSE]
control_dat[ , flag_color := ifelse(flag, "red", "black")]
control_dat[ , flag_binary := ifelse(flag, 1, 0)]


# Test case. 
# dat_no_2009 <- subset(dat, year != 2009)
# cdat_no_2009 <- summarySE(dat_no_2009, measurevar="prevalence", groupvars="month", na.rm=TRUE)
# print(cdat_no_2009)
# print(control_dat[year == 2009])

# Plot: Overview. 
ggplot(control_dat, aes(x=time_period, y=control_prevalence)) + 
  geom_line(color="darkgray") +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) + 
  geom_point(aes(x=time_period, y=prevalence, color=flag_color), size=5) + 
  scale_color_manual(values=c("black", "red"), labels=c("Normal", "Anomaly")) +
  geom_line(aes(x=time_period, y=prevalence)) + 
  facet_wrap(~ year, scale="free_x", nrow=1) + 
  theme(axis.text.x=element_text(angle=90)) + 
  ylab("Prevalence") + 
  xlab("Time") + 
  theme_alex

# Plot: Annual Average. 
ggplot(avg_year, aes(month, prevalence)) + 
  geom_line(size=2) + 
  geom_point(size=2) + 
  geom_line(aes(month, upper), linetype=9) + 
  geom_line(aes(month, lower), linetype=9) + 
  labs(title=paste("Average Year (mean of prevalence by month) for:", 
                   dat$concept_name[1], sep="\n")) +
  xlab("Month") + 
  ylab("Prevalence") + 
  scale_x_continuous(limits=c(1, 12), breaks=1:12) +
  theme_alex



# Table. 
table_subset <- subset(control_dat, flag==TRUE, select=c("year", "month", "source_name", 
                                                           "concept_name", "prevalence", 
                                                           "control_prevalence", "se"))
table_subset[ , dev := (prevalence - control_prevalence)^2 / se]
table_subset <- table_subset[(order(-dev))]
table_subset


