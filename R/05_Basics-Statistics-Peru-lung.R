#08.06.2023: Peru Lung
setwd("~/BSAPR Assessment")

library(tidyverse)
data_unchanged <- read_csv("data/raw/perulung_ems.csv")
str(data_unchanged)

data <- data_unchanged |> mutate(id=as.integer(id), 
                      sex = factor(sex, levels=c(0,1), labels=c("f","m")), 
                      respsymptoms=factor(respsymptoms, levels=c(0,1), labels=c("no","yes")), 
                      asthma_hist=factor(asthma_hist))
data

ggplot(data, aes(x=fev1)) + 
  geom_histogram(bins=40) #Defintion Bin: A histogram displays numerical data by
                          #grouping data into "bins" of equal width. Each bin is 
                          #plotted as a bar whose height corresponds to how many
                          #data points are in that bin.

toy_dat<-data.frame(x1=1:11)
breaks<-seq(from=0.5,to=12.5,by=2) #Starts at 0.5 and goes up to 12.5
ggplot(toy_dat, aes(x=x1)) + 
  geom_histogram(breaks=breaks) #Trade off

ggplot(data, aes(x=fev1)) + 
  geom_histogram(aes(y=after_stat(density)))

summary(data) #An easy way to obtain some important descriptive statistics on your variables.
#Note: There are no missing values in this data set. These would appear in the lowest row as NA: number of missings.

#What proportion of values are less or equal to a given value t?
#The empirical cumulative distribution function (ECDF):

ggplot(data, aes(fev1)) +  stat_ecdf() #What proportion if children have a fev1 kess than 1.5 -> you can find
                                      #this out with this graph
#We can use ECDF to obtain quantiles
quantile(data$fev1, c(0.25,0.5,0.75))

ggplot(data, aes(y=fev1, color=sex)) + 
  geom_boxplot() +
  scale_x_discrete()

library(gtsummary)
tabl_summary <- data |>   
  select(where(is.double)) |>
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", 
                                     "{median} ({p25}, {p75})", 
                                     "{min}, {max}")
  )
show(tabl_summary)

#FEV1 by age and sex
data$age_cat<-cut(data$age,seq(6,12,2), right=FALSE)
tbl_fev1 <- data  %>% 
  group_by(sex, age_cat) %>%
  summarise(n=n(), mean=mean(fev1), sd=sd(fev1))
tbl_fev1

ggplot(data, aes(x=age_cat, y=fev1, fill=sex)) + 
  geom_boxplot() + 
  xlab("Age group") + 
  ylab("FEV1") +
  facet_wrap(~sex)

tbl_fev1$SE <-tbl_fev1$sd/sqrt(tbl_fev1$n)
tbl_fev1$CI_norm_lb<-tbl_fev1$mean - qnorm(0.975)*tbl_fev1$SE
tbl_fev1$CI_norm_ub<-tbl_fev1$mean + qnorm(0.975)*tbl_fev1$SE
print(tbl_fev1, digits=3)


#Differences in mean
comp_mean<-tbl_fev1 %>% 
  filter(age_cat == "[8,10)") %>%
  select(mean, SE, CI_norm_lb, CI_norm_ub)
print(comp_mean,digits=3)

D<-comp_mean$mean[comp_mean$sex=="m"]-comp_mean$mean[comp_mean$sex=="f"]
print(D, digits=4)

D = comp_mean$mean[comp_mean$sex=="m"]-comp_mean$mean[comp_mean$sex=="f"]
SE_D<-sqrt(sum(comp_mean$SE^2))
CI_lb<-D-qnorm(0.975)*SE_D
CI_ub<-D+qnorm(0.975)*SE_D
sprintf("D =  %1.3f, SE_D = %1.3f, 95%%-CI: [%1.3f, %1.3f]", D, SE_D, CI_lb, CI_ub)

#p-value
D = comp_mean$mean[comp_mean$sex=="m"]-comp_mean$mean[comp_mean$sex=="f"]
SE_D<-sqrt(sum(comp_mean$SE^2))
z<-D/sqrt(sum(comp_mean$SE^2))
p<-2*pnorm(-abs(z))
sprintf("D =  %1.3f, SE_D = %1.3f, z_D = %1.3f, P = %1.3e", D, SE_D, z, p)

#t-test
tbl_fev1$SE <-tbl_fev1$sd/sqrt(tbl_fev1$n)
tbl_fev1$CI_t_lb<-tbl_fev1$mean - qt(0.975,tbl_fev1$n-1)*tbl_fev1$SE
tbl_fev1$CI_t_ub<-tbl_fev1$mean + qt(0.975,tbl_fev1$n-1)*tbl_fev1$SE
print(tbl_fev1, digits=3)

#Two-sample t-test
ind<-data$age_cat=="[8,10)"
t.test(fev1~sex, data=data, subset=ind)

ind<-data$age_cat=="[6,8)"
t.test(fev1~sex, data=data, subset=ind)

#One-sample t-test
ind<-data$age_cat=="[6,8)" & data$sex=="f"
t.test(data$fev1[ind], mu=1.4)

#FEV1 by asthma history
ggplot(data, aes(x=asthma_hist, y=fev1)) + 
  geom_boxplot() + 
  ylab("FEV1") + xlab("Asthma history") +
  facet_wrap(~sex)

#Sample meany by groups
dat<-data %>% filter(sex=="f")
tbl2_fev1 <- dat %>% group_by(asthma_hist) %>% summarise(n=n(), mean=mean(fev1), sd=sd(fev1))
tbl2_fev1 

#Overall means
total <- dat %>% summarise(n=n(), mean=mean(fev1), sd=sd(fev1))
total