h1_b_data <- read.csv("h1_B_2016.csv")


library(sampling)
library(plotly)
library(stringr)


#############pre-processing########################## 

indx_na_sal <- which(is.na(h1_b_data$PREVAILING_WAGE))

h1_b_data <- h1_b_data[-indx_na_sal,]

indx_0_sal <- which(h1_b_data$PREVAILING_WAGE==0)

h1_b_data <- h1_b_data[-indx_0_sal,]

summ <- summary(h1_b_data$PREVAILING_WAGE)
summ

q1 <-(summ[2]-(1.5 * (summ[5]-summ[2]))) 
q3 <- (summ[5]+(1.5 * (summ[5]-summ[2])))

indx_outliers <- which(h1_b_data$PREVAILING_WAGE<q1 | h1_b_data$PREVAILING_WAGE>=q3)

h1_b_data <- h1_b_data[-indx_outliers,]


##########piechart for categorical##########
plot_ly(h1_b_data[,-ncol(h1_b_data)], labels = ~names(table(h1_b_data$CASE_STATUS)), 
        values = ~table(h1_b_data$CASE_STATUS), type = 'pie',
        textposition = 'inside',textinfo = 'label+percent') %>% layout(title = "Status of Applications for 2016",
                                                                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE,
                                                                                                                                                              zeroline = FALSE, showticklabels = FALSE))


#############boxplot################
new_plot <- h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "CERTIFIED"]
plot_1<- plot_ly(x = ~h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "CERTIFIED"], type = "box" , name = "CERTIFIED") %>% 
  add_trace(x = ~h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "DENIED"], name = "DENIED") %>%
  add_trace(x = ~h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "CERTIFIED-WITHDRAWN"], name = "CERTIFIED-WITHDRAWN") %>%
  add_trace(x = ~h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "WITHDRAWN"], name = "WITHDRAWN") %>% layout(title = "Salary distribution of Groups",
                                                                                          xaxis = list(title = "Prevailing wage", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE,
                                                                                          zeroline = FALSE, showticklabels = FALSE))
plot_1


###########Histograms##############
h1 <- plot_ly(x=h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "CERTIFIED"], type='histogram' ,nbinsx=100, color = "", colors = c("Oranges"), name="Certified")
h2 <- plot_ly(x=h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "DENIED"], type='histogram' ,nbinsx=100, color = "", colors = c("Blues"), name="Denied")
h3 <- plot_ly(x=h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "CERTIFIED-WITHDRAWN"], type='histogram' ,nbinsx=100, color = "", colors = c("Greens"), name="Certified-Withdrawn")
h4 <- plot_ly(x=h1_b_data$PREVAILING_WAGE[h1_b_data$CASE_STATUS == "WITHDRAWN"], type='histogram' ,nbinsx=100, color = "", colors = c("Purples"), name="Withdrawn")
  
s1 <- subplot(nrows=2,h1,h2,h3,h4) %>% layout(title = "Distribution of Salaries ")
s1

###################Histogram Salary##################
hist(h1_b_data$PREVAILING_WAGE,breaks=100, ylim = c(0,40000))

plot_ly(x=h1_b_data$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("Oranges"), name="10")


############Central Limit###############
total <- as.numeric(h1_b_data$PREVAILING_WAGE)


samples <- 10000

mean_size_10 <- numeric(samples)
mean_size_20 <- numeric(samples)
mean_size_30 <- numeric(samples)
mean_size_40 <- numeric(samples)

set.seed(123)

for(i in 1:samples){
  mean_size_10[i]<- mean(sample(total, size=10, replace = T))
  mean_size_20[i]<- mean(sample(total, size=20, replace = T))
  mean_size_30[i]<- mean(sample(total, size=30, replace = T))
  mean_size_40[i]<- mean(sample(total, size=40, replace = T))
}


p1 <- plot_ly(x=mean_size_10, type='histogram' ,nbinsx=100, color = "", colors = c("Oranges"), name="10")

p2 <- plot_ly(x=mean_size_20, type='histogram' ,nbinsx=100, color = "", colors = c("Greens"), name="20") 

p3 <- plot_ly(x=mean_size_30, type='histogram' ,nbinsx=100, color = "", colors = c("Purples"),name="30")

p4 <- plot_ly(x=mean_size_40, type='histogram' ,nbinsx=100, color = "", colors = c("Blues"), name="40")

cat("Population , Mean =",round(mean(total),3), ", SD = ", round(sd(total),3))
cat("Sampe Size 10, Mean =", round(mean(mean_size_10),3), ', SD =',round(sd(total)/(sqrt(10)),3),"\nSampe Size 20, Mean =", round(mean(mean_size_20),3),", SD =",round(sd(total)/(sqrt(20)),3),"\nSampe Size 30, Mean =", round(mean(mean_size_30),3),", SD =",round(sd(total)/(sqrt(30)),3),"\nSampe Size 40, Mean =", round(mean(mean_size_40),3),", SD =",round(sd(total)/(sqrt(40)),3))
subplot(nrows=2,p1,p2,p3,p4) %>% layout(title = "Sampling Distribution ")

############Sampling#############
#Simple random sampling with replacementr(SRSWR)#
set.seed(123)

sampling_wr <- srswr(100, nrow(h1_b_data))

selected_data <- (1:nrow(h1_b_data))[sampling_wr!=0]
selected_data <- rep(selected_data,sampling_wr[sampling_wr!=0])
sample_srswr <- h1_b_data[selected_data,] 

original_data <- plot_ly(x=h1_b_data$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("Greys"), name="Salary")
hist_srswr <- plot_ly(x=sample_srswr$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("Oranges"), name="SRSWR")
hist_srswr

#Simple random sampling without replacementr(SRSWOR)#
set.seed(123)

sampling_swor <- srswor(100, nrow(h1_b_data))
sample_srswor <- h1_b_data[sampling_swor!=0,]

hist_srswor <- plot_ly(x=sample_srswor$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("Blues"), name="SRSWOR")
hist_srswor

#Systematic Sampling #
set.seed(123)

N <- nrow(h1_b_data)
n <- 100
k <- ceiling(N / n)
r <- sample(k, 1)
s <- seq(r, by = k, length = n)

sample_systematic <- h1_b_data[s, ]

hist_systematic <- plot_ly(x=sample_systematic$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("BLUE"), name="Systematic")
hist_systematic

####unequal prob systematic sampling####
#library(sampling)
set.seed(123)

pik <- inclusionprobabilities(h1_b_data$PREVAILING_WAGE, 100)

s <- UPsystematic(pik)

sample.4 <- h1_b_data[s != 0, ]


hist_unpsystematic <- plot_ly(x=sample.4$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("BLUE"), name="Unequal Prob Systematic")
hist_unpsystematic


#################Confidence interval #########3###########
pop.mean <- mean(h1_b_data$PREVAILING_WAGE)
pop.sd <- sd(h1_b_data$PREVAILING_WAGE)
conf <- c(80, 90)
alpha <- 1 - conf/100

cat("Prevailing wage : mean =",pop.mean," and sd =",pop.sd)


for (i in alpha) {
  str_srswr <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                       100*(1-i), i, 
                       pop.mean - qnorm(1-i/2) * pop.sd,
                       pop.mean + qnorm(1-i/2) * pop.sd)
  cat(str_srswr,"\n")
}

#srswr#

sd.sample.means_srswr <- pop.sd/sqrt(nrow(sample_srswr))
xbar_srswr <- mean(sample_srswr$PREVAILING_WAGE)
cat("SRSWR : mean =",xbar_srswr," and sd =",sd.sample.means_srswr)

for (i in alpha) {
  str_srswr <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                       100*(1-i), i, 
                       xbar_srswr - qnorm(1-i/2) * sd.sample.means_srswr,
                       xbar_srswr + qnorm(1-i/2) * sd.sample.means_srswr)
  cat(str_srswr,"\n")
}

#srswor#
sd.sample.means_srswor <- pop.sd/sqrt(nrow(sample_srswor))
xbar_srswor <- mean(sample_srswor$PREVAILING_WAGE)
cat("SRSWOR : mean =",xbar_srswor," and sd =",sd.sample.means_srswor)

for (i in alpha) {
  str_srswor <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                        100*(1-i), i, 
                        xbar_srswor - qnorm(1-i/2) * sd.sample.means_srswor,
                        xbar_srswor + qnorm(1-i/2) * sd.sample.means_srswor)
  cat(str_srswor,"\n")
}


#systematic#

sd.sample.means_systematic <- pop.sd/sqrt(nrow(sample_systematic))
xbar_systematic <- mean(sample_systematic$PREVAILING_WAGE[!is.na(sample_systematic$PREVAILING_WAGE)])
cat("SRSWOR : mean =",xbar_systematic," and sd =",sd.sample.means_systematic)

for (i in alpha) {
  str_systematic <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                            100*(1-i), i, 
                            xbar_systematic - qnorm(1-i/2) * sd.sample.means_systematic,
                            xbar_systematic + qnorm(1-i/2) * sd.sample.means_systematic)
  cat(str_systematic,"\n")
}

#Upsystematic#
sd.sample.means_upsystematic <- pop.sd/sqrt(nrow(sample.4))
xbar_upsystematic <- mean(sample.4$PREVAILING_WAGE[!is.na(sample.4$PREVAILING_WAGE)])
cat("UPSystematic : mean =",xbar_upsystematic," and sd =",sd.sample.means_upsystematic)

for (i in alpha) {
  str_upsystematic <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                              100*(1-i), i, 
                              xbar_upsystematic - qnorm(1-i/2) * sd.sample.means_upsystematic,
                              xbar_upsystematic + qnorm(1-i/2) * sd.sample.means_upsystematic)
  cat(str_upsystematic,"\n")
}


################On demand################
var2 <- numeric(1)
var <- names(tail(sort(table(h1_b_data$JOB_TITLE))))

for (i in 1:length(var)) {
  indx <- which(is.na(h1_b_data$PREVAILING_WAGE[as.character(h1_b_data$JOB_TITLE) == var[i]]))
  
  var2[i] <- mean((h1_b_data$PREVAILING_WAGE[as.character(h1_b_data$JOB_TITLE) == var[i]])[-indx])
}


bar_job_vs_freq <- plot_ly(x = ~var, y = ~var2, color = ~var, type = "bar" ) %>% layout(title = "Popular Jobs with average Income per annum", xaxis = list(title = ""), yaxis = list(title = "Salary"))
bar_job_vs_freq


###################################State wise###############################################
library(stringr)
states <- data.frame(State= as.character(str_trim(sapply(h1_b_data$WORKSITE, function(x){
  temp <- str_split(x, ",")
  sapply(temp, function(y){y[2]
  })
}))))


h1_b_data$state <- states

state_job_data <- h1_b_data[h1_b_data$state != "NA",c("state","JOB_TITLE")]


p <- plot_ly(x = ~names(table(h1_b_data$state[h1_b_data$state != "NA"])), y = ~table(h1_b_data$state[h1_b_data$state != "NA"]), color = ~names(table(h1_b_data$state[h1_b_data$state != "NA"])), type = "bar")
p
