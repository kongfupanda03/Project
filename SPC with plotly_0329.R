pacman::p_load(tidyverse, rebus,IRdisplay,data.table, 
               stringr, lubridate, editrules,readxl, plotly)

####generate subset###
dataset1 <- read_csv('cleaned_ds_0402.csv')
dataset = subset(dataset1, dataset1$Process == 'Assembly1' & dataset1$TestItem =='TORQUE'& dataset1$Station =='NUT1' )

avg <- mean(dataset$TestValue)
sig3 <- 3*sd(dataset$TestValue)
sig2 <- 2*sd(dataset$TestValue)
sig <- sd(dataset$TestValue)

###### rule1 ######
# Nelson's QC rule 1: detect values outside + or -3 sd
nelsonr1 <- function(x, m = mean(x), s = sd(x)) {
  which(abs((x - m) / s) >= 3)
}

rule1 <- nelsonr1(dataset$TestValue)
for (rowindex in rule1){
  dataset[rowindex,'Rule1'] <- "Violation" 
}
dataset[c("Rule1")][is.na(dataset[c("Rule1")])] <- "Good"


p1<- gplot(dataset,aes(x=TimeStamp,y=TestValue))+
  geom_hline(yintercept = c(sig3 +avg,-sig3 +avg),colour='red')+
  geom_hline(yintercept = c(sig2+avg,sig+avg,-sig+avg,-sig2+avg), linetype='dashed')+
  geom_hline(yintercept = avg,colour='green')+
  geom_point(aes(colour=Rule1))+geom_line(aes(colour= 'grey'))+
  scale_colour_manual(values = c("Violation" = "red", "Good" = "blue"))
   
ggplotly(p1)


###### rule2 ######
# # Nelson's QC rule 2: detect runs of >= 9 points on the same side of the mean
nelsonr2 <- function(x, m = mean(x), minrun = 9) {
  n <- length(x)
  counts <- sign(x - m)
  result <- counts
  for (runlength in 2:minrun)
    result <- result + c(counts[runlength:n], rep(0, runlength - 1))
  which(abs(result) >= minrun)
}

rule2 <- nelsonr2(dataset$TestValue)
for (rowindex in rule2){
  dataset[rowindex,'Rule2'] <- "Violation" 
}
dataset[c("Rule2")][is.na(dataset[c("Rule2")])] <- "Good"

p2 <- ggplot(dataset,aes(x=TimeStamp,y=TestValue))+
  geom_hline(yintercept = c(sig3 +avg,-sig3 +avg),colour='red')+
  geom_hline(yintercept = c(sig2+avg,sig+avg,-sig+avg,-sig2+avg), linetype='dashed')+
  geom_hline(yintercept = avg,colour='green')+
  geom_point(aes(colour=Rule2))+geom_line(aes(colour= 'grey'))+
  scale_colour_manual(values = c("Violation" = "red", "Good" = "blue"))
  

ggplotly(p2)

#####Rule3#####
#Nelson's QC rule 3: detect strict increase or decrease in >= 6 points in a row
# Between 6 points you have 5 instances of increasing or decreasing. Therefore minrun - 1.
nelsonr3 <- function(x, minrun = 6) {
  n <- length(x)
  signs <- sign(c(x[-1], x[n]) - x)
  counts <- signs
  for (rl in 2:(minrun - 1)) {
    counts <- counts + c(signs[rl:n], rep(0, rl - 1))
  }
  which(abs(counts) >= minrun - 1)
}

rule3 <- nelsonr3(dataset$TestValue)
for (rowindex in rule3){
  dataset[rowindex,'Rule3'] <- "Violation" 
}
dataset[c("Rule3")][is.na(dataset[c("Rule3")])] <- "Good"

p3 <- gplot(dataset,aes(x=TimeStamp,y=TestValue))+
  geom_hline(yintercept = c(sig3 +avg,-sig3 +avg),colour='red')+
  geom_hline(yintercept = c(sig2+avg,sig+avg,-sig+avg,-sig2+avg), linetype='dashed')+
  geom_hline(yintercept = avg,colour='green')+
  geom_point(aes(colour=Rule3))+geom_line(aes(colour= 'grey'))+
  scale_colour_manual(values = c("Violation" = "red", "Good" = "blue"))
  

ggplotly(p3)

#####Rule 4########
# Nelson's QC rule 4: 14 points in a row alternating in direction from the mean,
# or 14 points in a row alternating in increase and decrease
nelsonr4 <- function(x, m = mean(x), minrun = 14, directing_from_mean = FALSE) {
  n <- length(x)
  if (directing_from_mean == TRUE) {
    signs <- sign(x - m)
  } else {
    signs <- sign(c(x[-1],x[n]) - x)
  }
  counts <- signs
  fac <- -1
  for (rl in 2:minrun) {
    counts <- counts + fac * c(signs[rl:n], rep(0, rl - 1))
    fac <- -fac
  }
  counts <- abs(counts)
  which(counts >= minrun)
}

rule4 <- nelsonr4(dataset$TestValue)
for (rowindex in rule4){
  dataset[rowindex,'Rule4'] <- "Violation" 
}
dataset[c("Rule4")][is.na(dataset[c("Rule4")])] <- "Good"

p4 <- gplot(dataset,aes(x=TimeStamp,y=TestValue))+
  geom_hline(yintercept = c(sig3 +avg,-sig3 +avg),colour='red')+
  geom_hline(yintercept = c(sig2+avg,sig+avg,-sig+avg,-sig2+avg), linetype='dashed')+
  geom_hline(yintercept = avg,colour='green')+
  geom_point(aes(colour=Rule4))+geom_line(aes(colour= 'grey'))+
  scale_colour_manual(values = c("Violation" = "red", "Good" = "blue"))
  

ggplotly(p4)

#######Rule 5: two out of 3 >2 sd from mean in the same direction#######
nelsonr5 <- function(x, m = mean(x), s = sd(x), minrun = 3) {
  n <- length(x)
  pos <- 1 * ((x - m) / s > 2)
  neg <- 1 * ((x - m) / s < -2)
  poscounts <- pos
  negcounts <- neg
  for (rl in 2:minrun) {
    poscounts <- poscounts + c(pos[rl:n], rep(0, rl - 1))
    negcounts <- negcounts + c(neg[rl:n], rep(0, rl - 1))
  }
  counts <- apply(cbind(poscounts, negcounts), 1, max)
  which(counts >= minrun -1)
}
rule5 <- nelsonr5(dataset$TestValue)
for (rowindex in rule5){
  dataset[rowindex,'Rule5'] <- "Violation" 
}
dataset[c("Rule5")][is.na(dataset[c("Rule5")])] <- "Good"

p5 <- gplot(dataset,aes(x=TimeStamp,y=TestValue))+
  geom_hline(yintercept = c(sig3 +avg,-sig3 +avg),colour='red')+
  geom_hline(yintercept = c(sig2+avg,sig+avg,-sig+avg,-sig2+avg), linetype='dashed')+
  geom_hline(yintercept = avg,colour='green')+
  geom_point(aes(colour=Rule5))+geom_line(aes(colour= 'grey'))+
  scale_colour_manual(values = c("Violation" = "red", "Good" = "blue"))


ggplotly(p5)

#######Rule 6######
# Nelson's QC rule 6: four out of five > 1 sd from mean in the same direction

nelsonr6 <- function(x, m = mean(x), s = sd(x), minrun = 5) {
  n <- length(x)
  pos <- 1 * ((x - m) / s > 1)
  neg <- 1 * ((x - m) / s < -1)
  poscounts <- pos
  negcounts <- neg
  for (rl in 2:minrun) {
    poscounts <- poscounts + c(pos[rl:n], rep(0, rl - 1))
    negcounts <- negcounts + c(neg[rl:n], rep(0, rl - 1))
  }
  counts <- apply(cbind(poscounts, negcounts), 1, max)
  which(counts >= minrun - 1)
}

rule6 <- nelsonr6(dataset$TestValue)
for (rowindex in rule6){
  dataset[rowindex,'Rule6'] <- "Violation" 
}
dataset[c("Rule6")][is.na(dataset[c("Rule6")])] <- "Good"

p6 <- gplot(dataset,aes(x=TimeStamp,y=TestValue))+
  geom_hline(yintercept = c(sig3 +avg,-sig3 +avg),colour='red')+
  geom_hline(yintercept = c(sig2+avg,sig+avg,-sig+avg,-sig2+avg), linetype='dashed')+
  geom_hline(yintercept = avg,colour='green')+
  geom_point(aes(colour=Rule6))+geom_line(aes(colour= 'grey'))+
  scale_colour_manual(values = c("Violation" = "red", "Good" = "blue"))

ggplotly(p6)

######Rule 7######
# Nelson's QC rule 7: >= 15 points in a row within 1 sd from the mean

nelsonr7 <- function(x, m = mean(x), s = sd(x), minrun = 15) {
  n <- length(x)
  within <- 1 * (abs((x - m) / s) < 1)
  counts <- within
  for (rl in 2:minrun)
    counts <- counts + c(within[rl:n], rep(0, rl - 1))
  which(counts >= minrun)
}
rule7 <- nelsonr7(dataset$TestValue)
for (rowindex in rule7){
  dataset[rowindex,'Rule7'] <- "Violation" 
}
dataset[c("Rule7")][is.na(dataset[c("Rule7")])] <- "Good"

p7 <- gplot(dataset,aes(x=TimeStamp,y=TestValue))+
  geom_hline(yintercept = c(sig3 +avg,-sig3 +avg),colour='red')+
  geom_hline(yintercept = c(sig2+avg,sig+avg,-sig+avg,-sig2+avg), linetype='dashed')+
  geom_hline(yintercept = avg,colour='green')+
  geom_point(aes(colour=Rule7))+geom_line(aes(colour= 'grey'))+
  scale_colour_manual(values = c("Violation" = "red", "Good" = "blue"))

ggplotly(p7)

#####Rule 8####
# Nelson's QC rule 8: >= 8 points in a row all outside the m + -1s range

nelsonr8 <- function(x, m = mean(x), s = sd(x), minrun = 8) {
  n <- length(x)
  outofrange <- 1 * (abs((x - m) / s) > 1)
  counts <- outofrange
  for (rl in 2:minrun)
    counts <- counts + c(outofrange[rl:n], rep(0, rl - 1))
  which(counts >= minrun)
}
rule8 <- nelsonr8(dataset$TestValue)
for (rowindex in rule8){
  dataset[rowindex,'Rule8'] <- "Violation" 
}
dataset[c("Rule8")][is.na(dataset[c("Rule8")])] <- "Good"

p8 <- gplot(dataset,aes(x=TimeStamp,y=TestValue))+
  geom_hline(yintercept = c(sig3 +avg,-sig3 +avg),colour='red')+
  geom_hline(yintercept = c(sig2+avg,sig+avg,-sig+avg,-sig2+avg), linetype='dashed')+
  geom_hline(yintercept = avg,colour='green')+
  geom_point(aes(colour=Rule8))+geom_line(aes(colour= 'grey'))+
  scale_colour_manual(values = c("Violation" = "red", "Good" = "blue"))

ggplotly(p8)

