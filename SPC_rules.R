
###Apply the rules to the raw data######


#############################DataProcessing#################################################################
rulevector <- c("Rule1", "Rule2", "Rule3", "Rule4", "Rule5", "Rule6", "Rule7", "Rule8")
rulevectorvalue <- c("Rule1value", "Rule2value", "Rule3value", "Rule4value", "Rule5value", "Rule6value", "Rule7value", "Rule8value")
combinedAll[ , rulevector] <- NA
combinedAll[ , rulevectorvalue] <- NA

########################rule1############################
rule1 <- nelsonr1(combinedAll$Value)
for (rowindex in rule1){
  combinedAll[rowindex,14] <- "True" 
}
combinedAll[c("Rule1")][is.na(combinedAll[c("Rule1")])] <- "False"
combinedAll$Rule1value<-ifelse(combinedAll$Rule1=="False",combinedAll$Rule1value <- NA,combinedAll$Rule1value <- combinedAll$Value )