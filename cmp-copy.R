###########################################################################
#script to generate left-right variable, as per
#Lowe et al. 2011. “Scaling Policy Preferences from Coded Political Texts.”
#Legislative Studies Quarterly 36(1): 123–55
###########################################################################

library(manifestoR)


# load cmp data
mp_setapikey("manifesto_apikey.txt")
data <- mp_maindataset()

data$progressive <- data$per103 + data$per105 + data$per106 + data$per107 + data$per108 + data$per202 + data$per416 + data$per501 + data$per502 + data$per602 + data$per604 + data$per606 +  data$per705
data$conservative <- data$per104 + data$per109 + data$per110 + data$per201 + data$per305 + data$per601 + data$per603 + data$per605 + data$per606 + data$per608
data$left <- data$per403 + data$per404 + data$per405 + data$per406 + data$per409 + data$per412 + data$per413 + data$per415 + data$per503 + data$per504 + data$per701
data$right <- data$per401 + data$per402 + data$per407 + data$per410 + data$per414 + data$per505  + data$per702

data$pc <- log(data$progressive + .5) - log(data$conservative + .5)
data$lr <- log(data$left + .5) - log(data$right + .5)

cmp.data <- data
cmp.data$edate <-  as.Date(cmp.data$edate)
