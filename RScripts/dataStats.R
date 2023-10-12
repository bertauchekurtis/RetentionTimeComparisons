training <- read.csv(file = "../data/trainingSet_withVars_DATA_TWO.csv")
testing <- read.csv(file = "../data/testingSet_withVars_DATA_TWO.csv")

total <- rbind(testing, training)
phos <- total[which(total$modS > 0 | total$modT > 0 | total$modY > 0),]
ox <- total[which(total$modM > 0),]


