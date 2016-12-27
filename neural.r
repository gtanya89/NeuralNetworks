source(file="util/connect.r")
source(file="util/common.r")

DATA <- HIGHWAY_DATA_NORM
CONFIG <- HIGHWAY_CONFIG_NORM

require(gtools)

# I10 west
i10Links <- ore.pull(CONFIG[CONFIG$ONSTREET == 'I-10' & CONFIG$DIRECTION == 3, ])

getInterestingLinks <- function(data, links){
	iL <- c()
	for(i in c(1:nrow(links))){
		rows <- nrow(fromSensor(data, links[i,]$LINK_ID))
	  if(rows > 5000){
	    iL[i] <- links[i,]$LINK_ID

	    print(links[i,]$LINK_ID)
	    print(rows)
	  }
	}
	return(iL)
}

interestedLinks <- getInterestingLinks(DATA, i10Links)

SDATA <- fromSensors(DATA, interestedLinks)
SDATA <- merge(SDATA, HIGHWAY_CONFIG_NORM[c("LINK_ID", "POSTMILE")], by.x="LINK_ID", by.y="LINK_ID")
SDATA <- within(SDATA,{
	NDAY_OF_WEEK  <- as.numeric(as.ore.character(SDATA$DT, format = "D")) / 7
	DAY_OF_MONTH  <- ore.mday(SDATA$DT)
	NTIME_OF_DAY  <- ore.hour(SDATA$DT) * 12 + ore.minute(SDATA$DT) / ( 24 * 12 )
	NSP <- SDATA$SP / max(SDATA$SP)
	MONTH <- ore.month(SDATA$DT)
})

SDATA.train <- SDATA[ SDATA$DAY_OF_MONTH <= 28 & SDATA$MONTH == 4 | SDATA$DAY_OF_MONTH <= 21 & SDATA$MONTH == 5, ]
SDATA.test <- SDATA[ SDATA$DAY_OF_MONTH > 21 & SDATA$DAY_OF_MONTH <= 28 & SDATA$MONTH == 5, ]

setGrowthParams <- function(d){
	d$GROWTH_2 <- 0
	d$GROWTH_4 <- 0
	res <- ore.groupApply(d, INDEX = d$LINK_ID, parallel = TRUE, function(D){
		ordered <- D[ order(D$DT), ]
		for(i in c(5:nrow(ordered))){
			ordered[i,]$GROWTH_2 <- ordered[i - 1,]$SP - ordered[i - 2,]$SP
			ordered[i,]$GROWTH_4 <- ( (ordered[i - 1,]$SP + ordered[i - 2,]$SP) / 2 ) - ( (ordered[i - 3,]$SP + ordered[i - 4,]$SP) / 2 )
		}
		
		ordered[i,]$GROWTH_2 <- ( ordered[i,]$GROWTH_2 - min(ordered[i,]$GROWTH_2) ) / ( max(ordered[i,]$GROWTH_2) - min(ordered[i,]$GROWTH_2) )
		ordered[i,]$GROWTH_4 <- ( ordered[i,]$GROWTH_4 - min(ordered[i,]$GROWTH_4) ) / ( max(ordered[i,]$GROWTH_4) - min(ordered[i,]$GROWTH_4) )

		return(ordered)
	})
	return(res)
}

SDATA.TrNorm <- ore.pull(setGrowthParams(SDATA.train))
SDATA.TtNorm  <- ore.pull(setGrowthParams(SDATA.test))

training <- data.frame()
testing  <- data.frame()

for(i in c(1:length(interestedLinks))){
	sensor <- as.character(interestedLinks[i])

	training <- rbind(training, SDATA.TrNorm[[sensor]])
	testing <- rbind(testing, SDATA.TtNorm[[sensor]])
}

training$NGROWTH_2 <- ( training$GROWTH_2 - min(training$GROWTH_2) ) / ( max(training$GROWTH_2) - min(training$GROWTH_2) )
training$NGROWTH_4 <- ( training$GROWTH_4 - min(training$GROWTH_4) ) / ( max(training$GROWTH_4) - min(training$GROWTH_4) )

testing$NGROWTH_2 <- ( testing$GROWTH_2 - min(testing$GROWTH_2) ) / ( max(testing$GROWTH_2) - min(testing$GROWTH_2) )
testing$NGROWTH_4 <- ( testing$GROWTH_4 - min(testing$GROWTH_4) ) / ( max(testing$GROWTH_4) - min(testing$GROWTH_4) )


fit <- ore.neural('NSP ~ NDAY_OF_WEEK + NTIME_OF_DAY + NGROWTH_2 + NGROWTH_4', data = training)
ans <- ore.pull(predict(fit, ore.push(testing), supplemental.cols = c("NSP")))
ans$AVG <- mean(ans$NSP)

rmse <- sqrt(mean((ans$NSP - ans$pred_NSP)^2))
baseline <- sqrt(mean((ans$NSP - ans$AVG)^2))

testing.prediction <- data.frame(testing, ans[c("pred_NSP", "AVG")])

links <- unique(testing.prediction$LINK_ID)

for(l in links){
	print(l)
	data <- testing.prediction[ testing.prediction$LINK_ID == l, ]
	png(paste('~/Desktop/model2/',l,'.png',sep=''))
	plot(data$NSP,type="l",col="green3")
	lines(data$pred_NSP,col="blue")
	lines(data$AVG,col="black")
	dev.off()
}



callNeural <- function(size, nLayer, activation, index){
	fit <- ore.neural('NSP ~ NDAY_OF_WEEK + NTIME_OF_DAY + NGROWTH_2 + NGROWTH_4', data = training,  hiddenSizes = rep(size, times=nLayer), activations = activation)
	ans <- ore.pull(predict(fit, ore.push(testing), supplemental.cols = c("NSP")))
	ans$AVG <- mean(ans$NSP)
	rmse <- sqrt(mean((ans$NSP - ans$pred_NSP)^2))
	baseline <- sqrt(mean((ans$NSP - ans$AVG)^2))

	write(index, file = "~/Desktop/neural", append = TRUE, sep = " ")
	write(size, file = "~/Desktop/neural", append = TRUE, sep = " ")
	write(nLayer, file = "~/Desktop/neural", append = TRUE, sep = " ")
	write(paste(activation, collapse = ','), file = "~/Desktop/neural", append = TRUE, sep = " ")
	write(rmse, file = "~/Desktop/neural", append = TRUE, sep = " ")
	write(baseline, file = "~/Desktop/neural", append = TRUE, sep = "\n")


	testing.prediction <- data.frame(testing, ans[c("pred_NSP", "AVG")])
	links <- unique(testing.prediction$LINK_ID)
	dir.create(paste('~/Desktop/iter/model-',index,sep=''), showWarnings = TRUE, recursive = FALSE, mode = "0777")
	for(l in links){
		data <- testing.prediction[ testing.prediction$LINK_ID == l, ]
		png(paste('~/Desktop/iter/model-',index,'/',l,'.png',sep=''))
		plot(data$NSP,type="l",col="green3")
		lines(data$pred_NSP,col="blue")
		lines(data$AVG,col="black")
		dev.off()
	}
}

iterativePredict <- function(){
	hiddenSizes <- c(64,128,256)
	hiddenLayers <- c(1:10)
	activations <- c("atan","bSigmoid","cos","gaussian","gaussError","gompertz","linear","reciprocal","sigmoid","sigmoidModulus","sigmoidSqrt","sin","square","tanh","wave")
	index <- 1
	for(size in hiddenSizes){
		for(nLayer in hiddenLayers){
			pActivations <- permutations(n = length(activations), r = nLayer, v = activations)
			for(i in c(1:length(pActivations))){
				print(size)
				print(nLayer)
				print(c(pActivations[i,], "linear"))
				print(index)

				if(index > 11){
					callNeural(size, nLayer, c(pActivations[i,], "linear"), index)	
				}
				index <- index + 1
			}
		}
	}
}


interestedSensor <- 717055



buildModel <- function(sensor){
	SDATA <- fromSensor(DATA, sensor)
	SDATA <- within(SDATA, {
		NDAY_OF_WEEK  <- as.numeric(as.ore.character(SDATA$DT, format = "D")) / 7
		DAY_OF_MONTH  <- ore.mday(SDATA$DT)
		NTIME_OF_DAY  <- ore.hour(SDATA$DT) * 12 + ore.minute(SDATA$DT) / ( 24 * 12 )
		NSP <- SDATA$SP / max(SDATA$SP)
		MONTH <- ore.month(SDATA$DT)
	})

	SDATA.dump <- ore.pull(SDATA)
	SDATA.ordered <- SDATA.dump[ order(SDATA.dump$DT), ]

	SDATA.train <- SDATA.ordered[ SDATA.ordered$DAY_OF_MONTH <= 28 & SDATA.ordered$MONTH == 4 | SDATA.ordered$DAY_OF_MONTH <= 21 & SDATA.ordered$MONTH == 5, ]
	SDATA.test  <- SDATA.ordered[ SDATA.ordered$DAY_OF_MONTH > 21 & SDATA.ordered$DAY_OF_MONTH <= 28 & SDATA.ordered$MONTH == 5, ]


	# Additional variables
	SDATA.train$GROWTH_2 <- 0
	SDATA.train$GROWTH_4 <- 0
	SDATA.test$GROWTH_2 <- 0
	SDATA.test$GROWTH_4 <- 0
	for(i in c(5:nrow(SDATA.train))){
		SDATA.train[i,]$GROWTH_2 <- SDATA.train[i - 1,]$SP - SDATA.train[i - 2,]$SP
		SDATA.train[i,]$GROWTH_4 <- ( (SDATA.train[i - 1,]$SP + SDATA.train[i - 2,]$SP) / 2 ) - ( (SDATA.train[i - 3,]$SP + SDATA.train[i - 4,]$SP) / 2 )
	}
	for(i in c(5:nrow(SDATA.test))){
		SDATA.test[i,]$GROWTH_2 <- SDATA.test[i - 1,]$SP - SDATA.test[i - 2,]$SP
		SDATA.test[i,]$GROWTH_4 <- ( (SDATA.test[i - 1,]$SP + SDATA.test[i - 2,]$SP) / 2 ) - ( (SDATA.test[i - 3,]$SP + SDATA.test[i - 4,]$SP) / 2 )
	}

	SDATA.train$NGROWTH_2 <- ( SDATA.train$GROWTH_2 - min(SDATA.train$GROWTH_2) ) / ( max(SDATA.train$GROWTH_2) - min(SDATA.train$GROWTH_2) )
	SDATA.train$NGROWTH_4 <- ( SDATA.train$GROWTH_4 - min(SDATA.train$GROWTH_4) ) / ( max(SDATA.train$GROWTH_4) - min(SDATA.train$GROWTH_4) )

	SDATA.test$NGROWTH_2 <- ( SDATA.test$GROWTH_2 - min(SDATA.test$GROWTH_2) ) / ( max(SDATA.test$GROWTH_2) - min(SDATA.test$GROWTH_2) )
	SDATA.test$NGROWTH_4 <- ( SDATA.test$GROWTH_2 - min(SDATA.test$GROWTH_2) ) / ( max(SDATA.test$GROWTH_2) - min(SDATA.test$GROWTH_2) )

	return(SDATA)
}

predictor <- function(SDATA, index, sensor, model, sizes=c(), activations=c()){
	fit <- ore.neural(model, data = SDATA.train,  hiddenSizes = sizes, activations = activations, maxIterations=1000L)
	ans <- ore.pull(predict(fit, ore.push(SDATA.test), supplemental.cols = c("NSP")))
	#png(paste('~/Desktop/iter/',sensor,'-',index,'.png',sep=''))
	png(paste('/u01/Scripts/NN/DATA/iter',sensor,'-',index,'.png',sep=''))
	
	plot(ans$NSP,type="l",col="green3")
	lines(ans$pred_NSP,col="blue")
	lines(historical.average$NSP,col="black")

	#dev.off()
	
	rmse <- sqrt(mean((ans$NSP - ans$pred_NSP)^2))
	ans$AVG <- mean(ans$NSP)
	baseline <- sqrt(mean((ans$NSP - ans$AVG)^2))

	print(rmse)
	print(baseline)

	#cat(rmse)
	#cat(",")
	#cat(baseline)
	#cat("\n")
}

SDATA <- buildModel(interestedSensor)

predictor(SDATA, 387, interestedSensor, 'NSP ~ NDAY_OF_WEEK + NTIME_OF_DAY + NGROWTH_2 + NGROWTH_4', sizes=c(64,64,64,64), activations=c('bSigmoid', 'square', 'tanh', , 'linear'))
predictor(SDATA, 186, interestedSensor, 'NSP ~ NDAY_OF_WEEK + NTIME_OF_DAY + NGROWTH_2 + NGROWTH_4', sizes=c(128,128,128,128), activations=c('atan','tanh','gaussian','bSigmoid','linear'))


iterativePredict <- function(interestedSensor, SDATA){
	model <- 'NSP ~ NDAY_OF_WEEK + NTIME_OF_DAY + NGROWTH_2 + NGROWTH_4'

	hiddenSizes <- c(64,128,256)
	hiddenLayers <- c(4)
	activations <- c("atan","bSigmoid","gaussian","linear","sigmoid","sigmoidSqrt","square","tanh")

	#sink("~/Desktop/neural")
	index <- 1
	for(size in hiddenSizes){
		for(nLayer in hiddenLayers){
			pActivations <- permutations(n = length(activations), r = nLayer, v = activations)
			for(i in c(1:length(pActivations))){
				#cat(index)
				#cat(",")
				#cat(size)
				#cat(",")
				#cat(nLayer)
				#cat(",")
				#cat(paste(c(pActivations[i,], "linear")))
				#cat(",")

				predictor(SDATA, index, interestedSensor, model, rep(size, times=nLayer), c(pActivations[i,], "linear"))

				index <- index + 1
			}
		}
	}
	#sink()
}

historical <- data.table(SDATA.train)
historical.average <- historical[, lapply(.SD, mean), by=list(NDAY_OF_WEEK, NTIME_OF_DAY)]
plot(historical.average$NSP,type="l",col="black")

# Closer examiniation of the interested activation functions



# errors <- c()
# for(i in c(1:length(interestedLinks))){
# 	sensor <- interestedLinks[i]
# 	errors[i] <- predictor(sensor, 'NSP ~ NDAY_OF_WEEK + NTIME_OF_DAY + NGROWTH_2 + NGROWTH_4')
# }


# interestedLinks.errors <- data.frame(interestedLinks, errors)
# interestedLinks.errorData <- ore.pull(merge(interestedLinks.errors, HIGHWAY_CONFIG_NORM, by.x="interestedLinks", by.y="LINK_ID"))

# interestedLinks.errorData$errorGroup <- 0
# for(i in c(1:nrow(interestedLinks.errorData))){
# 	if(interestedLinks.errorData[i,]$errors <= 3){
# 		interestedLinks.errorData[i,]$errorGroup <- 0
# 	} else if(interestedLinks.errorData[i,]$errors <= 6){
# 		interestedLinks.errorData[i,]$errorGroup <- 1
# 	} else {
# 		interestedLinks.errorData[i,]$errorGroup <- 2
# 	}
# }

# col <- c("green", "yelow", "red")
# map <- qmap('Los Angeles', zoom = 10, maptype = 'roadmap')
# map + geom_point(data = interestedLinks.errorData, aes(x=interestedLinks.errorData$LATITUDE, y = interestedLinks.errorData$LONGITUDE), color=col[interestedLinks.errorData$errorGroup], size=2, alpha=0.5)
