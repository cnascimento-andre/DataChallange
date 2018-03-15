# TOTVS Data Challenge

# André de Carvalho Nascimento
# 13/03/2018
# ---------------------------------------------------------------------------------------------------------

library(ggplot2)
library(caret)
library(jsonlite)
library(reshape2)
library(dplyr)
library(gridExtra)

rawdata <- fromJSON("D:/folder where the dataset.zip is unzipped/sample.txt")

# the initial idea was to download the file direct from the link, but I had some problems to unzip and open the sample.txt file
# so I had to download and unzip the file by hand

# ------------------------ Tidying the data ---------------------------------------------------------------

    # check if the data is from a single cnpj source
    # check if all operations are the same, "VENDA"
    # check for near zero values from $total$icmsTot
levels(as.factor(rawdata$emit$cnpj)) # checked - there is only the cnpj 01.234.567/0001-89
levels(as.factor(rawdata$ide$natOp)) # checked, all from "VENDA"
nz <- nearZeroVar(rawdata$total$icmsTot, saveMetrics = TRUE)
nz[nz$zeroVar == FALSE & nz$nzv == FALSE,] #checked, only vProd, VTotTrib and vnf have non near zero values

    # extract the names of the consumed products 
    # buil a matrix (same rows as rawdata) that specifies the quantity and value paid for each product at each "NF"
prod <- c(); id <- c()
for (i in 1:length(rawdata$dets)) {
    prod <- c(prod, rawdata$dets[[i]]$prod$xProd)
    id <- c(id, rep(i, length(rawdata$dets[[i]]$prod$xProd)))
}
prodnames <- levels(as.factor(prod))
columns <- outer(c("qty", "v"),prodnames, function(x,y) paste(x,y,sep = "."))
columns <- c(columns[1,], columns[2,])
mqv <- as.data.frame(matrix(0, nrow = length(rawdata$dets), ncol = length(columns)));
colnames(mqv) <- columns
for (i in 1:length(rawdata$dets)) {
    for (j in 1:length(rawdata$dets[[i]]$prod$xProd)) {
        qtycol <- paste("qty", rawdata$dets[[i]]$prod$xProd[j], sep = ".")
        vcol <- paste("v", rawdata$dets[[i]]$prod$xProd[j], sep = ".")
        qty <- rawdata$dets[[i]]$prod$qCom[j]
        val <- rawdata$dets[[i]]$prod$vProd[j]
        mqv[i, qtycol] <- mqv[i, qtycol] + qty
        mqv[i, vcol] <- mqv[i, vcol] + val
    }
}

    # check sum(all products) with sum(vProd)
paste("difference in R$: ",signif(abs(sum(mqv[, grepl("v.", names(mqv))]) - sum(rawdata$total$icmsTot$vProd)), digits = 2)) #checked

    # check the data time/date range
    # defining lunch and dinner
    # build a matrix (same rows as rawdata) with TIME columns: /Day...../weekday...../monthday..../time...../timeNUM...../type...../
days <- sort(rawdata$ide$dhEmi)
difftime(days[1], days[length(days)], units = 'days') # checked - days from 2016-01-05 to 2016-01-23 - same month
mtime <- data.frame(day = as.Date(rawdata$ide$dhEmi, origin ="1970-01-01"),
                    weekday = as.factor(weekdays(rawdata$ide$dhEmi)),
                    monthday = as.numeric(format(as.Date(rawdata$ide$dhEmi, format = "%Y-%m-%d") ,format = "%d")),
                    time = format(as.POSIXct(rawdata$ide$dhEmi) ,format = "%H:%M:%S"),
                    timeNUM = as.numeric(difftime(rawdata$ide$dhEmi, as.Date(rawdata$ide$dhEmi), units = "mins")),
                    type = as.factor(sample(c("lunch", "dinner"),length(rawdata$ide$dhEmi), replace = TRUE)))
mtime$type[mtime$timeNUM > 1000] <- "dinner" # timeNUM equivalent to 17h 
mtime$type[mtime$timeNUM < 1000] <- "lunch" # timeNUM equivalent to 17h 

    # Building the final tidy data frame ()
rawTIDY <- cbind(vProd = rawdata$total$icmsTot$vProd,
                 vTotTrib = rawdata$total$icmsTot$vTotTrib, 
                 mesa = as.factor(rawdata$infAdic$infCpl),
                 mtime, mqv)
summary(rawTIDY)

# ------------------------ Exploratory data analysis ---------------------------------------------------------------

# aggregating the sum of products by day (monthday)
drops <- c("mesa", "monthday", "weekday", "time", "timeNUM", "type", "day")
agg <- aggregate(rawTIDY[ , !(names(rawTIDY) %in% drops)], by  =list(rawTIDY$monthday), sum)
colnames(agg)[1] <- "monthday"

# checking for nzvalues regargind new data.frame
nz2 <- nearZeroVar(rawTIDY, saveMetrics = TRUE)
nz2 <- nz2[nz2$zeroVar == FALSE & nz2$nzv == FALSE,]
# regarding the products, only AGUA, BUFFET, REFRIGERANTE and SUCO have nzvalues and can show us some representative information

# plotting QTY for the main products for each day
keeps1 <- c("monthday", rownames(nz2)[c(grep("^qty.", rownames(nz2)))])
longformatQTY <- melt(agg[,keeps1], id = "monthday")
ggplot(data = longformatQTY, aes(x = monthday, y = value, colour = variable)) +
            ggtitle("Main products total consume (qty)") +
            geom_line() +
            ylab("Qty (KG | UN)")+
            xlab("2016-jan-")+
            scale_x_continuous(breaks = c(agg$monthday))
# this plot shows peaks for each product at the same weekday (2006-01-06, 13 and 20) which is wednesday
# the buffet qty, shown in KG, seems to be more constant (smaller difference between peaks and valleys)
# regarding the beverages, REFRIGERANTE is much more consumed than AGUA, followed by SUCO
# as expected, the BUFFET consumption pulls up the BEVERAGE consumption

# -------------------------- Forecast total income -------------------------------------------------------
library(quantmod)
library(forecast)

# decomposing the time series for total income
ts1 <- ts(agg$vProd, frequency = 6) # this frequency is due to the 6 working days of the restaurant
plot(decompose(ts1), xlab = "Weeks")
# with this plot it is possible to see the seasonal efect (wednesday peaks already observed)
# which is more relevant than the trend (that remains between R$5000,00 - $5800,00)

# using forecast function
ts1Train <- window(ts1, start = 1, end = 2.8)
ts1Test <- window(ts1, start = 2.6, end = c(3,5))
ets1 <- ets(ts1Train, model = "MMM")
# an important warning message says that the seasonal component could not be estimated
# even with different models, as AAA, MAM, etc.
fcast <- forecast(ets1)
# plotting the forecast for the fourth week
plot(fcast, xlim = c(1,4.5), xlab = "6 days period", ylab = "total income R$", main = "Prediction by forecast (MMM) model")
lines(ts1Test, col = "red")
# as it is observed, the model it is not reliable to next period prediction, maybe it is necessary more time data

# using linear model
lm(agg$vProd ~ agg$monthday)
# the linear regression calculates an almost constant prediction for the total income (~R$5400,00 per day)
ggplot(data = agg, aes(x = monthday, y = vProd, colour = "observed data")) +
    ggtitle("Prediction by linear regression") +
    ylab("total income - R$") + 
    xlab("monthday - 2016-jan") + 
    geom_line() +
    geom_smooth(aes(colour = "linear model prediction"), method = "lm", se = FALSE) +
    scale_x_continuous(breaks = c(agg$monthday))

# As there is no enougth data for a properly forecast model,
# the linear calculates an almost constant prediction, 
# and by the exploratory analysis we could see a sazonal aspect,
# maybe using the mean of each weekday is a good choice to predict the next week total income
wdays <- c()
for (i in 1:length(agg$monthday)) {
    wdays[i] <- weekdays(as.Date(paste("2016-01-", agg$monthday[i], sep = "")))
}
wmean <- tapply(agg$vProd, wdays, mean)
df <- data.frame(weekday = as.factor(names(wmean)), vProd = wmean)
df$weekday <- factor(df$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
df <- df[order(df$weekday),]
prediction <- data.frame(january.day = c(25:30), vProd = df$vProd)
print(prediction)
# plotting prediction by mean
ggplot(data = agg, aes(x = monthday, y = vProd, colour = "observed data")) +
    ggtitle("Prediction by weekday mean") +
    ylab("total income - R$") + 
    xlab("monthday - 2016-jan-") + 
    geom_line() + geom_point() +
    geom_line(data = prediction, aes(x = january.day, y = vProd, colour = "simple mean prediction")) +
    geom_point(data = prediction, aes(x = january.day, y = vProd, colour = "simple mean prediction")) +
    scale_x_continuous(breaks = c(agg$monthday, prediction$january.day))
