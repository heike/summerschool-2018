## ------------------------------------------------------------------------
tips <- read.csv("https://raw.githubusercontent.com/heike/summerschool-2017/master/01-Introduction-to-R/data/tips.csv")

summary(tips$total_bill)

## ------------------------------------------------------------------------
tips$rate <- tips$tip / tips$total_bill
mean(tips$rate[tips$smoker=="Yes"])

## ------------------------------------------------------------------------
library(ggplot2)

qplot(total_bill, tip, geom = "point", data = tips, facets=~day)

## ------------------------------------------------------------------------
nrow(iris)
ncol(iris)
dim(iris)

## ------------------------------------------------------------------------
rep(1:5, each = 2)

## ------------------------------------------------------------------------
rep(1:5, times = 3)

## ------------------------------------------------------------------------
sum(c(TRUE, TRUE, FALSE, TRUE, FALSE))

## ------------------------------------------------------------------------
sum(tips$rate > .2)

## ------------------------------------------------------------------------
sum(tips$total_bill[tips$rate > .2])

## ------------------------------------------------------------------------
qplot(carat, price, data = diamonds)

## ------------------------------------------------------------------------
diamonds$ppc <- diamonds$price / diamonds$carat

## ------------------------------------------------------------------------
qplot(ppc, geom = "histogram", data = diamonds[diamonds$ppc > 10000,])

## ------------------------------------------------------------------------
mydf <- data.frame(col1 = 1:6, col2 = rep(c("a", "b"), times = 3))

## ------------------------------------------------------------------------
mydf[mydf$col2 == "a",]

## ------------------------------------------------------------------------
mtcars[4,]

## ------------------------------------------------------------------------
mylist <- list(vec = 1:6, df = data.frame(x = 1:2, y = 3:4, z = 5:6))

## ------------------------------------------------------------------------
mylist[[2]]

## ------------------------------------------------------------------------
mylist[[2]][1,]

## ------------------------------------------------------------------------
head(mtcars, n = 8)

## ------------------------------------------------------------------------
str(mtcars)

## ------------------------------------------------------------------------
dim(mtcars)

## ------------------------------------------------------------------------
summary(mtcars)

## ------------------------------------------------------------------------
mean_and_ci <- function(x) {
    themean <- mean(x)
    theci <- t.test(x)$conf.int
    
    return(list(mean = themean, ci = theci))
}

## ------------------------------------------------------------------------
mean_and_ci <- function(x) {
    if (!is.numeric(x) && !is.logical(x)) stop("Need logical or numeric data")
    
    x <- as.numeric(x)
    
    themean <- mean(x)
    theci <- t.test(x)$conf.int
    
    return(list(mean = themean, ci = theci))
}

## ------------------------------------------------------------------------
for (i in 1:ncol(diamonds)) {
    if (is.numeric(diamonds[,i])) print(mean_and_ci(diamonds[,i]))
}

## ---- fig.width=5, fig.height=3, fig.align='center'----------------------
plot(cars)

## ---- fig.cap='This is a caption'----------------------------------------
plot(cars)

