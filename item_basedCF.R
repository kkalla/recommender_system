## Item-based CF

## Reading data
source('readingdata.R')

## Binarizing
r <- as(data,"realRatingMatrix")
r_b <- binarize(r, minRating = 1)
head(as(r_b,"matrix"))

## Make training set 
trainingData <- sample(4945,4500)
trainingSet <- r_b[trainingData]
trainingSet <- trainingSet[rowCounts(trainingSet)>5]

## recommend model
model <- Recommender(trainingSet,method = "IBCF",param="Jaccard")

recommenderUserList <- r_b[-trainingData]
recommendList <- predict(model,recommenderUserList,n=5)
as(recommendList,"list")$'124660'

## Simulation
r_b<- r_b[rowCounts(r_b)>5]
scheme <- evaluationScheme(r_b,method="split",train=.8,given=6,k=3)
scheme

algorithms <- list(
    "item-based CF" = list(name="IBCF", param=list(method="Jaccard"))
)
results <- evaluate(scheme,algorithms,n=c(1,3,5))
results
avg(results)
plot(results, annotate=T,legend="topleft")
plot(results, "prec/rec",annotate=T,legend="topleft")
