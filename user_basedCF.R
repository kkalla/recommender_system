## User-based CF
source('readingdata.R')

## Recommenderlab package는 데이터를 저장하는 두가지 class를 지원한다.
## 1. realRatingMatrix - continuous value, ex) movie rating
## 2. binaryRatingMatrix - binary value, ex) click or not
r <- as(data, "realRatingMatrix")

## Make training set
trainingData <- sample(4945,4500)
trainingSet <- r[trainingData]
trainingSet <- trainingSet[rowCounts(trainingSet)>5]
str(trainingSet)

## Make model using Recommender() with cosine similarity
model <- Recommender(trainingSet,method = "UBCF",parameter = "Cosine")
## model using Pearson similarity
model_pearson <- 
    Recommender(trainingSet,method = "UBCF",parameter = "Pearson")

## Recommend
recommenderUserList <- r[-trainingData]
recommendList <- predict(model,recommenderUserList,n=5)
as(recommendList,"list")

## Simulation for measuring accuracy
## Store evaluation scheme
## method = "split" - dataset is diviede into the proportion we specify
## method = "cross" - do cross test
## method = "bootstrap" - test using bootstrapping
r <- r[rowCounts(r)>5]
scheme <- evaluationScheme(r,method = "split",train=.8, given = 6,
                           goodRating = 4, k=3)
scheme

algorithms <- list(
    "user-based CF_Cosine" = list(name="UBCF",param=list(method="Cosine")),
    "user-based CF_Pearson"=list(name="UBCF",param=list(method="Pearson")))
results <- evaluate(scheme,algorithms,n=c(1,3,5))
results
## See FPR, TPR, recall, precision etc.
avg(results)

## Decide number of neighbor after decide similarity
algorithms_nn <- list(
    "user-based CF_c20"=list(name="UBCF",param=list(method="Cosine",nn=20)),
    "user-based CF_c30"=list(name="UBCF",param=list(method="Cosine",nn=30)),
    "user-based CF_c40"=list(name="UBCF",param=list(method="Cosine",nn=40)),
    "user-based CF_c50"=list(name="UBCF",param=list(method="Cosine",nn=50)),
    "user-based CF_c60"=list(name="UBCF",param=list(method="Cosine",nn=60)),
    "user-based CF_c70"=list(name="UBCF",param=list(method="Cosine",nn=70)))
results_nn <- evaluate(scheme,algorithms_nn,n=c(1,3,5))

plot(results_nn, annotate=TRUE,legend="topleft")
plot(results_nn, "prec/rec",annotate=TRUE, legend = "left")
avg(results_nn)

## Check F-measure
recall_c20 <- (avg(results_nn)$'user-based CF_c20')[,"recall"]
precision_c20 <- (avg(results_nn)$'user-based CF_c20')[,"precision"]
F_c20 <- 2*(precision_c20*recall_c20)/(precision_c20+recall_c20)
mean(F_c20)

recall_c30 <- (avg(results_nn)$'user-based CF_c30')[,"recall"]
precision_c30 <- (avg(results_nn)$'user-based CF_c30')[,"precision"]
F_c30 <- 2*(precision_c30*recall_c30)/(precision_c30+recall_c30)

recall_c40 <- (avg(results_nn)$'user-based CF_c40')[,'recall'] 
precision_c40 <- (avg(results_nn)$'user-based CF_c40')[,'precision'] 
F_c40 <- 2*(precision_c40*recall_c40)/(precision_c40+recall_c40)

recall_c50 <- (avg(results_nn)$'user-based CF_c50')[,'recall'] 
precision_c50 <- (avg(results_nn)$'user-based CF_c50')[,'precision'] 
F_c50 <- 2*(precision_c50*recall_c50)/(precision_c50+recall_c50)

recall_c60 <- (avg(results_nn)$'user-based CF_c60')[,'recall'] 
precision_c60 <- (avg(results_nn)$'user-based CF_c60')[,'precision'] 
F_c60 <- 2*(precision_c60*recall_c60)/(precision_c60+recall_c60)

recall_c70 <- (avg(results_nn)$'user-based CF_c70')[,'recall'] 
precision_c70 <- (avg(results_nn)$'user-based CF_c70')[,'precision'] 
F_c70 <- 2*(precision_c70*recall_c70)/(precision_c70+recall_c70)

a <- cbind(
    cbind(recall_c20,precision_c20,F_c20),
    cbind(recall_c30,precision_c30,F_c30),
    cbind(recall_c40,precision_c40,F_c40),
    cbind(recall_c50,precision_c50,F_c50),
    cbind(recall_c60,precision_c60,F_c60),
    cbind(recall_c70,precision_c70,F_c70)
)
print(a)
