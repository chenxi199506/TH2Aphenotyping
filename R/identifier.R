#load_all()
#use_package("magrittr")
#use_package("tidyverse")
#use_package("e1071")
#usethis::use_pipe(export = TRUE)
#给定Pannel（默认），提供数据接口，测试数据
#需要优化参数



#' predict of the TH2A
#'
#' @param xtest input dataframe of your scRNA data.
#' @param Panelgene the panel gene you decided.
#' @return a new col of your data with prediction either TH2A (return 1) or Non-TH2A (return 0).
#' @export
#' @importFrom e1071 svm
#' @examples
#' TH2A_prediction(test)


TH2A_prediction<-function(xtest,Panelgene=c("CACNA1D","IL1RL1","IL17RB","IL5","INPP1","HPGDS","EGLN3")){

  train<- modelall %>% select((Panelgene))
  train$cluster<-modelall$cluster
  svm.model<- e1071::svm(cluster ~ ., data = train)
  xtest<- xtest%>% select((Panelgene))
  svm.pred <- predict(svm.model,xtest)
  xtest$TH2A_prediction<-svm.pred
  return(xtest)

}


#' predict of the TH2A prob
#'
#' @param xtest input dataframe of your scRNA data.
#' @param Panelgene the panel gene you decided.
#' @return a new col of your data with prediction either TH2A (return 1) or Non-TH2A (return 0).
#' @export
#' @importFrom e1071 svm
#' @examples
#' TH2A_probability(test)


TH2A_probability<-function(xtest,Panelgene=c("CACNA1D","IL1RL1","IL17RB","IL5","INPP1","HPGDS","EGLN3")){
  train<- modelall %>% select((Panelgene))
  train$cluster<-modelall$cluster
  svm.model<- e1071::svm(cluster ~ ., data = train,probability = TRUE)
  xtest<- xtest%>% select((Panelgene))
  svm.pred <- predict(svm.model,xtest,probability = TRUE)
  pred<-as.data.frame(attr(svm.pred,"probabilities"))
  xtest$TH2A_probability<-pred[,2]
  return(xtest)
}




