# Developed By Michel Chamoun
# Assignment: credit risk - Statistical Learning course

library("dplyr")
  # load the .csv file
  path_train = "C:\\Users\\micho\\OneDrive\\UNI\\HEC\\Winter Term\\statistical learning MATH 60603\\assignments\\assignment 1 credit risk\\CreditGameData\\CreditGameData2021\\CreditGame_TRAIN.csv"
  training_csv = read.csv(path_train)
  names(training_csv)

  # exploring the data
  summary_of_variable <- function(varvar){
    print(summary(varvar))
    print(paste("number of NA variables: ", sum(is.na(varvar))))
    print(paste("the number of unique values: ", length(unique(varvar))))
    print(paste("the unique values are: ", unique(varvar)))
  }
  
  summary_of_variable(training_csv$NB_EMPT)
  summary_of_variable(training_csv$R_ATD)
  summary_of_variable((training_csv$DUREE))  
  summary_of_variable(training_csv$PRT_VAL)
  summary_of_variable(training_csv$AGE_D)  
  summary_of_variable(training_csv$REV_BT)
  summary_of_variable(training_csv$REV_NET)  
  summary_of_variable(training_csv$TYP_RES)  
  summary_of_variable(training_csv$ST_EMPL)  
  summary_of_variable(training_csv$MNT_EPAR)  
  summary_of_variable(training_csv$NB_ER_6MS)
  summary_of_variable(training_csv$NB_ER_12MS)
  summary_of_variable(training_csv$NB_DEC_12MS)
  summary_of_variable(training_csv$NB_OPER)  
  summary_of_variable(training_csv$NB_COUR)  
  summary_of_variable(training_csv$NB_INTR_1M)  
  summary_of_variable(training_csv$NB_INTR_12M)
  summary_of_variable(training_csv$PIR_DEL) 
  summary_of_variable(training_csv$NB_DEL_30)
  summary_of_variable(training_csv$NB_DEL_60)  
  summary_of_variable(training_csv$NB_DEL_90)  
  summary_of_variable(training_csv$MNT_PASS)  
  summary_of_variable(training_csv$MNT_ACT)
  summary_of_variable(training_csv$MNT_AUT_REN)  
  summary_of_variable(training_csv$MNT_UTIL_REN)  
  summary_of_variable(training_csv$NB_SATI)  
  summary_of_variable(training_csv$TYP_FIN)  
  summary_of_variable(training_csv$MNT_DEMANDE)  
  summary_of_variable((training_csv$DEFAULT))  
  summary_of_variable(training_csv$PROFIT_LOSS)  
  
  str(training_csv)
  head(training_csv)  
  training_csv %>% nrow()
  
  # select the ID where NB_ER_6MS>NB_ER_12MS ==> if it exists --> ABSURD!
  training_csv %>% select(ID_TRAIN,NB_ER_6MS,NB_ER_12MS) %>% filter(NB_ER_6MS>NB_ER_12MS) # 0 selected ==> GOOD!!
  
  # select the ID where NB_INTR_1M > NB_INTR_12M ==> if it exists --> ABSURD!
  training_csv %>% select(ID_TRAIN,NB_INTR_1M,NB_INTR_12M) %>% filter(NB_INTR_1M>NB_INTR_12M) # 0 selected ==> GOOD!!
  

  #--------------------------------------------- edit the data ---------------------------------------------
  #training_data = training_csv
  raw_data = training_csv
 
  raw_data$ST_EMPL %>% unique()
  #summary((raw_data %>% select(AGE_D,ST_EMPL,RESLT) %>% filter(ST_EMPL==""))$RESLT) #92.41% of ST_EMPL="" payed
  #summary((raw_data %>% select(AGE_D,ST_EMPL,RESLT) %>% filter(is.na(AGE_D)))$RESLT)#95.06% of "no age" payed
  
    # add a column AGE_ADDED which is = 1 if the age was missing and replaced by the median age
      raw_data$AGE_ADDED <- ifelse(is.na(raw_data$AGE_D),1,0)
      #unique(raw_data %>% select(AGE_D,AGE_ADDED) %>% filter(is.na(AGE_D)))  
      #unique(raw_data %>% select(AGE_D,AGE_ADDED) %>% filter(!(is.na(AGE_D))))
      median_age = median(raw_data$AGE_D,na.rm=TRUE)
      # replace NA by the median Age
      raw_data$AGE_D <- ifelse(is.na(raw_data$AGE_D),median_age,raw_data$AGE_D)
      
    # add a column ST_EMPL_ADDED which is = 1 if the missing values were replaced by the most frequent ST_EMPL
      raw_data$ST_EMPL_ADDED <- ifelse(raw_data$ST_EMPL=="",1,0)
      # replace the "" by most frequent ST_EMPL
      most_frequent_ST_EMPL = tail(names(sort(table(raw_data$ST_EMPL))), 1)
      raw_data$ST_EMPL <- ifelse(raw_data$ST_EMPL=="",most_frequent_ST_EMPL,raw_data$ST_EMPL)
      unique(raw_data$ST_EMPL) #DONE
      
    # transform the problem into a binary classification problem
    # generate the RESLT column which is 1 if profit and 0 if loss
      #raw_data$RESLT <- ifelse(raw_data$PROFIT_LOSS <= 0, 0, 1) 
      #unique((raw_data %>% select(PROFIT_LOSS,RESLT) %>% filter(PROFIT_LOSS>0))$RESLT)
      
    # drop the TYP_FIN, PROFIT_LOSS columns NB: we leave the ID_TRAIN to be able to split the data afterwards
      drops <- c('TYP_FIN','PROFIT_LOSS')
      raw_data = raw_data[,!(names(raw_data)%in%drops)]     
      
  
      sum(is.na(raw_data)==TRUE) #NO MORE NA
      sum(raw_data=="") #NO MORE ""
  
    # categorical variables
    raw_data = raw_data %>%
      mutate(TYP_RES = as.factor(TYP_RES), ST_EMPL = as.factor(ST_EMPL),DEFAULT = as.factor(DEFAULT),
             AGE_ADDED=as.factor(AGE_ADDED),ST_EMPL_ADDED=as.factor(ST_EMPL_ADDED))
    
  summary(raw_data)
  
  # let's go a bit further and see the correlation between numeric variables
    # first we need to select the numeric variables only
  numericVars = select_if(raw_data,is.numeric)
  corrplot(cor(numericVars %>% select(-c("ID_TRAIN"))),method="color")
  
  # we can see some very strong correlations between some variables
  # high correlation between:
  #       REV_BT and REV_NET ==> discard REV_NET
  #       NB_INTR_1M and NB_INTR_12M ==> discard 1M
  # -----------------------------------------------------------------------------------------------------------------
  
  
  # -------------------------------------------- train/validation split ---------------------------------------------
  raw_clean_data = raw_data
  
  nb_rows = raw_clean_data %>% nrow()
  set.seed(654321)
  
    # training set
  percentage_train = 0.8
  training_data = sample_n(raw_clean_data,nb_rows*percentage_train)
  training_data %>% nrow()
  
    # validation set
  validation_data <- anti_join(raw_clean_data, training_data, by="ID_TRAIN") # Anti-join
  validation_data %>% nrow()
  
  # now we can drop the ID_TRAIN column
  drops <- c("ID_TRAIN")
  training_data <- training_data[,!(names(training_data)%in%drops)]
  #validation_data <- validation_data[,!(names(validation_data)%in%drops)]

  # -----------------------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------ some functions -------------------------------------------------
  # remove columns by their name
  remove_col <- function(df,col){
    return(df %>% select(-c(col)))
  }
  
  # add interactions
    # first we put all the categorical variables first (we do not want interactions with them)
    # then we calculate interactions for the rest of the variables.
  add_interactions <- function(df){
    df = df %>% select(DEFAULT,AGE_ADDED,ST_EMPL_ADDED,ST_EMPL,TYP_RES,everything())
    df_plus=df
    nb_col = length(names(df))
    names_of_col = names(df)
    
    for(i in 6:(nb_col-1)){
      for(j in (i+1):nb_col){
        # interactions
        df_plus = df_plus %>%
          mutate(!!paste(names_of_col[i],names_of_col[j],sep="_"):=as.numeric(df[[i]])*df[[j]])
      }
    }
    return(df_plus)
  }
  
  # add interactions validation
  # first we put all the categorical variables first (we do not want interactions with them)
  # then we calculate interactions for the rest of the variables.
  add_interactions_validation <- function(df){
    df = df %>% select(ID_TEST,AGE_ADDED,ST_EMPL_ADDED,ST_EMPL,TYP_RES,everything())
    df_plus=df
    nb_col = length(names(df))
    names_of_col = names(df)
    
    for(i in 6:(nb_col-1)){
      for(j in (i+1):nb_col){
        # interactions
        df_plus = df_plus %>%
          mutate(!!paste(names_of_col[i],names_of_col[j],sep="_"):=as.numeric(df[[i]])*df[[j]])
      }
    }
    return(df_plus)
  }
  
  
   confusion=function(truth,pred,conversion=c(1,1)){
     # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
     # pred: is a vector of predictions (coded 0,1 or TRUE,FALSE) for the same set, and in the same order
     # conversion: ratios of the proportion for each category before and after over/undersampling.
     # Output: Confusion matrix and fit statistics
     
   #  pred=factor(as.logical(pred),c(FALSE,TRUE))
   #  truth=factor(as.logical(truth),c(FALSE,TRUE))
     a=conversion*table(truth,pred,dnn=c("Truth","Prediction"))
     if(ncol(a)<2){ return(  list(
       Confusion=NA,
       Misclassification=NA,
       Precision=NA,
       Sensitivity=NA,
       Specificity=NA
     )
     )
       }
     list(
       Confusion=addmargins(a, FUN = list(Total = sum), quiet = TRUE),
       Misclassification=1-sum(diag(a))/sum(a),
       Precision=a[2,2]/sum(a[,2]),
       Sensitivity=a[2,2]/sum(a[2,]),
       Specificity=a[1,1]/sum(a[1,])
     )
     
   }
  
  roc=function(truth,p,k=100,plot=TRUE,lines=FALSE,...){
    # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
    # p: is a vector of predicted probabilites of getting 1 (or TRUE) for the same set, and in the same order
    # k: number of points at which the ROC curve will be evaluated
    # plot: plots the ROC curve (or not)
    # lines: add a line to an existing ROC plot
    # Output: (invisible means it will not be displayed on the console)
    #    ROC = list of points of the ROC curve
    #    AUC = Area under the curve
    
    Curve=rbind(c(0,0),
                t(sapply(quantile(p,(k:1)/(k+1)),function(th){
                  pred=as.numeric(p>th)
                  if(length(unique(pred))==2){
                    a=confusion(truth,pred)
                    return(c(1-a$Specificity,a$Sensitivity))
                  } else {
                    return(c(NA,NA))
                  }
                }
                )),
                c(1,1)
    )
    Curve=Curve[complete.cases(Curve),]
    if(plot&!lines) plot(Curve,xlab="1-Specificity",ylab="Sensitivity",main="ROC curve",xlim=0:1,ylim=0:1,type="l",...)
    if(plot&lines) lines(Curve,...)
    invisible(list(ROC=Curve,AUC=sum(diff(Curve[,1])*(Curve[-1,2]+Curve[-nrow(Curve),2])/2)))
  }
  
  lift=function(truth,p,k=100,plot=TRUE,lines=FALSE,...){
       # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
       # p: is a vector of predicted probabilites of getting 1 (or TRUE) for the same set, and in the same order
       # k: number of points at which the lift chart will be evaluated
       # plot: plots the lift chart (or not)
       # lines: add a line to an existing lift chart
       # Output: (invisible means it will not be displayed on the console)
       #     - list of points of the lift chart
       
       Curve=cbind((1:k)/k,c(sapply(quantile(p,((k-1):1)/k),function(th){confusion(truth,as.numeric(p>=th))$Precision})/mean(truth),1)
       )
       if(plot&!lines) plot(Curve,xlab="Depth",ylab="Cumulative Lift",main="Cumulative Lift Chart",xlim=0:1,type="l",...)
       if(plot&lines) lines(Curve,...)
       invisible(Curve)
     }
  
  
   confusion=function(truth,pred,conversion=c(1,1)){
     # Truth: is a vector of 0,1 (or TRUE,FALSE) for the target variable (ideally) in a validation or test set.
     # pred: is a vector of predictions (coded 0,1 or TRUE,FALSE) for the same set, and in the same order
     # conversion: ratios of the proportion for each category before and after over/undersampling.
     # Output: Confusion matrix and fit statistics
     
   #  pred=factor(as.logical(pred),c(FALSE,TRUE))
   #  truth=factor(as.logical(truth),c(FALSE,TRUE))
     a=conversion*table(truth,pred,dnn=c("Truth","Prediction"))
     if(ncol(a)<2){ return(  list(
       Confusion=NA,
       Misclassification=NA,
       Precision=NA,
       Sensitivity=NA,
       Specificity=NA
     )
     )
       }
     list(
       Confusion=addmargins(a, FUN = list(Total = sum), quiet = TRUE),
       Misclassification=1-sum(diag(a))/sum(a),
       Precision=a[2,2]/sum(a[,2]),
       Sensitivity=a[2,2]/sum(a[2,]),
       Specificity=a[1,1]/sum(a[1,])
     )
     
   }
  
   
   # function that gives 0 or 1 according to a certain threshold: if p>=th ==> 1 else ==>0
   p_to_binary <- function(prob,th){
     bin = ifelse(prob>=th,1,0)
     return(as.factor(bin))
   }
   
   # function to find the best cut
   best_cut <- function(truth, probs){
     outs = confusion(truth,p_to_binary(probs,0))
     Misclassification=outs$Misclassification
     TP=outs$Sensitivity
     TN=outs$Specificity
     steps = seq(from = 0.05, to = 1, by= 0.05)
     for (p in steps){
       outs = confusion(truth,p_to_binary(probs,p))
       Misclassification = cbind(Misclassification,outs$Misclassification)
       TP = cbind(TP,outs$Sensitivity)
       TN = cbind(TN,outs$Specificity)
     }
     # plot on a chart
     p_cut = seq(from = 0, to = 1, by= 0.05)
     par(mfrow=c(3,1))
     plot(p_cut,Misclassification,type="l")
     plot(p_cut,TP,type="l")
     plot(p_cut,TN,type = "l")
   }
   
  # -----------------------------------------------------------------------------------------------------------------
  
  # ----------------------------------------------- model -----------------------------------------------------------
  
  #               ----------------------- ALL COLUMNS ARE INCLUDED! -----------------------
  
  # full model
  full_model = glm(DEFAULT~., family="binomial", data=training_data)
  
  #predicted_probabilities = cbind(validation_data$ID_TRAIN,p1)
  #cbind(p1,validation_data)[sort.list(p1,decreasing=TRUE)[1:10],]
  
  # stepwise selection from the full  model
  full_mod_step = step(full_model,trace=FALSE)
  
  
  #               ----------------------- REV_NET and NB_INTR_1M ARE REMOVED -----------------------
  
  training_data_removed = remove_col(training_data,c("REV_NET","NB_INTR_1M"))
  validation_data_removed = remove_col(validation_data,c("REV_NET","NB_INTR_1M"))
  
  #full model
  full_model_removed = glm(DEFAULT~., family="binomial", data=training_data_removed)
  #summary(full_model)
  
  #predicted_probabilities_removed = cbind(validation_data_removed$ID_TRAIN,p1_removed)
  #cbind(p1_removed,validation_data_removed)[sort.list(p1_removed,decreasing=TRUE)[1:10],]
  
  # stepwise selection from the full  model
  full_mod_step_removed = step(full_model_removed,trace=FALSE)
  
  # full model with interactions of order 2
  training_data_removed_interact = add_interactions(training_data_removed)
  validation_data_removed_interact = add_interactions(validation_data_removed)
  
  full_mod_removed_interact = glm(DEFAULT~., family="binomial", data=training_data_removed_interact)
  
  # stepwise selection from the full  model with interacctions of order 2
  full_mod_step_removed_interact = step(full_mod_removed_interact,trace=FALSE)
  
  # -----------------------------------------------------------------------------------------------------------------
  
  # ----------------------------------------------- predict -----------------------------------------------------------
  p_full_model =
    predict(full_model,newdata=validation_data %>% select(-c("ID_TRAIN")),type="response")
  p_full_mod_step = 
    predict(full_mod_step,newdata=validation_data %>% select(-c("ID_TRAIN")),type="response")
  
  p_full_model_removed = 
    predict(full_model_removed,newdata=validation_data_removed %>% select(-c("ID_TRAIN")),type="response")
  p_full_mod_step_removed = 
    predict(full_mod_step_removed,newdata=validation_data_removed %>% select(-c("ID_TRAIN")),type="response")
  
  p_full_mod_removed_interact =
    predict(full_mod_removed_interact, newdata=validation_data_removed_interact %>% select(-c("ID_TRAIN")), type="response")
  p_full_mod_step_removed_interact = 
    predict(full_mod_step_removed_interact, newdata=validation_data_removed_interact %>% select(-c("ID_TRAIN")), type="response")
  
  # -----------------------------------------------------------------------------------------------------------------
  roc(validation_data$DEFAULT,p_full_model)$AUC
  roc(validation_data$DEFAULT,p_full_mod_step,lines=TRUE,col="red")$AUC
  
  roc(validation_data_removed$DEFAULT,p_full_model_removed,lines=TRUE,col="green")$AUC
  roc(validation_data_removed$DEFAULT,p_full_mod_step_removed,lines=TRUE,col="yellow")$AUC
  
  roc(validation_data_removed_interact$DEFAULT,p_full_mod_removed_interact,lines=TRUE,col="blue")$AUC
  # THE MODEL WITH INTERACTIONS IS THE BEST ONE  
  
  lift(as.numeric(validation_data$DEFAULT),p_full_model)
  lift(as.numeric(validation_data$DEFAULT),p_full_mod_step,lines=TRUE,col="red")
  lift(as.numeric(validation_data_removed$DEFAULT),p_full_model_removed,lines=TRUE,col="green")
  lift(as.numeric(validation_data_removed$DEFAULT),p_full_mod_step_removed,lines=TRUE,col="yellow")
  lift(as.numeric(validation_data_removed_interact$DEFAULT),p_full_mod_removed_interact,lines=TRUE,col="blue")
  
  e=confusion(validation_data_removed_interact$DEFAULT,p_to_binary(p_full_mod_removed_interact,0.05))
  
  best_cut(validation_data_removed_interact$DEFAULT,p_full_mod_removed_interact) # 0.1
  
  # ---------------------------------------------- COMPLETE ---------------------------------------------------------
  
  # train the best model with the complete data
  # best model: full_mod_removed_interact
    # raw_clean_data is the training data
    complete_training_data = remove_col(raw_clean_data,c("ID_TRAIN","REV_NET","NB_INTR_1M"))
    complete_training_data = add_interactions(complete_training_data)
    
    best_model = glm(DEFAULT~., family="binomial", data=complete_training_data)
  # load the testing data
    testing_path = "C:\\Users\\micho\\OneDrive\\UNI\\HEC\\Winter Term\\statistical learning MATH 60603\\assignments\\assignment 1 credit risk\\CreditGameData\\CreditGameData2021\\CreditGame_Applications.CSV"
    
    # delete complete data
     rm(complete_training_data)
      
     testing_csv = read.csv(testing_path)
     
     # add a column AGE_ADDED which is = 1 if the age was missing and replaced by the median age
     testing_csv$AGE_ADDED <- ifelse(is.na(testing_csv$AGE_D),1,0)
     #unique(raw_data %>% select(AGE_D,AGE_ADDED) %>% filter(is.na(AGE_D)))  
     #unique(raw_data %>% select(AGE_D,AGE_ADDED) %>% filter(!(is.na(AGE_D))))
     #median_age = median(raw_data$AGE_D,na.rm=TRUE)
     # replace NA by the median Age
     testing_csv$AGE_D <- ifelse(is.na(testing_csv$AGE_D),median_age,testing_csv$AGE_D)
     
     # add a column ST_EMPL_ADDED which is = 1 if the missing values were replaced by the most frequent ST_EMPL
     testing_csv$ST_EMPL_ADDED <- ifelse(testing_csv$ST_EMPL=="",1,0)
     # replace the "" by most frequent ST_EMPL
     #most_frequent_ST_EMPL = tail(names(sort(table(raw_data$ST_EMPL))), 1)
     testing_csv$ST_EMPL <- ifelse(testing_csv$ST_EMPL=="",most_frequent_ST_EMPL,testing_csv$ST_EMPL)
      #DONE
     
     # drop the TYP_FIN, PROFIT_LOSS columns NB: we leave the ID_TRAIN to be able to split the data afterwards
     drops <- c('TYP_FIN','PROFIT_LOSS')
     testing_csv = testing_csv[,!(names(testing_csv)%in%drops)]     
     
     
     sum(is.na(raw_data)==TRUE) #NO MORE NA
     sum(raw_data=="") #NO MORE ""
     
     # categorical variables
     testing_csv = testing_csv %>%
       mutate(TYP_RES = as.factor(TYP_RES), ST_EMPL = as.factor(ST_EMPL),
              AGE_ADDED=as.factor(AGE_ADDED),ST_EMPL_ADDED=as.factor(ST_EMPL_ADDED))
     
     testing_csv = remove_col(testing_csv,c("REV_NET","NB_INTR_1M"))
     testing_csv = add_interactions_validation(testing_csv)
     
     #if(names(testing_csv %>% select(-c("ID_TEST"))) == names(training_data_removed_interact)){
      # print("HURRAY")}
     
     p_best_model =
       predict(best_model, newdata=testing_csv %>% select(-c("ID_TEST")), type="response")

     output = as.data.frame(cbind(testing_csv$ID_TEST,p_to_binary(p_best_model,0.1)))
    
     IDS = output %>% select(V1,V2) %>% filter(V2 == 1)
     IDS %>% nrow() 
     
     path_export = 
     write.csv(IDS$V1,"C:\\Users\\micho\\OneDrive\\UNI\\HEC\\Winter Term\\statistical learning MATH 60603\\assignments\\assignment 1 credit risk\\output.csv", row.names = FALSE)
     