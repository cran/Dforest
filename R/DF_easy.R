globalVariables(c("Type"))


#' @title Simple pre-defined pipeline for Decision forest
#' @description This is a script of decision forest for easy use t
#`        Several pre-defined pipelines have been implemented into the package.
#'
#'
#' @param Train_X Training Dataset
#' @param Train_Y Training data endpoint
#' @param Test_X Testing Dataset
#' @param Test_Y Testing data endpoint
#' @param mode pre-defined modeling
#'
#' @return  data_matrix training and testing result
#'
#' @export
#'
#' @examples
#'   # data(demo_simple)
#'   X = iris[,1:4]
#'   Y = iris[,5]
#'   names(Y)=rownames(X)
#'
#'   random_seq=sample(nrow(X))
#'   split_rate=3
#'   split_sample = suppressWarnings(split(random_seq,1:split_rate))
#'   Train_X = X[-random_seq[split_sample[[1]]],]
#'   Train_Y = Y[-random_seq[split_sample[[1]]]]
#'   Test_X = X[random_seq[split_sample[[1]]],]
#'   Test_Y = Y[random_seq[split_sample[[1]]]]
#'
#'   Result = DF_easy(Train_X, Train_Y, Test_X, Test_Y)

DF_easy  = function (Train_X, Train_Y, Test_X, Test_Y, mode = "default"){
  # parameter settings
  if (mode == "default") {
    stop_step=100
    CV_fold = 5
    Max_tree=20
    min_split=3
    cp = 0.1
    Filter = F
    p_val = 0.05
    Method = "bACC"
    Quiet = T
    Grace_val = 0.05
    imp_accu_val = 0.01
    imp_accu_criteria = F
  }

  CV_result  = DF_CV(X=Train_X, Y=Train_Y, stop_step=stop_step, CV_fold = CV_fold,
                        Max_tree=Max_tree, min_split=min_split, cp = cp,
                        Filter = Filter, p_val = p_val, Method = Method, Quiet = Quiet,
                        Grace_val = Grace_val, imp_accu_val = imp_accu_val, imp_accu_criteria = imp_accu_criteria )

  used_model = DF_train(X=Train_X, Y=Train_Y, stop_step=stop_step,
                        Max_tree=Max_tree, min_split=min_split, cp = cp,
                        Filter = Filter, p_val = p_val, Method = Method, Quiet = Quiet,
                        Grace_val = Grace_val, imp_accu_val = imp_accu_val, imp_accu_criteria = imp_accu_criteria )

  Pred_result = DF_pred(used_model, X = Test_X, Y = Test_Y)

  Performance = c(used_model$performance[[Method]],CV_result$performance[[Method]],Pred_result$performance[[Method]])

  MCC_matrix = c(used_model$performance[["MCC"]],CV_result$performance[["MCC"]],Pred_result$performance[["MCC"]])
  ACC_matrix = c(used_model$performance[["ACC"]],CV_result$performance[["ACC"]],Pred_result$performance[["ACC"]])
  bACC_matrix = c(used_model$performance[["bACC"]],CV_result$performance[["bACC"]],Pred_result$performance[["bACC"]])

  data_matrix=data.frame(ACC = round(Performance,3),Type=c("Fitting","Cross.Validation","Testing"))


  p_1 = ggplot(data_matrix, aes(x=Type,y=Performance,fill=Type,label = round(Performance,3)))+
    geom_bar(stat = "identity", width = 0.5, alpha = 0.8)+
    scale_fill_manual(values=c("Blue","Green","Darkred"), guide = F)+
    geom_label(size = 5, position = position_dodge(width = 0.5),fill = "white")+
    xlab(label = NULL)+ylim(0,1)+labs(title=paste("Model Performance (",Method,")",sep=""))+
    theme(text = element_text(size=15))

  p_2 = DF_ConfPlot(Pred_result, Test_Y)
  p_2 = p_2 + theme(legend.position="none")
  p_3 = DF_ConfPlot_accu(Pred_result, Test_Y)
  p_3 = p_3 + theme(legend.position="none")+xlab("ConfidenceLevel (Cumulative)")

  layout = matrix(c(1,2,1,3), nrow=2, byrow=TRUE)
  multiplot(p_1,p_2,p_3,cols = 2, layout = layout)

  result = list(bAcc=bACC_matrix,MCC=MCC_matrix,ACC=ACC_matrix)
  return(result)
}
