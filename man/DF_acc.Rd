% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DT_con.R
\name{DF_acc}
\alias{DF_acc}
\title{Performance evaluation from Decision Tree Predictions}
\usage{
DF_acc(pred, label)
}
\arguments{
\item{pred}{Predictions}

\item{label}{Known-endpoint}
}
\value{
result$ACC:   Predicting Accuracy

result$MIS:   MisClassfication Counts

result$MCC:   Matthew's Correlation Coefficients

result$bACC:  balanced Accuracy
}
\description{
Performance evaluation from Decision Tree Predictions
}
