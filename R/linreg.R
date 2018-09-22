#' linreg class
#'
#' This class contains methods to handle multiple linear regression.
#' You can find more information about the linear regression in the link
#' bellow:
#' \url{https://en.wikipedia.org/wiki/Linear_regression}
#'
#' @field formula A formula to show the relationship between
#' dependent and independent variables.
#' @field data A data.frame to give the values of variables.
#' @field reg_coeffs A matrix of regression coefficients.
#' @field fitted_value A matrix of predicted values for dependent
#' variable.
#' @field resids A matrix that contains the differences between
#' predicted and actual values of dependent variable.
#' @field df A numeric scalar that shows the degree of freedom.
#' @field resids_var A matrix that shows the variance of residuals.
#' @field reg_coef_var A matrix that shows the variance of regression
#'  coefficients.
#' @field t_values A matrix that shows t values.
#' @field p_values A matrix of p values.
#'
#' @import methods
#' @export linreg
#' @exportClass linreg

linreg <- setRefClass('linreg'
,fields = list(  formula = 'formula'
               , data = 'character'
               , reg_coeffs = 'matrix'
               , fitted_values = 'matrix'
               , resids = 'matrix'
               , df = "numeric"
               , resids_var = "matrix"
               , reg_coef_var = "matrix"
               , t_values="matrix"
               , p_values = "matrix"
               )
,methods = list(
initialize = function(formula,data){
    "initializing regression calculations"

    #independent variables matrix
    X <- model.matrix(formula,data)

    #dependent variable vector
    y <- data[,all.vars(formula)[1]]

    #finding Regressions_coeffcients
    reg_coeffs <<- (solve(t(X) %*% X) %*% t(X)) %*% y

    #finding fitted_values
    fitted_values <<- X %*% reg_coeffs

    #finding residuals
    resids <<- (y - fitted_values)

    #finding degree of freedom
    n <- nrow(data)
    p <- length(all.vars(formula))

    df <<- n - p

    #finding residual variance
    resids_var <<- (t(resids) %*% resids) / df

    #finding variance of the regression cofficients
    reg_coef_var <<- as.numeric(resids_var) * solve(t(X) %*% X)

    #finding t-values
    t_values <<- reg_coeffs/as.numeric(sqrt(var(reg_coeffs)))

    #findind p_values
    p_values <<- pt(reg_coeffs,df)

    #formula
    formula <<- formula

    #data
    data <<- deparse(substitute(data))

}
,print = function(){
  "print regression coefficients"
  cat('call:\n')
  cat(paste0('linreg(formula =',deparse(formula),' , data = ',data,')\n\n'))
  cat('coefficients:\n')
  cat('(intercept)')
  for(i in 2:length(rownames(reg_coeffs))){
    cat(sprintf("%20s",rownames(reg_coeffs)[i]))
  }
  cat('\n')
  cat(round(reg_coeffs[,1][1],3))
  for(i in 2:length(reg_coeffs[,1])){
    cat(sprintf('%20s',as.character(round(reg_coeffs[,1][i]))))
  }
}
,plot = function(){
  "ploting residuals and scaled residuals vs fitted values"
plot_data <- data.frame(resids = resids, fitted_values = fitted_values
                        ,scaled_residuals = sqrt(abs(resids)))


 p1 <- ggplot2::ggplot(data = plot_data,mapping = ggplot2::aes(x = fitted_values,y = resids)) +
 ggplot2::geom_point()+
 ggplot2::geom_smooth(method = "loess", se=FALSE, color = "red")
 p2 <- ggplot2::ggplot(data = plot_data,mapping = ggplot2::aes(x = fitted_values,y = scaled_residuals)) +
   ggplot2::geom_point()+
   ggplot2::geom_smooth(method = "loess", se=FALSE, color = "red")
plotly::subplot(p1,p2,nrows = 2, titleY = TRUE,shareX = TRUE)

}
,resid = function(){
  "printing vector of resisuals"
 return(as.vector(resids))
}
,pred = function(){
  "printing vector of fitted values"
 return(as.vector(fitted_values))
}
,coef = function(){
  "printing regression coefficients"
  round(reg_coeffs[,1],3)
}
,summary = function(){
  "suammary"

}
)
)
#' @importFrom ggplot2 ggplot
#' @importFrom plotly subplot
NULL

