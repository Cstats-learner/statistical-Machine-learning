# for the demonstration purpose I have used state data from library datasets 
library(datasets)
states = data.frame(state.x77, row.names=state.abb)
new_data = states[,-5]#deleting Murder(response variable)
#defining function for calculaating press statistic
#PRESS = ??(ei / (1-hii))2
#where
#ei: The ith residual
#hii: the ii-th  value in the hat matrix
press_cal <- function(x,y){
  lin_model=lm(y~.,data=x)
  press=sum((lin_model$residuals/(1-hatvalues(lin_model)))^2)#press statistic
  return(press)
}
press_statistic=press_cal(new_data,states$Murder)
press_statistic
#for checking whether the output of the press_cal function is correct or not 
#We will match it's output with the predefined PRESS statistic of library MPV 
library(MPV)
m1=lm(states$Murder~.,data=new_data)
PRESS(m1)
# outputs are matching which means press_cal is giving correct output and working fine.
