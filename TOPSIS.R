# Step 0
# Load the fighter aircraft problem data as the example
x_t = read.csv('/Users/changmufan/Desktop/review.csv',header = T)
row.names(x_t) = c('Group1','Group2','Group3','Group4','Group5','Group6','Group7')
x_t = x_t[,2:8]

norm_DM = sweep(x_t,2,sqrt(colSums((x_t)^2)),'/') 
#import weight & calculate WNDmatrix !
CWV<-c(0.1,0.125,0.125,0.125,0.125,0.25,0.15)
WNDmatrix = sweep(norm_DM,2,CWV,'*')
WNDmatrix

#set A* & A' 
A_plus = apply(WNDmatrix,2,'max')  #another cool function!!!!!
A_minus = apply(WNDmatrix,2,'min')
A_plus
A_minus

#calculate separation measure
S_plus = sqrt(rowSums((sweep(WNDmatrix,2,A_plus,'-'))^2))
S_plus
S_minus = sqrt(rowSums((sweep(WNDmatrix,2,A_minus,'-'))^2))
S_minus

#calculate the relative closeness
RC = S_minus / (S_plus + S_minus)
RC

#rank the preference from C_plus
x_t_rank = 8-rank(RC)
x_t_rank
WNDmatrix = cbind(WNDmatrix,RC)
WNDmatrix = cbind(WNDmatrix,x_t_rank)
colnames(WNDmatrix)[8:9] = c('總分','排名')
