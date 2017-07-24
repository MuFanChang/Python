rm(list = ls())
# read data
x = read.csv('/Users/changmufan/Desktop/review.csv',header = T)
row.names(x) = c('Group1','Group2','Group3','Group4','Group5','Group6','Group7')
x = x[,2:8]
SAW_score_vector <- c(0,0,0,0,0,0,0);
x<-cbind(x,SAW_score_vector);
SAW_weight_vector <- c(0.1,0.125,0.125,0.125,0.125,0.25,0.15);

x["Group1","SAW_score_vector"] <- sum (SAW_weight_vector * x["Group1",]);
x["Group2","SAW_score_vector"] <- sum (SAW_weight_vector * x["Group2",]);
x["Group3","SAW_score_vector"] <- sum (SAW_weight_vector * x["Group3",]);
x["Group4","SAW_score_vector"] <- sum (SAW_weight_vector * x["Group4",]);
x["Group5","SAW_score_vector"] <- sum (SAW_weight_vector * x["Group5",]);
x["Group6","SAW_score_vector"] <- sum (SAW_weight_vector * x["Group6",]);
x["Group7","SAW_score_vector"] <- sum (SAW_weight_vector * x["Group7",]);
x;
# Rank them!
x_rank = 8-rank(x[,8])
x = cbind(x,x_rank)
colnames(x)[8:9] = c('總分','排名')
#this SAW is non-standardize's version!!!------------------------UP-----------------------------------------

#below is standardize bx S/SUM(S)
x1 = read.csv('/Users/changmufan/Desktop/review.csv',header = T)
row.names(x1) = c('Group1','Group2','Group3','Group4','Group5','Group6','Group7')
x1 = x1[,2:8]

x1[,1]<- x[,1]/sum(x[,1]);
x1[,2] <- x[,2]/sum(x[,2]);
x1[,3] <- x[,3]/sum(x[,3]);
x1[,4] <- x[,4]/sum(x[,4]);
x1[,5] <- x[,5]/sum(x[,5]);
x1[,6] <- x[,6]/sum(x[,6]);
x1[,7] <- x[,7]/sum(x[,7]);

x1<-cbind(x1,SAW_score_vector);

x1["Group1","SAW_score_vector"] <- sum (SAW_weight_vector * x1["Group1",]);
x1["Group2","SAW_score_vector"] <- sum (SAW_weight_vector * x1["Group2",]);
x1["Group3","SAW_score_vector"] <- sum (SAW_weight_vector * x1["Group3",]);
x1["Group4","SAW_score_vector"] <- sum (SAW_weight_vector * x1["Group4",]);
x1["Group5","SAW_score_vector"] <- sum (SAW_weight_vector * x1["Group5",]);
x1["Group6","SAW_score_vector"] <- sum (SAW_weight_vector * x1["Group6",]);
x1["Group7","SAW_score_vector"] <- sum (SAW_weight_vector * x1["Group7",]);
x1;

x1_rank = 8-rank(x1[,8])
x1 = cbind(x1,x1_rank)
colnames(x1)[8:9] = c('總分','排名')
#below is standardize bx S/MaxS
x2 = read.csv('/Users/changmufan/Desktop/review.csv',header = T)
row.names(x2) = c('Group1','Group2','Group3','Group4','Group5','Group6','Group7')
x2 = x2[,2:8]

x2[,1]<- x2[,1]/max(x2[,1]);
x2[,2] <- x2[,2]/max(x2[,2]);
x2[,3] <- x2[,3]/max(x2[,3]);
x2[,4] <- x2[,4]/max(x2[,4]);
x2[,5] <- x2[,5]/max(x2[,5]);
x2[,6] <- x2[,6]/max(x2[,6]);
x2[,7] <- x2[,7]/max(x2[,7]);
x2<-cbind(x2,SAW_score_vector);

x2["Group1","SAW_score_vector"] <- sum (SAW_weight_vector * x2["Group1",]);
x2["Group2","SAW_score_vector"] <- sum (SAW_weight_vector * x2["Group2",]);
x2["Group3","SAW_score_vector"] <- sum (SAW_weight_vector * x2["Group3",]);
x2["Group4","SAW_score_vector"] <- sum (SAW_weight_vector * x2["Group4",]);
x2["Group5","SAW_score_vector"] <- sum (SAW_weight_vector * x2["Group5",]);
x2["Group6","SAW_score_vector"] <- sum (SAW_weight_vector * x2["Group6",]);
x2["Group7","SAW_score_vector"] <- sum (SAW_weight_vector * x2["Group7",]);
x2;
x2_rank = 8-rank(x2[,8])
x2 = cbind(x2,x2_rank)
colnames(x2)[8:9] = c('總分','排名')
#below is standardize bx (S-MinS)/(MaxS-MinS) ***added "Price" column and it's ranking criteria is low to high!!!
x3 = read.csv('/Users/changmufan/Desktop/review.csv',header = T)
row.names(x3) = c('Group1','Group2','Group3','Group4','Group5','Group6','Group7')
x3 = x3[,2:8]

x3[,1] <- (x3[,1]-min(x3[,1]))/(max(x3[,1])-min(x3[,1]));
x3[,2] <- (x3[,2]-min(x3[,2]))/(max(x3[,2])-min(x3[,2]));
x3[,3] <- (x3[,3]-min(x3[,3]))/(max(x3[,3])-min(x3[,3]));
x3[,4] <- (x3[,4]-min(x3[,4]))/(max(x3[,4])-min(x3[,4]));
x3[,5] <- (x3[,5]-min(x3[,5]))/(max(x3[,5])-min(x3[,5]));
x3[,6] <- (x3[,6]-min(x3[,6]))/(max(x3[,6])-min(x3[,6]));
x3[,7] <- (x3[,7]-min(x3[,7]))/(max(x3[,7])-min(x3[,7]));
x3<-cbind(x3,SAW_score_vector);

x3["Group1","SAW_score_vector"] <- sum (SAW_weight_vector * x3["Group1",]);
x3["Group2","SAW_score_vector"] <- sum (SAW_weight_vector * x3["Group2",]);
x3["Group3","SAW_score_vector"] <- sum (SAW_weight_vector * x3["Group3",]);
x3["Group4","SAW_score_vector"] <- sum (SAW_weight_vector * x3["Group4",]);
x3["Group5","SAW_score_vector"] <- sum (SAW_weight_vector * x3["Group5",]);
x3["Group6","SAW_score_vector"] <- sum (SAW_weight_vector * x3["Group6",]);
x3["Group7","SAW_score_vector"] <- sum (SAW_weight_vector * x3["Group7",]);
x3;
x3_rank = 8-rank(x3[,8])
x3 = cbind(x3,x3_rank)
colnames(x3)[8:9] = c('總分','排名')
#below is standardize bx S/sqrt(sum(S^2))
x4 = read.csv('/Users/changmufan/Desktop/review.csv',header = T)
row.names(x4) = c('Group1','Group2','Group3','Group4','Group5','Group6','Group7')
x4 = x4[,2:8]

x4[,1] <- x4[,1]/sqrt(sum(x4[,1]^2));
x4[,2] <- x4[,2]/sqrt(sum(x4[,2]^2));
x4[,3] <- x4[,3]/sqrt(sum(x4[,3]^2));
x4[,4] <- x4[,4]/sqrt(sum(x4[,4]^2));
x4[,5] <- x4[,5]/sqrt(sum(x4[,5]^2));
x4[,6] <- x4[,6]/sqrt(sum(x4[,6]^2));
x4[,7] <- x4[,7]/sqrt(sum(x4[,7]^2));
x4<-cbind(x4,SAW_score_vector);

x4["Group1","SAW_score_vector"] <- sum (SAW_weight_vector * x4["Group1",]);
x4["Group2","SAW_score_vector"] <- sum (SAW_weight_vector * x4["Group2",]);
x4["Group3","SAW_score_vector"] <- sum (SAW_weight_vector * x4["Group3",]);
x4["Group4","SAW_score_vector"] <- sum (SAW_weight_vector * x4["Group4",]);
x4["Group5","SAW_score_vector"] <- sum (SAW_weight_vector * x4["Group5",]);
x4["Group6","SAW_score_vector"] <- sum (SAW_weight_vector * x4["Group6",]);
x4["Group7","SAW_score_vector"] <- sum (SAW_weight_vector * x4["Group7",]);
x4;
x4_rank = 8-rank(x4[,8])
x4 = cbind(x4,x4_rank)
colnames(x4)[8:9] = c('總分','排名')

x1;
x2;
x3;
x4;
x_rank
x1_rank
x2_rank
x3_rank
x4_rank

