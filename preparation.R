library(plyr)
library(ggplot2)



n <- 1000

a_mean <- -50

b_mean <- 50

a_std <- 20

b_std <- 30

treshold <- data.frame(treshold = 20)

df <- data.frame(
                  Category=factor(rep(c("A", "B"), each=n)),
                  Data=round(c(rnorm(n, mean=a_mean, sd=a_std), rnorm(n, mean=b_mean, sd=b_std)))
                        )


mu <- ddply(df, "Category", summarise, grp.mean=mean(Data))

confusion <- ddply(df, "Category", summarise, A_test=sum(Data<treshold$treshold))

confusion$B_test <- n-confusion$A_test

confusion <- t(confusion)[-1,]

colnames(confusion) <- c("A_True", "B_True")

confusion <- as.data.frame(confusion)




p <- ggplot(df, aes(x=Data, color=Category, fill = Category))+
      geom_histogram( alpha=0.5, position = "identity")+
      geom_vline(data=mu, aes(xintercept=grp.mean, color=Category),
                  linetype="dashed")+
      geom_vline(data = treshold, aes(xintercept = treshold, linetype = "dotted"), 
                 show.legend = FALSE) +
      theme_classic()+
      theme(legend.position="top")+
      labs(caption = "Martin Slíva, (CC)")
      

print(p)
print(confusion)

