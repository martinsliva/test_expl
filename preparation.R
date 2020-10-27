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
      geom_vline(data = treshold, aes(xintercept = treshold, linetype = "longdash", color="blue"), 
                 show.legend = FALSE) +
      theme_classic()+
      theme(legend.position="top")+
      labs(caption = "Martin Slíva, (CC)")
      

print(p)
print(confusion)


df3 <- data.frame( Popis = c("Správně identifikovaní nemocní", "Mylně identifikovaní nemocní", "Správně identifikovaní zdraví", "Mylně identifikovaní zdraví"),
                   Pocty = c(100000, 50000, 800000, 30000)
)

sub_label3 <- "Odhadované počty" 

p2 <- ggplot(df3, aes(x="", y=Pocty, fill=Popis))+
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +
         geom_text(aes(y = Pocty, label = Pocty), color = "white")+
         theme_void()+
         labs(title = sub_label3,
            caption = "Martin Slíva, (CC)") +
         theme(legend.position="bottom")

print(p2)