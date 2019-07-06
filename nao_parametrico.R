

## mann whitney
## dados:
scores <- c(14.8, 07.3, 05.6, 06.3, 09.0, 04.2, 10.6, 12.5, 12.9, 16.1, 11.4, 02.7, 
            12.7, 14.2, 12.6, 02.1, 17.7, 11.8, 16.9, 07.9, 16.0, 10.6, 05.6, 05.6,
            07.6, 11.3, 08.3, 06.7, 03.6, 01.0, 02.4, 06.4, 09.1, 06.7, 18.6, 03.2,
            06.2, 06.1, 15.3, 10.6, 01.8, 05.9, 09.9, 10.6, 14.8, 05.0, 02.6, 04.0)

lugar <- c(rep(1,12),
           rep(2,36))
lugar <- as.factor(lugar)

## teste
wilcox.test(scores~lugar, alternative="greater", exact = FALSE) 



## kruscal wallis
## dados:
producao <- c(83, 91, 94, 89, 89, 96, 91, 92, 90, 91, 90, 81, 83, 84, 83, 88, 91, 
              89, 84, 101, 100, 91, 93, 96, 95, 94 ,78, 82, 81, 77, 79, 81, 80, 81)

metodo <- c(rep(1,9),
            rep(2,10),
            rep(3,7),
            rep(4,8))
metodo <- as.factor(metodo)

## teste
kruskal.test(producao~metodo)



## wilcoxon
## dados:
filho_1 <- c(86, 71, 77, 68, 91, 72, 77, 91, 70, 71, 88, 87)
filho_2 <- c(88, 77, 76, 64, 96, 72, 65, 90, 65, 80, 81, 72)

## teste
wilcox.test(filho_2, filho_1, paired=TRUE, exact = FALSE)
wilcox.test(filho_1, filho_2, paired=TRUE, exact = FALSE)

