
#--------------------------- מבחנים השוואת חלופות -----------------------------------

# ----------- אחוז מסיימים ---------------

test<- t.test(x= finishPrecent1$CA,y=finishPrecent$CA, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)

test<- t.test(x= finishPrecent2$CA,y=finishPrecent$CA, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)

test<- t.test(x= finishPrecent1$CA,y=finishPrecent2$CA, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)

# ----------- זמן ממוצע בתור ---------------

test<- t.test(x= avgWaitingByreplication1$AW, y = avgWaitingByreplication$AW, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)

test<- t.test(x= avgWaitingByreplication2$AW, y = avgWaitingByreplication$AW, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)

test<- t.test(x= avgWaitingByreplication1$AW, y = avgWaitingByreplication2$AW, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)

# ----------- אורך תור ממוצע ---------------

test<- t.test(x= avgQueueByreplication1$AQ, y = avgQueueByreplication$AQ, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)

test<- t.test(x= avgQueueByreplication2$AQ, y = avgQueueByreplication$AQ, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)

test<- t.test(x= avgQueueByreplication1$AQ, y = avgQueueByreplication2$AQ, alternative="two.sided", paired = TRUE, var.equal = TRUE,conf.level=0.99)
print(test)
