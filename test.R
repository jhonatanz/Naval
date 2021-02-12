comb <- function (b, mini, maxi){
  y <- data.frame(matrix(ncol = ncol(NPM_te)+1, nrow = 0))
  names(y)<-c(names(NPM_te), "pred")
  acum <- 0
  for (i in 1:length(b)) {
    temp1<-filter(NPM_te, Ship_spd == b[i] & Turb_decay > mini & Turb_decay <= maxi)
    temp2<-temp1[sample(nrow(temp1), size = 1),]
    ind<-temp2$Ship_spd/3
    temp3<-predict(modelos[[ind]], newdata = temp2)
    acum <- acum + temp3*60
    temp4<-mutate(temp2, "pred" = temp3, "f_acum" = acum, "t" = i)
    y<-rbind(y, temp4)
  }
  return(y)
}

z<-comb(b, 0.975, 0.981)
d<-comb(b, 0.981, 0.994)
f<-comb(b, 0.994, 1)

plot(z$t, z$f_acum, type= "l", col ="blue")
lines(d$t, d$f_acum, col="red")
lines(f$t, f$f_acum, col="black")
ta<-rbind(summarize(z, consumo = max(f_acum), min_decay = min(Turb_decay), max_decay = max(Turb_decay)),
      summarize(d, consumo = max(f_acum), min_decay = min(Turb_decay), max_decay = max(Turb_decay)),
      summarize(f, consumo = max(f_acum), min_decay = min(Turb_decay), max_decay = max(Turb_decay))
)
print(ta)
