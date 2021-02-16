library(splines)
library(dplyr)
library(purrr)
mdl_df1 <- readRDS("Naval_app/mdl_df1.RDS")
mdl_df2 <- readRDS("Naval_app/mdl_df2.RDS")
mdl_df3 <- readRDS("Naval_app/mdl_df3.RDS")
mdl_df4 <- readRDS("Naval_app/mdl_df4.RDS")
mdl_df5 <- readRDS("Naval_app/mdl_df5.RDS")
mdl_df6 <- readRDS("Naval_app/mdl_df6.RDS")
mdl_df7 <- readRDS("Naval_app/mdl_df7.RDS")
mdl_df8 <- readRDS("Naval_app/mdl_df8.RDS")
mdl_df9 <- readRDS("Naval_app/mdl_df9.RDS")

modelos<-list(mdl_df1, mdl_df2, mdl_df3, mdl_df4, mdl_df5, mdl_df6, mdl_df7,
              mdl_df8, mdl_df9)

NPM_te<-read.csv("Naval_app/test.csv")

b <- 3*rep(1:9, each = 10, times = 3)

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

comb2 <- function (b, mini, maxi){
  y <- vector("list", length(b))
  acum <- 0
  for (i in 1:length(b)) {
    temp1<-filter(NPM_te, Ship_spd == b[i] & Turb_decay > mini & Turb_decay <= maxi)
    temp2<-temp1[sample(nrow(temp1), size = 1),]
    ind<-temp2$Ship_spd/3
    temp3<-predict(modelos[[ind]], newdata = temp2)
    acum <- acum + temp3*60
    y[[i]]<-mutate(temp2, "pred" = temp3, "f_acum" = acum, "t" = i)
  }
  y<-bind_rows(y)
  return(y)
}

z2<-comb2(b, 0.975, 0.981)
d2<-comb2(b, 0.981, 0.994)
f2<-comb2(b, 0.994, 1)


b<-tibble(v=c(3, 6, 9, 12), t=c(1, 1, 1, 1))
mini<-0.975
maxi<-1

comb1 <- function(v, t, mini, maxi){
  temp1<-filter(NPM_te, Ship_spd == v & Turb_decay > mini & Turb_decay <= maxi)
  temp2<-temp1[sample(nrow(temp1), size = 60*t, replace = T),]
  pred<-predict(modelos[[v/3]], newdata = temp2)
  temp3<-mutate(temp2, pred = pred)
  return(temp3)
}

test<-map2(b$v, b$t, comb1, mini, maxi)%>%
  bind_rows()%>%
  mutate(acum = cumsum(60*pred), t=(1:length(pred))/60)
