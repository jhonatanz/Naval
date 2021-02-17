library(splines)
library(dplyr)
library(purrr)
library(plotly)

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
NPM_te <- NPM_te %>%
  mutate(NPM_te, cond = as.factor(if_else(Turb_decay < 0.981, "mala", 
                                if_else(Turb_decay >= 0.981 & Turb_decay <0.994, 
                                        "normal", "optima"))))
NPM_te$Ship_spd <- as.factor(NPM_te$Ship_spd)

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

comb1 <- function(v, t, condicion){
  temp1<-filter(NPM_te, Ship_spd == v & cond == condicion)
  temp2<-temp1[sample(nrow(temp1), size = 60*t, replace = T),]
  pred<-predict(modelos[[v/3]], newdata = temp2)
  temp3<-mutate(temp2, pred = pred)
  return(temp3)
}
x <- list()
for(i in levels(NPM_te$cond)){
  x[[i]]<-map2(b$v, b$t, comb1, i)%>%
    bind_rows()%>%
    mutate(acum = cumsum(60*pred), t=(1:length(pred))/60)%>%
    group_by(Ship_spd)%>%
    mutate(media = mean(pred))
}
df<-bind_rows(x)

x1<-map2(b$v, b$t, comb1, "mala")%>%
  bind_rows()%>%
  mutate(acum = cumsum(60*pred), t=(1:length(pred))/60)%>%
  group_by(Ship_spd)%>%
  mutate(media = mean(pred))

y1<-map2(b$v, b$t, comb1, "normal")%>%
  bind_rows()%>%
  mutate(acum = cumsum(60*pred), t=(1:length(pred))/60)%>%
  group_by(Ship_spd)%>%
  mutate(media = mean(pred))

z1<-map2(b$v, b$t, comb1, "optima")%>%
  bind_rows()%>%
  mutate(acum = cumsum(60*pred), t=(1:length(pred))/60)%>%
  group_by(Ship_spd)%>%
  mutate(media = mean(pred))

df<-bind_rows(x1, y1, z1)

plt1 <- ggplot(data=df)+
  geom_line(mapping = aes(x=t, y=acum, color = cond))

plt2 <- ggplot(data = df)+
  geom_point(mapping = aes(x = t, y = pred, color = cond))+
  geom_line(mapping = aes(x = t, y = media, linetype = cond))

plt3 <- ggplot(data = df)+
  geom_line(mapping = aes(x = t, y = media, color = cond))
ggplotly(plt1)
p2<-ggplotly(plt2)
p3<-ggplotly(plt3)
subplot(p2, p3, nrows = 1)
p2
plt2
plt4 <- ggplot(data = df)+
  geom_point(mapping = aes(x=t, y = pred, color = Turbine_temp))
plt4

plt5 <- ggplot(data = df)+
  geom_point(mapping = aes(x = Turbine_temp, y = pred, color = as.factor(Ship_spd)))
plt5
ggplotly(plt5)

t<-ggplot(mpg, aes(cty, hwy))+geom_point()
t + labs(x = "New x axis label", y = "New y axis label",
         title ="Add a title above the plot",
         subtitle = "Add a subtitle below title",
         caption = "Add a caption below plot"
         )
df1<-df%>%
  group_by(cond)%>%
  summarize(Consumo = round(max(acum)))
