NPM<-read.csv("navalplantmaintenance.csv", header = F, sep = " ", na.strings = "")
View(NPM)
vect_NA<-apply(NPM, 2, function(x) mean(is.na(x)))<0.3
NPM<-NPM[,vect_NA]
names(NPM)<-c("Lever_pos", "Ship_spd", "GT_shft_trq", "GT_rpm", "Gen_rpm", "S_prop_trq", "P_prop_trq", "Turbine_temp",
              "Comp_in_temp", "Comp_out_temp","Turbine_press", "Comp_in_press", "Comp_out_press", "Exhaust_press",
              "Turb_inj_cnt", "Fuel_flow", "Comp_decay", "Turb_decay")
summary(NPM)
pairs(NPM)
library(caret)
a<-nearZeroVar(NPM)
names(NPM[,a])
# se observa que comp_in_temp y Comp_in_press nunca cambian, por lo tanto no son relevantes para el desarrollo de un modelo
NPM<-NPM[, -a]
cor(NPM)
fit<-lm(Comp_decay~., data = NPM)
summary(fit)
plot(NPM$Lever_pos, NPM$Comp_decay)
plot(NPM$Ship_spd, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$GT_shft_trq, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$GT_rpm, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$Gen_rpm, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$S_prop_trq, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$P_prop_trq, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$Turbine_temp, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$Comp_out_temp, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$Turbine_press, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$Comp_out_press, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$Exhaust_press, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$Turb_inj_cnt, NPM$Comp_decay, col=NPM$Lever_pos)
plot(NPM$Fuel_flow, NPM$Comp_decay, col=NPM$Lever_pos)

# los graficos anteriores demuestran una fuerte dependencia de la posicion del lever y/o de la velocidad del barco, por lo que se 
# decide generar data frames distintos para cada posicion del lever

NPM$Lever_pos<-as.factor(NPM$Lever_pos)
levels(NPM$Lever_pos)
library(dplyr)

dfs<-as.list(levels(NPM$Lever_pos))
dfs1<-lapply(1:9, function(x) filter(NPM, Lever_pos == dfs[[x]]))
#for (i in 1:9) {
#  dfs[i]<-filter(NPM, Lever_pos == dfs[[i]])
#}
df1<-dfs1[[1]]
df2<-dfs1[[2]]
df3<-dfs1[[3]]
df3<-dfs1[[4]]
df5<-dfs1[[5]]
df6<-dfs1[[6]]
df7<-dfs1[[7]]
df8<-dfs1[[8]]
df9<-dfs1[[9]]

a<-nearZeroVar(df1)
df1<-df1[,-a]
pairs(df1)

a<-nearZeroVar(df2)
df2<-df2[,-a]
pairs(df2)

a<-nearZeroVar(df3)
df3<-df3[,-a]
pairs(df3)

a<-nearZeroVar(df4)
df4<-df4[,-a]
pairs(df4)

a<-nearZeroVar(df5)
df5<-df5[,-a]
pairs(df5)

df1_norm<-apply(df1, 2, function(x) (x-mean(x))/sd(x))
df1_norm<-as.data.frame(df1_norm)
fit_dfn<-lm(Comp_decay~.-Turb_decay-P_prop_trq, data = df1_norm)
fit_dfn<-lm(Comp_decay~.-Turb_decay-P_prop_trq-Turbine_temp-S_prop_trq-Comp_out_press-Fuel_flow-GT_shft_trq, data = df1_norm)

# Comparacion entre los datos predecidos y los datos reales
comp<-data.frame(fit_dfn_nl1$fitted.values, df1_norm$Comp_decay, dif = df1_norm$Comp_decay-fit_dfn_nl1$fitted.values)
fit_dfn_nl1<-lm(Comp_decay~ns(GT_rpm, 4)+ns(Gen_rpm, 4)+ns(Comp_out_temp, 4)+ns(Turbine_press, 4)+Turb_inj_cnt, data = df1)
comp<-data.frame(fit_dfn_nl1$fitted.values, df1$Comp_decay, dif = df1$Comp_decay-fit_dfn_nl1$fitted.values)

# Inicio del entrenamiento del modelo final
vec_tr <- createDataPartition(y = df1$Comp_decay, p = 0.75, list = F)
df1_tr <- df1[vec_tr,]
df1_te <- df1[-vec_tr,]
fit_df1_nl <- lm(Comp_decay~ns(GT_rpm, 4)+ns(Gen_rpm, 4)+ns(Comp_out_temp, 4)+ns(Turbine_press, 4)+Turb_inj_cnt, data = df1_tr)
pred_df1_nl <- predict(fit_df1_nl, newdata = df1_te)
comp <- data.frame(df1_te$Comp_decay, prediccion = pred_df1_nl, diferencia = df1_te$Comp_decay-pred_df1_nl, 
                   perc = 100*(df1_te$Comp_decay-pred_df1_nl)/df1_te$Comp_decay)
