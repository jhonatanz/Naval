NPM<-read.csv("navalplantmaintenance.csv", header = F, sep = " ", na.strings = "")
vect_NA<-apply(NPM, 2, function(x) mean(is.na(x)))<0.3
NPM<-NPM[,vect_NA]
names(NPM)<-c("Lever_pos", "Ship_spd", "GT_shft_trq", "GT_rpm", "Gen_rpm", "S_prop_trq", "P_prop_trq", "Turbine_temp",
              "Comp_in_temp", "Comp_out_temp","Turbine_press", "Comp_in_press", "Comp_out_press", "Exhaust_press",
              "Turb_inj_cnt", "Fuel_flow", "Comp_decay", "Turb_decay")
library(caret)
a<-nearZeroVar(NPM)
print(names(NPM[,a]))
NPM<-NPM[, -a]
NPM$Lever_pos<-as.factor(NPM$Lever_pos)
summary(NPM)
plot(NPM$Comp_decay, NPM$Fuel_flow)
plot(NPM$Comp_decay, NPM$Fuel_flow, col = NPM$Lever_pos)
library(ggplot2)
ggplot(data = NPM) +
  geom_point(mapping = aes(x= Comp_decay, y= Fuel_flow)) +
  geom_smooth(mapping = aes(x= Comp_decay, y= Fuel_flow)) +
  facet_wrap(~ Lever_pos)
library(dplyr)
df1 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[1])
a<-nearZeroVar(df1)
df1<-df1[,-a]
df2 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[2])
a<-nearZeroVar(df2)
ggplot(data = df1) +
  geom_point(mapping = aes(x= Comp_decay, y= Fuel_flow)) +
  geom_smooth(mapping = aes(x= Comp_decay, y= Fuel_flow))
ggplot(data = df2) +
  geom_point(mapping = aes(x= Comp_decay, y= Fuel_flow)) +
  geom_smooth(mapping = aes(x= Comp_decay, y= Fuel_flow))
df3 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[3])
ggplot(data = df3) +
  geom_point(mapping = aes(x= Comp_decay, y= Fuel_flow)) +
  geom_smooth(mapping = aes(x= Comp_decay, y= Fuel_flow))
a<-nearZeroVar(df3)
df3 <- df3[,-a]
pairs(df3)
df3_n<-apply(df3, 2, function(x) (x-mean(x))/sd(x))
df3_n<-as.data.frame(df3_n)
fit_dfn3<-lm(Fuel_flow~., data = df3_n)
df3_n<-select(df3_n, -all_of("P_prop_trq"))
fit_dfn3<-lm(Fuel_flow~., data = df3_n)
library(car)
vif(fit_dfn3)
fit_dfn3
fit_dfn3<-lm(Fuel_flow~.-Turb_inj_cnt-Comp_out_temp-Turb_decay-Turbine_temp-Gen_rpm-GT_shft_trq-GT_rpm-S_prop_trq-Turbine_press, 
             data = df3_n)

# Los modelos se dejan iguales para las posiciones de palanca 3 y en adelante
df3 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[3])
df4 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[4])
df5 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[5])
df6 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[6])
df7 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[7])
df8 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[8])
df9 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[9])
fit_df3<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df3)
fit_df4<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df4)
fit_df5<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df5)
fit_df6<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df6)
fit_df7<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df7)
fit_df8<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df8)
fit_df9<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df9)
summary(fit_df3)
summary(fit_df4)
summary(fit_df5)
summary(fit_df6)
summary(fit_df7)
summary(fit_df8)
summary(fit_df9)
vif(fit_df3)
vif(fit_df4)
vif(fit_df5)
vif(fit_df6)
vif(fit_df7)
vif(fit_df8)
vif(fit_df9)
fit_df3<-lm(Fuel_flow~Turbine_temp+Turb_decay, data = df3)
fit_df4<-lm(Fuel_flow~Turbine_temp+Turb_decay, data = df4)
fit_df5<-lm(Fuel_flow~Turbine_temp+Turb_decay, data = df5)
fit_df6<-lm(Fuel_flow~Turbine_temp+Turb_decay, data = df6)
fit_df7<-lm(Fuel_flow~Turbine_temp+Turb_decay, data = df7)
fit_df8<-lm(Fuel_flow~Turbine_temp+Turb_decay, data = df8)
fit_df9<-lm(Fuel_flow~Turbine_temp+Turb_decay, data = df9)

# Análisis para la posición 1 de la palanca

df1 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[1])
a<-nearZeroVar(df1)
df1 <- df1[,-a]
pairs(df1)
df1_n<-apply(df1, 2, function(x) (x-mean(x))/sd(x))
df1_n<-as.data.frame(df1_n)
fit_dfn1<-lm(Fuel_flow~., data = df1_n)
summary(fit_dfn1)
df1_n<-select(df1_n, -all_of("P_prop_trq"))
fit_dfn1<-lm(Fuel_flow~., data = df1_n)
vif(fit_dfn1)
cor(df1_n)
# se decide observar la matriz de correlacion, en donde se encuentra que la variable 
# mas correlacionada con fuel_flow es Turbine_temp, por lo se buscan las variables 
# mas correlacionadas con esta ultima y se eliminan del modelo:
# GT_shft_trq
# Comp_out_temp
# Turbine_press
# Comp_out_press

fit_dfn1<-lm(Fuel_flow~.-GT_shft_trq-Comp_out_temp-Turbine_press-Comp_out_press, data = df1_n)
# se continua eliminando las correlaciones siguiendo una estrategia similar
fit_dfn1<-lm(Fuel_flow~.-GT_shft_trq-Comp_out_temp-Turbine_press-Comp_out_press-S_prop_trq-Comp_decay, data = df1_n)
fit_df1<-lm(Fuel_flow~.-GT_shft_trq-Comp_out_temp-Turbine_press-Comp_out_press-S_prop_trq-Comp_decay, data = df1)
summary(fit_df1)
#Modelo final
fit_df1_nl<-lm(Fuel_flow~ns(GT_rpm, 3)+ns(Gen_rpm, 3)+Turbine_temp+ns(Turb_inj_cnt, 3)+ns(Turb_decay, 3), data = df1)
summary(fit_df1_nl)

# Análisis para la posición 2 de la palanca

df2 <- NPM %>% filter(Lever_pos == levels(Lever_pos)[2])
a<-nearZeroVar(df2)
df2 <- df2[,-a]
df2_n<-apply(df2, 2, function(x) (x-mean(x))/sd(x))
df2_n<-as.data.frame(df2_n)
fit_dfn2<-lm(Fuel_flow~., data = df2_n)
summary(fit_dfn2)
df2_n<-select(df2_n, -all_of("P_prop_trq"))
fit_dfn2<-lm(Fuel_flow~., data = df2_n)
vif(fit_dfn2)
pairs(df2)
cor(df2)
fit_dfn2<-lm(Fuel_flow~.-GT_shft_trq-Comp_out_temp-Turbine_press-Comp_out_press, data = df2_n)
summary(fit_dfn2)
fit_dfn2<-lm(Fuel_flow~.-GT_shft_trq-Comp_out_temp-Turbine_press-Comp_out_press-S_prop_trq-Comp_decay-Turb_inj_cnt, data = df2_n)
fit_df2_nl<-lm(Fuel_flow~ns(GT_rpm, 3)+ns(Gen_rpm, 3)+Turbine_temp+ns(Turb_decay, 3), data = df2)
summary(fit_df2_nl)

#Inicio de entrenamiento
NPM<-read.csv("navalplantmaintenance.csv", header = F, sep = " ", na.strings = "")
vect_NA<-apply(NPM, 2, function(x) mean(is.na(x)))<0.3
NPM<-NPM[,vect_NA]
names(NPM)<-c("Lever_pos", "Ship_spd", "GT_shft_trq", "GT_rpm", "Gen_rpm", "S_prop_trq", "P_prop_trq", "Turbine_temp",
              "Comp_in_temp", "Comp_out_temp","Turbine_press", "Comp_in_press", "Comp_out_press", "Exhaust_press",
              "Turb_inj_cnt", "Fuel_flow", "Comp_decay", "Turb_decay")
library(caret)
library(dplyr)
library(splines)
a<-nearZeroVar(NPM)
print(names(NPM[,a]))
NPM<-NPM[, -a]
NPM$Lever_pos<-as.factor(NPM$Lever_pos)
vec_tr <- createDataPartition(y = NPM$Fuel_flow, p = 0.75, list = F)
NPM_tr <- NPM[vec_tr,]
NPM_te <- NPM[-vec_tr,]

df1_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[1])
df2_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[2])
df3_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[3])
df4_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[4])
df5_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[5])
df6_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[6])
df7_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[7])
df8_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[8])
df9_tr <- NPM_tr %>% filter(Lever_pos == levels(Lever_pos)[9])

df1_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[1])
df2_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[2])
df3_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[3])
df4_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[4])
df5_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[5])
df6_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[6])
df7_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[7])
df8_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[8])
df9_te <- NPM_te %>% filter(Lever_pos == levels(Lever_pos)[9])

mdl_df1<-lm(Fuel_flow~ns(GT_rpm, 3)+ns(Gen_rpm, 3)+Turbine_temp+ns(Turb_inj_cnt, 3)+ns(Turb_decay, 3), data = df1_tr)
mdl_df2<-lm(Fuel_flow~ns(GT_rpm, 3)+ns(Gen_rpm, 3)+Turbine_temp+ns(Turb_decay, 3), data = df2_tr)
mdl_df3<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df3_tr)
mdl_df4<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df4_tr)
mdl_df5<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df5_tr)
mdl_df6<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df6_tr)
mdl_df7<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df7_tr)
mdl_df8<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df8_tr)
mdl_df9<-lm(Fuel_flow~Turbine_temp+Turbine_press+Turb_decay, data = df9_tr)
saveRDS(mdl_df1, "mdl_df1.RDS")
saveRDS(mdl_df2, "mdl_df2.RDS")
saveRDS(mdl_df3, "mdl_df3.RDS")
saveRDS(mdl_df4, "mdl_df4.RDS")
saveRDS(mdl_df5, "mdl_df5.RDS")
saveRDS(mdl_df6, "mdl_df6.RDS")
saveRDS(mdl_df7, "mdl_df7.RDS")
saveRDS(mdl_df8, "mdl_df8.RDS")
saveRDS(mdl_df9, "mdl_df9.RDS")
write.csv(NPM_te, file = "test.csv")
