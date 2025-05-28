## Period of time:----

# baseline (time 0)
# 1 month after RT (time 1)
# 12 months after RT (time 12)
# 24 months after RT (time 24)
# instead of treating separately the 12 and 24 months times, we put them together keeping the maximum between
# them in files denominated as "long"

## Endpoints:----

# proctitis
# rectal_bleeding
# management_of_sphincter_control
# diarrhoea
# haematuria
# urinary_frequency
# urinary_urgency
# urinary_incontinence

## Adjustment of the side effects levels:----

#adjusting the dataset means:
# patient\time |  0  |  t  |    adjustment
#_________________________________________
# patient_code1|  2  |  1  |  -->    0
# patient_code2|  1  |  3  |  -->    3
# patient_code3|  2  |  2  |  -->    0

## 

## Datasets: selection of the features:----
#for each side effect and for each time point, we assembled a dataset carrying:
#-the IDs of the patients
#-the Site they were cured in
#-the intensity level of the aforementioned side effect
#-specific features for each dataset:
# these features come from the data of P2a, P3, overall characteristics of the patient (age, weight, ...), 
# and MAPS INDEXES (PCA indexes, lengths of the axes, centers of the ellipses, mean doses, ...) (python report to know more)
# and have been selected in these ways:
#   the numerical variables that scored more than a threshold (0.1) in terms of linear correlation coefficient with the side effect
#   the categorical variables that scored less than a threshold p_value (0.05) in an anova test with the side effect as numerical variable



## functions needed to run the models:----

perform_glmer_by_formula = function(df, formula, portion=0.5, soglia=0.15, plot=T, show_all=T){
  
  options(scipen = 999) #options (scipen=0) to revert to scientific notation VIF
  
  if(show_all==T)
  {cat("--------------------------------------------------------------------", "\n")
    cat("OLS RESULTS:", "\n")}
  df[[2]] = reduce_to1(df[[2]])
  
  if(show_all==T)
    print(formula)
  model <- glmer(formula, data = df, family=binomial) #lmer model
  
  if(show_all==T)
  {print(summary(model))
    cat("VIF:", "\n")}
  #print(vif(model))
  probabilities = fitted(model)
  predictions = ifelse(probabilities > soglia, 1, 0)
  t = table(df[[2]], predictions)
  
  if(show_all==T)
  {cat("\n\n")
    cat(cyan("OLS Performance on the train set: ", "\n"))
    print(t)}
  accuracy <- as.numeric((t[1,1] + t[2,2]) / sum(t))
  precision <- as.numeric(t[2,2] / (t[2,1] + t[2,2]))
  recall <- as.numeric(t[2,2] / (t[1,2] + t[2,2]))
  disease_ratio <- as.numeric((t[2,2]/sum(t[,2]))/(t[2,1]/sum(t[,1])))
  if(show_all==T)
    cat("Accuracy = ", accuracy, "\n", "Precision = ", precision, "\n", "Recall = ", recall, "Disease ratio = ", disease_ratio, "\n\n")
  
  
  
  if(plot)
    plot_curve_disease(df, probabilities, soglia)
  
  
  
  if (plot){
    y = df[[2]]
    df_sorted <- df[order(y), ]
    y_sorted = df_sorted[[2]]
    predictions = predict(model, df_sorted[,-1], type = "response")
    # Crea il primo plot con i label ordinati
    y = y_sorted#df_sorted[[1]]
    par(bg = "white", col.axis = "black", col.lab = "black", col.main="black") 
    plot(y, type = "n", xlab = "Non Malati || Malati", ylab = "Predicted Probabilities", main = "Predicted vs True")
    points(1:length(y), y, pch = ifelse(y == 1, 1, 19), col = ifelse(y == 1, "red", "blue"))
    
    
    for (i in 1:length(predictions)) {
      pred_class = ifelse(predictions[i] > soglia, 1, 0)
      if (y[i] == pred_class) {
        pch_s = 1
        if (y[i] == 1){
          color = "red"
        }else{
          color = "blue"
        }
      } else {
        pch_s = 4
        if (y[i] != 1){
          color = "green"
        }else{
          color = "purple"
        }
      }
      points(i, predictions[i], pch = pch_s,  col = color)
    }
  }
  
  # LEGENDA: 
  #   - Pallini Blu/Rossi = Correttamente classificati nella classe sani/malati
  #   - Croci = misclassificati, in particolare:
  #                                             - Croce verde: sano, classificato come malato
  #                                             - Croce viola: malato, classificato come sano
  
  
  
  
  
  ## ON THE TEST SET
  v=divide_test_set(df, portion*dim(df)[1])
  Tr=v$train
  Te=v$test
  
  model_t <- glmer(formula, data = Tr, family = binomial) #lmer model
  
  if(show_all==T)
  {print(summary(model_t))
    cat("VIF:", "\n")}
  #print(vif(model_t))
  
  
  probabilities = predict(model_t, Te,  type = "response", allow.new.levels = T)
  predictions = ifelse(probabilities > soglia, 1, 0)   ####### L'ERRORE E' QUI
  
  
  t = table(Te[[2]], predictions)
  cat("\n\n")
  cat( "OLS Performance on the test set: ", "\n")
  print(t)
  accuracy <- as.numeric((t[1,1] + t[2,2]) / sum(t))
  precision <- as.numeric(t[2,2] / (t[2,1] + t[2,2]))
  recall <- as.numeric(t[2,2] / (t[1,2] + t[2,2]))
  disease_ratio <- as.numeric((t[2,2]/sum(t[,2]))/(t[2,1]/sum(t[,1])))
  cat("Accuracy = ", accuracy, "\n", "Precision = ", precision, "\n", "Recall = ", recall, "Disease ratio = ", disease_ratio, "\n\n")
  
  
  
  if (plot){
    y = Te[[2]]
    df_sorted <- Te[order(y), ]
    y_sorted = df_sorted[[2]]
    predictions = predict(model_t, df_sorted[,-1], type = "response")
    # Crea il primo plot con i label ordinati
    y = y_sorted#df_sorted[[1]]
    plot(y, type = "n", xlab = "Non Malati || Malati", ylab = "Predicted Probabilities", main = "Predicted vs True (test set)")
    points(1:length(y), y, pch = ifelse(y == 1, 1, 19), col = ifelse(y == 1, "red", "blue"))
    
    for (i in 1:length(predictions)) {
      pred_class = ifelse(predictions[i] > soglia, 1, 0)
      if (y[i] == pred_class) {
        pch_s = 1
        if (y[i] == 1){
          color = "red"
        }else{
          color = "blue"
        }
      } else {
        pch_s = 4
        if (y[i] != 1){
          color = "green"
        }else{
          color = "purple"
        }
      }
      points(i, predictions[i], pch = pch_s,  col = color)
    }
  }
  
  return(list(table=t, model=model_t))
}
#performs a random division between train set and test set according to "portion" parameter.
#Then, runs the logistic mixed regression model written in "formula" with data belonging
#to dataset "df" in both train and test set, returning the performance indexes.
#"soglia" is the threshold above which a observation is classified as showing that side effect

jackknife_formula_glmer=function(N, df, formula, portion=0.8, soglia=0.25){
  v=c()
  tables=c()
  t_values=c()
  estimates=c()
  
  #number of repetitions you want to use for jackknife
  for (i in 1:N){
    v=perform_glmer_by_formula(df, formula, portion, soglia, plot=F, show_all = F)
    m=v$model
    t=v$table
    t_values=c(t_values, summary(m)$coefficients[, "Pr(>|z|)"])
    estimates=c(estimates, (summary(m)$coefficients)[,1])
    tables=c(tables, t)
  }
  
  indices=seq(1,length(tables), by=4)
  mean_table=matrix(c(mean(tables[indices]), mean(tables[indices+1]), mean(tables[indices+2]), mean(tables[indices+3])), 2)
  indices=seq(1,length(t_values),by=(length(t_values)/N))
  mean_t_values=c()
  mean_estimates=c()
  
  for (i in 0:(length(t_values)/N-1)){
    mean_t_values=c(mean_t_values, mean(t_values[indices+i]))
    mean_estimates=c(mean_estimates, mean(estimates[indices+i]))}
  
  
  cat("\n\n\n\n\nmean_table:\n")
  print(mean_table)
  t=mean_table
  accuracy <- as.numeric((t[1,1] + t[2,2]) / sum(t))
  precision <- as.numeric(t[2,2] / (t[2,1] + t[2,2]))
  recall <- as.numeric(t[2,2] / (t[1,2] + t[2,2]))
  disease_ratio <- as.numeric((t[2,2]/sum(t[,2]))/(t[2,1]/sum(t[,1])))
  cat("\nAccuracy = ", accuracy, "\n", "Precision = ", precision, "\n", "Recall = ", recall, "\n", "disease_ratio = ", disease_ratio)
  
  cat("\n\nmean_estimates:\n", mean_estimates)
  
  cat("\n\nmean_p_values:\n", mean_t_values)
  
  return (list(mean_t_values=mean_t_values, mean_table=mean_table))
}
#is the same concept of the function above, but repeated "N" times, selecting different train and test
#sets at each iteration. It returns the mean of each performance index after "N" iterations 

plot_curve_disease=function(df, probabilities, soglia){
  labels = df[[2]] 
  data <- data.frame(probabilities, labels)
  data <- data[order(data$probabilities), ]
  data$size=3
  #data$size <- ifelse(data$labels == 1, 5, 3)  # Assegna dimensioni diverse in base ai labels
  #i malati sono le x rosse, i sani i pallini blu
  plot <- ggplot(data, aes(x = seq_along(probabilities), y = probabilities)) +
    geom_point(aes(color = factor(labels), shape = factor(labels), size = size)) +
    scale_color_manual(values = c("0" = "blue", "1" = "red")) +
    scale_shape_manual(values = c("0" = 16, "1" = 16)) +        #4 instead of second 16
    scale_size_identity() +  # Usa size direttamente dai dati
    labs(x = "Index", y = "Probabilities", color = "Labels", shape = "Labels") +
    theme_minimal()
  print(plot)
  abline(h=soglia)
  
}
#shows the output of the logistic models, sorted from smallest to biggest and the threshold set

reduce_to1 = function(df) {
  data_copy = df
  for (i in 1:length(df)){
    data_copy[i] = min(df[i], 1)
  }
  return(data_copy)
}
reduce_to2 = function(df) {
  data_copy = df
  for (i in 1:length(df)){
    if(df[i]>=2)
      data_copy[i]=1
    else
      data_copy[i]=0
  }
  return(data_copy)
}
divide_test_set=function(df, N){
  df_to_one=df
  df_to_one[[2]]=reduce_to1(df[[2]])
  patients_indexes_test= sample(1:(dim(df)[1]), N, replace = TRUE)
  P1Tr = df[-patients_indexes_test,]
  P1Te = df[patients_indexes_test,]
  return(list(train=P1Tr, test=P1Te))
}
#these are supporting functions 

#libraries needed
library(lattice)
library(readxl)
library(lme4)
library(crayon)
library(car)
library(ggplot2)

## Models----
# Logistic mixed models for each side effect (8) and for each time point (1 and long).

# Estimated variable: side effect intensity (dichotomized: intensities above 1 have been made 1)
# Features: the ones selected at the previous step
# Logistic: because we want to let the doctor set a threshold and then see which patients are classified as
#   showing that particular side effect (that is output above the threshold) and which aren't (below)
# Why mixed models: because we can't think of all the patients as independent from each other: clearly the
#   hospital they were cured in has some influence --> random intercept: site of the patient



plot(1) #this is a useless plot, but necessary to run the models
setwd("C:/Users/133123/Dropbox/PC/Desktop/LAVORO/dataset_riassunto")


# proctitis.1----
P1=read.csv("P1.csv") #P1_removed is the dataset related to proctitis.1 (removed because patients from Karlsruhe and Mount Sinai has been removed since not enough)
attach(P1)
names(P1)[2]="proctitis.1"

formula=reduce_to1(proctitis.1) ~ z2 +
                                  haemorrhoids + 
                                  radiotherapy_toxicity_family_history +
                                  (1|Site)


model <- glmer(formula,
               data = P1, 
               family = binomial)                            

summary(model)                     


#results
m=perform_glmer_by_formula(P1, formula=formula, soglia=0.25) 
jackknife_formula_glmer(100, P1, formula)
#disease_ratio = 2.1
vif(model)
dotplot(ranef(model))

# proctitis.long----
PL=read.csv("PL.csv", sep=",") #PL_removed is the dataset related to proctitis.long (removed because patients from Karlsruhe and Mount Sinai has been removed since not enough)
attach(PL)
idxs=which(PL$proctitis.long==1) #we just remove the ones with intensity only 1 (we were told it's not meaningful to have only 1 as proctitis score)
PL[idxs, "proctitis.long"]=0

formula = reduce_to1(proctitis.long) ~ alpha_blocker + 
                                       Length_x_40_nn + 
                                       (1|Site)

model <- glmer(formula,
               data = PL, 
               family = binomial)

summary(model)          #alpha blockers are bad for proctitis????
dotplot(ranef(model))
vif(model)

m=perform_glmer_by_formula(PL, formula=formula, soglia=0.07, plot = T)

jackknife_formula_glmer(100, PL, formula, soglia=0.07)
#disease_ratio = 2.5 approximately


# management_of_sphincter_control.long ----

MSCL=read.csv("MSCL.csv") 

formula = reduce_to1(Ms1) ~ Ratio_Lx.Ly_70 + 
                            p3radio_rectum_v50_pc +  
                            (1|centers)    

model = glmer(formula, 
              data = MSCL,
              family = binomial, 
              na.action = na.omit)

summary(model)

m=perform_glmer_by_formula(na.omit(MSCL), formula=formula, soglia=0.03, plot = T)


# rectal_bleeding.1 ----

RB1=read.csv("RB1.csv")

formula=reduce_to1(M_modello_lineare.rectal_bleeding.1) ~ p3radical_prostatectomy + 
                                                          z1 +
                                                          z2 +
                                                          (1|Site)

model<-glmer(formula,
             data=RB1,
             family=binomial, 
             na.action=na.omit)

summary(model)   #having done prostatectimy is beneficial????

dotplot(ranef(model))

# rectal_bleeding.long ----

RBL=read.csv("RBL.csv")


formula=reduce_to1(M_modello_lineare.rectal_bleeding.long) ~ other_lipid_lowering_drugs +
                                                             PRSi_Rectal_bleeding +
                                                             Length_x_55 + 
                                                             Centre_x_40_nn +
                                                             (1|Site)

model <- glmer(formula,
               data = RBL, 
               family = binomial, 
               na.action = na.omit) 


summary(model)    #centre_x???
vif(model)









# urinary_incontinence.long ----

UIL=read.csv("UIL.csv") 

formula = reduce_to1(UrLong) ~ p3radical_prostatectomy + 
                               Length_y_60.x +  
                               (1|centers)                   

model = glmer(formula, 
              data = UIL,
              family = binomial, 
              na.action = na.omit)

summary(model)



# haematuria.1 ----
H1=read.csv("H1.csv")

formula=reduce_to1(M_modello_lineare.haematuria.1) ~ Ratio_Lx.Ly_55_nn +
                                                     Length_x_75 + 
                                                     p3gleason_score_a +
                                                     (1|Site)

model<-glmer(formula,
             data=H1,
             family=binomial, 
             na.action = na.omit)

summary(model)

# haematuria.long ----
HL=read.csv("HL.csv")

formula=reduce_to1(M_modello_lineare.haematuria.long) ~ ace_inhibitor + 
                                                        Length_x_75_nn +
                                                        Length_y_70 + 
                                                        Ratio_Lx.Ly_40 + 
                                                        PRSi_Haematuria +
                                                        p3pre_radio_turp +
                                                        (1|Site)
model<-glmer(formula,
             data=HL,
             family=binomial, 
             na.action = na.omit)

summary(model)

# urinary_frequency.1 ----
UF1=read.csv("UF1.csv")

formula=reduce_to2(M_modello_lineare.urinary_frequency.1) ~ PRSi_Urinary_frequency +
                                                            smoker + 
                                                            (1|Site)
model<-glmer(formula,
             data=UF1,
             family=binomial)

summary(model)





# urinary_frequency.long ----
UFL=read.csv("UFL.csv")

formula=reduce_to2(M_modello_lineare.urinary_frequency.long) ~ Length_x_60_nn + 
                                                               PRSi_Urinary_frequency +
                                                               p3gleason_score_a +
                                                               (1|Site)

model<-glmer(formula,
             data=UFL,
             family=binomial, 
             na.action=na.omit)

summary(model)




# diarrhoea.1 (mio) ----

D1=read.csv("D1.csv")

formula=reduce_to1(diarrhoea.1) ~ p3m_status +
                                  p3radio_pelvic +                          
                                  z2 +                             
                                  (1|Site)

model <- glmer(formula,
               data = D1, 
               family = binomial) 


summary(model)
vif(model)


#results
m=perform_glmer_by_formula(D1_removed, formula=formula, soglia=0.35) 
jackknife_formula_glmer(100, D1_removed, formula, soglia=0.35)
#disease_ratio = 2.06
dotplot(ranef(model))









# cantiere (pezzi di codice usati, non serve)----

setwd("C:/Users/133123/Dropbox/PC/Desktop/LAVORO/file aggiornati con indici ORIGINALE")
UF1=read.csv("urinary_frequency.1.csv")
setwd("C:/Users/133123/Dropbox/PC/Desktop/LAVORO/dataset_riassunto")

UF1=obtain_site(UF1)
UF1=UF1[-which(UF1$Site == c("Mount Sinai")),]
unique(UF1$Site)

names(UF1)[1]="id"
UF1=merge(UF1, M_0_endpoint_bladder[, c("id", "PRSi_Urinary_frequency")], "id")
HL=merge(HL, M_0_endpoint_bladder[, c("id", "Length_y_70")], "id")
HL=merge(HL, M_0_endpoint_bladder[, c("id", "Ratio_Lx.Ly_40")], "id")
HL=merge(HL, M_0_endpoint_bladder[, c("id", "PRSi_Haematuria")], "id")


setwd("C:/Users/133123/Dropbox/PC/Desktop/LAVORO/dataset_riassunto")
write.csv(HL, file = "HL.csv", row.names = FALSE) 
