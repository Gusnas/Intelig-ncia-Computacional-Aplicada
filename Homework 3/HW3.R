--------- QUESTÃO 1 ------------
  
  #Preparando_os_dados
  trainingReduction <- training[,reducedSet]
  testingReduction <- testing[,reducedSet]
  
  #Regressão_Logística  
  attach(trainingReduction)
  glm.fits <- glm(training$Class~.,data = trainingReduction, family = binomial)
  
  coef(glm.fits)
  #Criando_o_modelo_preditivo_para_o_conjunto_de_treino
  glm.probs=predict(glm.fits,newdata = testingReduction, type="response")
  glm.probs[1:10]
  
  #Criando_variáveis_binárias_de_'Class'
  contrasts(training$Class)
  
  #Aplicando_o_limiar_para_binarizar_os_dados
  plim <-0.5
  glm.pred=rep("successful",length(testing$Class))
  glm.pred[glm.probs > plim]="unsuccessful"
  head(glm.pred)
  
  cmTabGLM <- table(glm.pred, testing$Class)
  sum(diag(cmTabGLM))/sum(cmTabGLM)
  mean(glm.pred==testingReduction$Class)
  
  confusionMatrix(cmTabGLM)
  
  #Linear_Discriminant_Analysis(LDA)
  library(MASS)
  lda.fits <- lda(training$Class~., data = trainingReduction)
  lda.probs <- predict(lda.fits,newdata = testingReduction, type="response")
  # Predicted_classes
  head(lda.probs$class, 6)
  # Predicted_probabilities_of_class_memebership.
  head(lda.probs$posterior, 6)
  # Linear_discriminants
  head(lda.probs$x, 3)
  
  cmTabLDA <- table(lda.probs$class, testing$Class)
  sum(diag(cmTabLDA))/sum(cmTabLDA)
  mean(lda.probs$class==testing$Class)
  
  -----------------QUESTAO 2 -----------------------
    
    
    ----QDA ----- 
    
    library(MASS)
  training.Reduced <- training[,reducedSet]
  test.Reduced <- testing[,reducedSet]
  training.Reduced$Class <- training$Class
  test.Reduced$Class <- testing$Class
  
  qda.fit <- qda(Class ~.,data =training.Reduced)
  qda.class <- predict(qda.fit,test.Reduced)
  table(qda.class$class,test.Reduced$Class)
  
  mean(qda.class$class==test.Reduced$Class)
  
  -----KNN-------
    
    library(caret)
  library(class)
  
  training.Reduced <- training[,reducedSet]
  testing.Reduced <- testing[,reducedSet]
  
  preTraining <- preProcess(training.Reduced, method = c("center","scale"))
  standardized.Train <- predict(preTraining,training.Reduced) 
  
  preTesting <- preProcess(testing.Reduced, method = c("center","scale"))
  standardized.Test <- predict(preTesting,testing.Reduced) 
  
  set.seed(1)
  knn.fit1 <- knn(standardized.Train,standardized.Test,training$Class,k=1)
  knn.fit3 <- knn(standardized.Train,standardized.Test,training$Class,k=3)
  knn.fit6 <- knn(standardized.Train,standardized.Test,training$Class,k=6)
  
  table(knn.fit1,testing$Class)
  table(knn.fit3,testing$Class)
  table(knn.fit6,testing$Class)
  
  mean(knn.fit1==testing$Class)
  mean(knn.fit3==testing$Class)
  mean(knn.fit6==testing$Class)
  
  ---------- NEURAL NETWORK ----------
    
    training.Reduced <- training[,reducedSet]
  testing.Reduced <- testing[,reducedSet]
  training.Reduced$Class <- training$Class
  testing.Reduced$Class <- testing$Class
  
  install.packages("neuralnet")
  library(neuralnet)
  attach(training.Reduced)
  
  nn = neuralnet(Class ~ NumCI + NumDR + NumECI + NumPS + NumSCI + NumSR + NumUNK + CI.1940 + CI.1945 + CI.1950 + CI.1955 + DR.1955 + PS.1955 + CI.1960 + PS.1960 + CI.1965 + PS.1965 + CI.1970 + CI.1975 + CI.1980 + CI.AsiaPacific + CI.Australia + DR.Australia + PS.Australia + CI.EasternEurope + CI.GreatBritain + CI.MiddleEastandAfrica + CI.NewZealand + CI.NorthAmerica + CI.SouthAfrica + CI.TheAmericas + CI.WesternEurope + CI.English + DR.English + PS.English + CI.OtherLang + CI.PhD + DR.PhD + PS.PhD + Success.CI + Unsuccess.CI + Success.DR + Success.PS + Unsuccess.PS + CI.Dept1033 + CI.Dept1038 + CI.Dept1098 + CI.Dept1258 + CI.Dept2053 + CI.Dept2103 + CI.Dept2153 + CI.Dept2163 + CI.Dept2178 + CI.Dept2253 + CI.Dept2498 + CI.Dept2523 + CI.Dept2533 + CI.Dept2538 + CI.Dept2553 + CI.Dept2558 + CI.Dept2563 + CI.Dept2578 + CI.Dept2603 + CI.Dept2628 + CI.Dept2653 + CI.Dept2668 + CI.Dept2678 + PS.Dept2678 + CI.Dept2713 + CI.Dept2728 + CI.Dept2763 + CI.Dept2768 + CI.Dept2813 + CI.Dept2828 + CI.Dept2853 + CI.Dept2893 + CI.Dept2923 + CI.Dept3028 + CI.Dept3048 + CI.Dept3098 + CI.Dept3123 + CI.Dept3198 + CI.Dept3258 + CI.Dept3268 + CI.Dept528 + CI.Dept593 + CI.Dept653 + CI.Dept803 + CI.Dept828 + CI.Faculty1 + CI.Faculty13 + CI.Faculty19 + CI.Faculty22 + CI.Faculty25 + DR.Faculty25 + PS.Faculty25 + CI.Faculty31 + PS.Faculty31 + CI.Faculty34 + CI.Faculty4 + CI.Faculty46 + CI.Faculty7 + Duration0to5 + Duration10to15 + Duration5to10 + DurationGT15 + DurationLT0 + DurationUnk + Astar.CI + A.CI + B.CI + C.CI + Astar.PS + A.PS + C.PS + AstarTotal + ATotal + BTotal + CTotal + RFCD250103 + RFCD270103 + RFCD270106 + RFCD270199 + RFCD270201 + RFCD270299 + RFCD270603 + RFCD270708 + RFCD320202 + RFCD320305 + RFCD320502 + RFCD320602 + RFCD320701 + RFCD320702 + RFCD320799 + RFCD320899 + RFCD321003 + RFCD321004 + RFCD321006 + RFCD321010 + RFCD321013 + RFCD321014 + RFCD321015 + RFCD321016 + RFCD321021 + RFCD321024 + RFCD321028 + RFCD321029 + RFCD321202 + RFCD321204 + RFCD321206 + RFCD321208 + RFCD321216 + RFCD321299 + RFCD380103 + RFCD430101 + SEO670401 + SEO670403 + SEO671401 + SEO700103 + SEO730101 + SEO730102 + SEO730104 + SEO730105 + SEO730106 + SEO730107 + SEO730108 + SEO730109 + SEO730110 + SEO730111 + SEO730113 + SEO730114 + SEO730115 + SEO730116 + SEO730118 + SEO730199 + SEO730201 + SEO730202 + SEO730203 + SEO730204 + SEO730206 + SEO730211 + SEO730213 + SEO730214 + SEO730218 + SEO730219 + SEO730299 + SEO730303 + SEO730305 + SEO730306 + SEO740201 + SEO740301 + SEO750901 + SEO750902 + SEO770101 + SEO770703 + SEO780101 + SEO780102 + SEO780103 + SEO780105 + SEO780108 + Sponsor149A + Sponsor24D + Sponsor29A + Sponsor2B + Sponsor32D + Sponsor34B + Sponsor40D + Sponsor4D + Sponsor59C + Sponsor5A + Sponsor60D + Sponsor62B + Sponsor6B + Sponsor75C + Sponsor97A + ContractValueBandA + ContractValueBandB + ContractValueBandC + ContractValueBandD + ContractValueBandE + ContractValueBandF + ContractValueBandG + ContractValueBandH + ContractValueBandUnk + GrantCat10A + GrantCat10B + GrantCat20A + GrantCat20C + GrantCat30B + GrantCat30C + GrantCat30D + GrantCat30G + GrantCat50A + GrantCatUnk + Apr + Aug + Dec + Feb + Jan + Jul + Jun + May + Nov + Oct + Sep + Fri + Mon + Sat + Thurs + Tues + Wed + Day, data = training.Reduced, hidden = c(2,1), stepmax=1e6, linear.output = FALSE, threshold = 0.3)
  
  plot(nn)
  contrasts(testing.Reduced$Class)
  Predict=compute(nn, testing.Reduced)
  results <- data.frame(real = testing.Reduced$Class, predicao = Predict$net.result)
  results.pred = rep("sucessfull",length(testing.Reduced$Class))
  plim <- 0.5
  results.pred[results$predicao.1 < plim]="unsucessfull"
  confusion <- table(results.pred, testing.Reduced$Class)
  sum(diag(confusion))/sum(confusion)
  