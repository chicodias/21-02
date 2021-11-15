
```{r}
library(mlbench)
library(caret)
```


```{r}
set.seed(42)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(y~., data=dados, method = 'lm', preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)

# plot importance
plot(importance)
```
```{r}
c(  "Tamanho", "Temp", "Pressao", "FluxoCO2", "Umidade")
```

```{r}
base.mod <- lm(y ~ 1 , data= dados)  # base intercept only model
all.mod <- lm(y ~ . , data= dados) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)
```
```{r}
stepMod$anova
```
