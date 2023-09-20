factor_inorder <- function(x) return(factor(x,levels=unique(x)))
posterior_summary <- function(x) return(list(mean=mean(x),lb=quantile(x,0.025),ub=quantile(x,0.975)))
parse_block <- function(x) return(data.table(inc_prop = str_extract(x,"Inc\\d."), switch_prop = str_extract(x,"Switch\\d.")))
#pick up from Exp1_CrossTaskInterference.R line 35

fitlmer <- function(SD) {
  fit <- lmer(RT ~ 1 + stimCongruency*switchType + (1 + stimCongruency*switchType|subject),data=SD)
  coef <- fixef(fit)
  sdev <- vcov(fit) |> diag() |> sqrt()
  return(data.table(effect=names(coef),coef=coef,sdev=sdev))
}

dt_trimmed <- as.data.table(df_trimmed)
dt_coeffs <- dt_trimmed[,fitlmer(.SD),by=.(switch_perc,congruency_perc)]
ggplot(dt_coeffs,aes(y=coef,ymin=coef-sdev,ymax=coef+sdev,x=congruency_perc,color=switch_perc)) + 
  geom_pointrange(position = position_dodge(width=.25)) + facet_wrap(vars(effect),scales="free") + theme_classic()

dt_trimmed[,switch_perc := ifelse(switch_perc=="75% Switch","Switch75","Switch25")]
dt_trimmed[,congruency_perc := ifelse(congruency_perc=="75% Incongruent","Inc75","Inc25")]

foo <- model.matrix( ~ 1 + stimCongruency*switchType*congruency_perc*switch_perc,data=dt_trimmed)[,-1] |> as.data.table()
pnames <- names(foo) |> paste(collapse=" + ")
foo$subject <- dt_trimmed$subject
foo$RT <- dt_trimmed$RT

standat <- list(Y = dt_trimmed[,scale(RT)] |> as.vector(), X = model.matrix( ~ 1 + stimCongruency*switchType,data=dt_trimmed), 
                Block = dt_trimmed[,paste(congruency_perc,switch_perc,sep=":")] |> factor_inorder(),
                Subj = factor_inorder(dt_trimmed$subject))
ref <- list(subj = unique(standat$Subj),
            block = unique(standat$Block),
            effect = colnames(standat$X) |> factor_inorder())
standat <- c(standat,list(
  N = length(standat$Y),
  P = ncol(standat$X),
  N_Subj = uniqueN(standat$Subj),
  N_Block = uniqueN(standat$Block)
))
standat$Subj <- as.numeric(standat$Subj)
standat$Block <- as.numeric(standat$Block)
model <- stan_model("mvn_reg.stan")
fit <- sampling(model,standat,iter=400,chains=4)
fit <- recover_types(fit,ref)

effdt <- gather_draws(fit,mu_beta[effect,block]) |> setDT()
effdt <- effdt[,posterior_summary(.value),by=.(block,effect)]
effdt <- cbind(effdt,parse_block(effdt$block))
ggplot(effdt,aes(y=mean,ymin=lb,ymax=ub,x=inc_prop,color=switch_prop)) + 
  geom_pointrange(position = position_dodge(width=0.5)) + facet_wrap(vars(effect),scales = "free")

intdt <- spread_draws(fit,mu_beta[effect,block] | block)
intdt[,(`Inc75:Switch75`-`Inc75:Switch25`) - (`Inc25:Switch75`-`Inc25:Switch25`),by=.(effect)] |> 
  ggplot(aes(x=V1)) + geom_histogram(bins=100) + facet_wrap(vars(effect),scales="free") + geom_vline(xintercept = 0,color="red")

intdt[,(`Inc75:Switch75` + `Inc75:Switch25` + `Inc25:Switch75` + `Inc25:Switch25`)/4,by=.(effect)] |> 
  ggplot(aes(x=V1)) + geom_histogram(bins=100) + facet_wrap(vars(effect),scales="free") + geom_vline(xintercept = 0,color="red")
