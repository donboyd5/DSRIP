# https://stackoverflow.com/questions/7233288/non-stationary-seasonal-ar-part-from-css-error-in-r

glimpse(results)

tmp <- results %>%
  select(PPS_ID, PPS_NAME, MSR_RESULT_ID, MSR_RESULT_NAME, starts_with("ar"), starts_with("ma"), -ends_with("pvalue")) %>%
  setNames(str_remove(names(.), ".coeff"))
glimpse(tmp)

tmp %>%
  pivot_longer(-c(PPS_ID, PPS_NAME, MSR_RESULT_ID, MSR_RESULT_NAME)) %>%
  group_by(name) %>%
  summarise(n=sum(!is.na(value)))

# name      n
# <chr> <int>
#   1 ar1     554
# 2 ar2     122
# 3 ar3      23
# 4 ar4       2
# 5 ma1     101
# 6 ma2      48
# 7 ma3       7
# 8 ma4       2


fit = map(data, ~ arima(.$MSR_RESULT, 
                        xreg = as.matrix(.[, xvars]),
                        order=c(3, 0, 11),
                        fixed = c(rep(1, 11), NA, NA, NA, NA )))


xvars <- c("time_ma", "dsrip_ma", "interaction_ma")
d <- regdata %>%
  filter(PPS_ID %in% c(1), MSR_RESULT_ID=="PDI90RES") %>%
  arrange(PPS_ID, MSR_RESULT_ID, time)

arima(d$MSR_RESULT, 
      xreg = as.matrix(d[, xvars]),
      order=c(3, 0, 11),
      fixed = c(NA, NA, NA, rep(1, 11), NA, NA, NA, NA ))

m0 <- arima(d$MSR_RESULT, order=c(0, 0, 0))
m1 <- arima(d$MSR_RESULT, order=c(0, 1, 0))
summary(m1)

Acf(residuals(m0))
Acf(residuals(m1))


Pacf(residuals(m0))
Pacf(residuals(m1))

mx1 <- arima(d$MSR_RESULT, order=c(3, 1, 0))
summary(mx1)
str(mx1)

mx2 <- arima(d$MSR_RESULT, order=c(3, 1, 1))
summary(mx2)
str(mx2)

mx3 <- arima(d$MSR_RESULT, order=c(3, 1, 1), fixed=c(NA, NA, NA, 1))
summary(mx3)
str(mx3)

mx3a <- arima(d$MSR_RESULT, order=c(3, 1, 1), fixed=c(NA, NA, NA, 1), method="ML")
summary(mx3a)
str(mx3a)

mx3b <- arima(d$MSR_RESULT, order=c(3, 0, 1), fixed=c(NA, NA, NA, 1, NA), method="ML")
summary(mx3b)
str(mx3b)


mx4 <- arima(d$MSR_RESULT, order=c(0, 1, 1), fixed=c(1))
summary(mx4)
str(mx4)

mx5 <- arima(d$MSR_RESULT, 
             xreg = as.matrix(d[, xvars]),
             order=c(3, 1, 1), 
             fixed=c(NA, NA, NA, 1, NA, NA, NA))
summary(mx5)
str(mx5)

mx3b <- arima(d$MSR_RESULT, order=c(3, 0, 1), fixed=c(NA, NA, NA, 1, NA), method="ML")
summary(mx3b)
str(mx3b)


mx6 <- arima(d$MSR_RESULT, 
             xreg = as.matrix(d[, xvars]),
             order=c(3, 0, 1), 
             fixed=c(NA, NA, NA, 1, NA, NA, NA, NA),
             method="ML")
summary(mx6)
str(mx6)

mx7 <- arima(d$MSR_RESULT, 
             xreg = as.matrix(d[, xvars]),
             order=c(3, 0, 11), 
             fixed=c(NA, NA, NA, rep(1, 11), NA, NA, NA, NA),
             method="ML")
summary(mx7)
str(mx7)

mx8 <- arima(d$MSR_RESULT, 
             xreg = as.matrix(d[, xvars]),
             order=c(1, 0, 11), 
             fixed=c(NA, rep(1, 11), NA, NA, NA, NA),
             method="ML")
summary(mx8)
str(mx8)

mx8a <- arima(d$MSR_RESULT, 
             xreg = as.matrix(d[, xvars]),
             order=c(1, 0, 11), 
             fixed=c(NA, NA, rep(1, 10), NA, NA, NA, NA),
             method="ML")
summary(mx8a)
str(mx8a)


mod <- mx8a; nfree_vars <- nrow(mod$var.coef); (dgfr <- mod$nobs - nfree_vars)



djb2 <- auto.arima(d$MSR_RESULT, xreg=as.matrix(d[, xvars]))
summary(djb2)

d3 <- arima(d$MSR_RESULT, 
            xreg = as.matrix(d[, xvars]),
            order=c(1, 0, 1),
            fixed=c(NA, 0, NA, NA, NA, NA))
summary(d3)

d4 <- arima(d$MSR_RESULT, 
            xreg = as.matrix(d[, xvars]),
            order=c(1, 0, 1),
            fixed=c(NA, NA, NA, NA, NA, NA))
summary(d4)


test <- readRDS(here::here("results", "regressions_automa.rds"))
glimpse(test)

t2 <- test %>%
  unnest(glanced)
glimpse(t2)

count(t2, term)

t2 <- test %>%
  filter(PPS_ID==1, MSR_RESULT_ID=="PDI90RES")
glimpse(t2)
str(t2$fit)

t3 <- test %>%
  select(-data, -tidied, -glanced, -ztest) %>%
  # filter(row_number()<3) %>%
  mutate(diff=ifelse(is.null(fit[[1]]$model$Delta), 0, fit[[1]]$model$Delta),
         phi=ifelse(is.null(fit[[1]]$model$phi[1]), 0, fit[[1]]$model$phi[1]))
t3

t4 <- test %>%
  select(-data, -tidied, -glanced, -ztest) %>%
  # filter(row_number()<3) %>%
  mutate(nobs=fit[[1]]$nobs)
count(t4, nobs)



mx1$coef; mx2$coef

2*(1-pt(0.2028/0.0461, NROW(fpp2::uschange)-5)) = 1.8e-5




auto.arima(d$MSR_RESULT)
djb <- auto.arima(d$MSR_RESULT)
str(djb)

d <- regdata %>%
  filter(PPS_ID %in% c(1), MSR_RESULT_ID=="PDI90RES") %>%
  arrange(PPS_ID, MSR_RESULT_ID, time)

djb2 <- auto.arima(d$MSR_RESULT, xreg=as.matrix(d[, xvars]))
summary(djb2)
str(djb2)

dgfr <- djb2$nobs - length(djb2$coef)

mod <- djb2
nfree_vars <- nrow(mod$var.coef)
(dgfr <- mod$nobs - nfree_vars)


2*(1-pt(0.2028/0.0461, NROW(fpp2::uschange)-5)) = 1.8e-5




xvars <- c("time_ma", "dsrip_ma", "interaction_ma")
d <- regdata %>%
  filter(PPS_ID %in% c(1), MSR_RESULT_ID=="PDI90RES") %>%
  arrange(PPS_ID, MSR_RESULT_ID, time)
d

mod1 <- lm(MSR_RESULT ~ time_ma + dsrip_ma + interaction_ma, data=d)
mod <- mod1; dgfr <- mod$df.residual

summary(mod)
lmtest::coeftest(mod)
lmtest::coeftest(mod, df=dgfr)

# repeat with arima version
mod2 <- arima(d$MSR_RESULT, xreg=d[xvars], order=c(0, 0, 0))

mod <- mod2
nfree_vars <- nrow(mod$var.coef)
(dgfr <- mod$nobs - nfree_vars)
lmtest::coeftest(mod)
lmtest::coeftest(mod, df=dgfr)





d2 <- readRDS(here::here("results", "regressions_mafixed.rds"))

a <- proc.time()
xvars <- c("time_ma", "dsrip_ma", "interaction_ma")
check <- regdata %>%
  # additional filtering if desired:
  # filter(PPS_ID %in% (c(1:99)) %>%
  arrange(PPS_ID, MSR_RESULT_ID, time) %>%
  nest(data=-c(MSR_RESULT_ID, MSR_RESULT_NAME, PPS_ID, PPS_NAME)) %>%
  mutate(
    fit = map(data, ~ arima(.$MSR_RESULT, 
                            xreg = as.matrix(.[, xvars]),
                            order=c(2, 0, 11),
                            fixed = c(NA, NA, rep(1, 11), NA, NA, NA, NA ))),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    ztest=map(fit, ztest))
b <- proc.time()
b - a # 20 seconds for 605 regressions
saveRDS(check, here::here("results", "check.rds"))

glimpse(check)

check$fit[[1]]$var.coef


d2 %>% ht
d2 <- readRDS(here::here("results", "check.rds"))

d2a <- d2 %>%
  mutate(ztest3=map(fit, ztest3))

d2b <- d2a %>%
  unnest(ztest3) %>%
  select(PPS_ID, PPS_NAME, MSR_RESULT_ID, MSR_RESULT_NAME, term, estimate, p.value) %>%
  pivot_longer(c(estimate, p.value)) %>%
  mutate(name=ifelse(name=="estimate", "coeff", "pvalue")) %>%
  unite(combo, term, name, sep=".") %>%
  pivot_wider(names_from = "combo") %>%
  left_join(measures %>% select(MSR_RESULT_ID, improve))
glimpse(d2b)

d2c <- get_sig(d2b, "dsrip_ma", "interaction_ma", .01)
glimpse(d2c)
count(d2c, sig_type)
mean(d2c$good)
