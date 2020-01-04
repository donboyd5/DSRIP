
# Question:

# If we estimate an arima model with 11 fixed terms (coeff=1) and all other terms free, the standard errors on all free
# variables get quite large.

# That doesn't happen when we estimate other similar models that do not have fixed ma terms.

# Any idea why this would make the se's get so large?

# The self-contained example below estimates several models to show this effect.


library("forecast")
library("lmtest") # for coeftest

data <- 
  structure(
    list(MSR_RESULT = c(60.5278146952724, 60.3247984826932, 
                        60.1407382836564, 60.0761175330355, 59.9250936329588, 59.7821730828167, 
                        58.8889651857447, 58.7634556996964, 58.772697944511, 58.7064791330413, 
                        58.5307837975151, 58.1065876788133, 58.0735960455856, 57.9819411084423, 
                        57.8800496102055, 57.9364199037203, 57.6903625849293, 57.596317984399, 
                        60.517703692756, 60.5139552846377, 60.6443448676458, 60.4559358465608, 
                        60.7944660757187, 60.8826708797605, 60.8991853996958, 61.1009249073055, 
                        60.81575417632, 60.6088504265635, 60.8347498412828, 61.0450723244062, 
                        60.7150434967112, 60.3931747585209, 60.3390481883981, 60.4658158222681, 
                        60.3554369809372, 60.3449757271126, 60.1943839837942, 61.3828409301178, 
                        61.4688752116981, 61.3309502036744, 61.3541926369152, 61.3760330578512, 
                        61.2425658453696, 61.1893285980044, 61.441361167685, 61.4361419115926, 
                        61.5233750453005, 61.442980694001, 61.58026233604, 61.2270984235194, 
                        61.1393050035667, 61.1318522145378, 61.1009286412512, 61.1560201786908, 
                        61.5019281510047, 61.2505075111652, 61.6310513795417),
         
         time_ma = c(-17.5, 
                     -16.5, -15.5, -14.5, -13.5, -12.5, -11.5, -10.5, -9.5, -8.5, 
                     -7.5, -6.5, -5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 
                     3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 
                     15.5, 16.5, 17.5, 18.5, 19.5, 20.5, 21.5, 22.5, 23.5, 24.5, 25.5, 
                     26.5, 27.5, 28.5, 29.5, 30.5, 31.5, 32.5, 33.5, 34.5, 35.5, 36.5, 
                     37.5, 38.5), 
         
         dsrip_ma = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                      0, 0.0833333333333333, 0.166666666666667, 0.25, 0.333333333333333, 
                      0.416666666666667, 0.5, 0.583333333333333, 0.666666666666667, 
                      0.75, 0.833333333333333, 0.916666666666667, 1, 1, 1, 1, 1, 1, 
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                      1, 1, 1, 1, 1, 1), 
         
         interaction_ma = c(0, 0, 0, 0, 0, 0, 0, 0, 
                            0, 0, 0, 0, 0, 0.0833333333333333, 0.25, 0.5, 0.833333333333333, 
                            1.25, 1.75, 2.33333333333333, 3, 3.75, 4.58333333333333, 5.5, 
                            6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 
                            17.5, 18.5, 19.5, 20.5, 21.5, 22.5, 23.5, 24.5, 25.5, 26.5, 27.5, 
                            28.5, 29.5, 30.5, 31.5, 32.5, 33.5, 34.5, 35.5, 36.5, 37.5, 38.5
         )), 
    
    row.names = c(NA, -57L), 
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))

data

xvars_ma <- as.matrix(data[, c("time_ma", "dsrip_ma", "interaction_ma")])



# simple ar1 model -- generally a good fit for these LHS variables, and what auto.arima chooses
modyy1 <- arima(data$MSR_RESULT, xreg=xvars_ma, order=c(1, 0, 0))
summary(modyy1)
# Coefficients:
#          ar1  intercept  time_ma  dsrip_ma  interaction_ma
#       0.5448    56.7993  -0.2132    3.9887          0.2287
# s.e.  0.1090     0.4804   0.0415    0.6390          0.0411
# 
# sigma^2 estimated as 0.1545:  log likelihood = -27.84,  aic = 67.68
coeftest(modyy1) # z test -- se's match


# ma11 fixed coefficients -- notice:
#  1) the much higher se's on the free variables
#  2) the rise in aic
modyy2 <- arima(df$MSR_RESULT, xreg=xvars_ma, order=c(0, 0, 11), fixed = c(rep(1, 11), NA, NA, NA, NA ))
summary(modyy2)
# Coefficients:
#       ma1  ma2  ma3  ma4  ma5  ma6  ma7  ma8  ma9  ma10  ma11  intercept  time_ma  dsrip_ma  interaction_ma
#         1    1    1    1    1    1    1    1    1     1     1    57.5474  -0.1523    2.6807          0.1938
# s.e.    0    0    0    0    0    0    0    0    0     0     0     2.0478   0.1545    2.6007          0.1643
# 
# sigma^2 estimated as 0.1731:  log likelihood = -41.65,  aic = 93.31
coeftest(modyy2) # gives the same se's


# ma11 but now the ma coeffs are free
# Note that the se's on the non-ma terms are again small
modyy3 <- arima(df$MSR_RESULT, xreg=xvars_ma, order=c(0, 0, 11))
summary(modyy3)
# Coefficients:
#          ma1     ma2      ma3      ma4      ma5      ma6      ma7      ma8      ma9    ma10    ma11  intercept  time_ma  dsrip_ma  interaction_ma
#       0.4931  0.2346  -0.1201  -0.3567  -0.4575  -0.4304  -0.1433  -0.2940  -0.3461  0.0756  0.3448    57.0622  -0.1905    3.5993          0.2103
# s.e.  0.1434  0.1941   0.1927   0.1776   0.1952   0.1914   0.1512   0.1568   0.2022  0.2072  0.1581     0.3040   0.0311    0.4064          0.0288
# sigma^2 estimated as 0.1059:  log likelihood = -21.15,  aic = 74.31
coeftest(modyy3) # z test is default with arima
coeftest(modyy3, df=Inf) # Inf --> z test rather than t test 


# ar1 free and ma11 coeffs fixed
# the se's on variables of interest are large when the ma terms are fixed
modyy4 <- arima(df$MSR_RESULT, xreg=xvars_ma, order=c(1, 0, 11), fixed = c(NA, rep(1, 11), NA, NA, NA, NA ))
summary(modyy4)

