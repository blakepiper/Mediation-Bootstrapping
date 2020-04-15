library(boot)

##Defining Terms for Sobel and Bootstrapping
a = a_path$coefficients[2]
b = b_path$coefficients[3]
SEa = coef(summary(a_path))[ , "Std. Error"][2]
SEb = coef(summary(b_path))[ , "Std. Error"][2]
Sobel_Z = (a*b)/sqrt((b^2*SEa^2)+(a^2*SEb^2)+(SEa*SEb))
total = c_path$coefficients[2]
direct = b_path$coefficients[2]
indirect = total - direct
percent_mediated = round((indirect/total)*100, 2)

find_Sobel_Z <- function(dataset, random) {
  d = dataset[random, ]
  apath = lm(mentHapp ~ StatSkill, data=d)
  bpath = lm(selfHapp ~ StatSkill + mentHapp, data=d)
  a = apath$coefficients[2]
  b = bpath$coefficients[3]
  SEa = coef(summary(apath))[ , "Std. Error"][2]
  SEb = coef(summary(bpath))[ , "Std. Error"][2]
  Sobel_Z_cal = (a*b)/sqrt((b^2*SEa^2)+(a^2*SEb^2)+(SEa*SEb))
  return(Sobel_Z_cal)
}

indirecteffect <- function(dataset, random){
  dat = dataset[random, ]
  apath1 = lm(mentHapp ~ StatSkill, data=dat)
  bpath1 = lm(selfHapp ~ StatSkill + mentHapp, data=dat)
  a = apath1$coefficients[2]
  b = bpath1$coefficients[3]
  SEa = coef(summary(apath1))[ , "Std. Error"][2]
  SEb = coef(summary(bpath1))[ , "Std. Error"][2]
  indirect = apath1$coefficients[2]*bpath1$coefficients[3]
  return(indirect)
}

calc_Sobel_SE <- function(dataset, random){
  dat = dataset[random, ]
  apath1 = lm(mentHapp ~ StatSkill, data=dat)
  bpath1 = lm(selfHapp ~ StatSkill + mentHapp, data=dat)
  a = apath1$coefficients[2]
  b = bpath1$coefficients[3]
  SEa = coef(summary(apath1))[ , "Std. Error"][2]
  SEb = coef(summary(bpath1))[ , "Std. Error"][2]
  SE_Pooled = sqrt((a^2*SEb) + (a^2*SEa))
  return(SE_Pooled)
}

##Bootstrapping
#Boot Sobel Z 
boot_Sobel <- boot(data=noout,
                   statistic = find_Sobel_Z,
                   R = 5000)
boot_Sobel_CI <- boot.ci(boot_Sobel, conf = .95, type = "norm")

#Boot Indirect Effect
boot_indirect <- boot(data=noout,
                      statistic = indirecteffect,
                      R = 5000)

boot_indirect_CI <- boot.ci(boot_indirect, conf = .95, type = "norm")

#Boot SE of Indirect Effect

boot_SE_Pooled <- boot(data=noout,
                       statistic = calc_Sobel_SE,
                       R = 5000)

boot_indirect_CI <- boot.ci(boot_SE_Pooled, conf = .95, type = "norm")