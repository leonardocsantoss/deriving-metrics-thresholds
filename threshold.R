library("ggplot2")
library("moments")
library("gridExtra")
library("boot")
source("multiplot.R")

# read csv
df = read.csv('metrics.csv', header=T, sep=";")

#### Experimento 2
# Projetos usados: camel (1.4, 1.6), jEdit (4.0, 4.1, 4.2, 4.3), lucene (2.0, 2.2, 2.4), pBeans (1.0, 2.0), 
# poi (3.0), synapse (1.1, 1.2), Tomcat (6.0.389418), velocity (1.4, 1.5, 1.6.1), xalan (2.4.0, 2.5.0, 2.6.0, 2.7.0) E xerces (1.4.4)
df.exp = df[df$Project == 'camel' & df$Version %in% c('1.4', '1.6'), ]
df.exp = rbind(df.exp, df[df$Project == 'jEdit' & df$Version == '3.2.1', ])
df.exp = rbind(df.exp, df[df$Project == 'camel' & df$Version %in% c('1.4', '1.6'), ])
df.exp = rbind(df.exp, df[df$Project == 'jEdit' & df$Version %in% c('4.0', '4.1', '4.2', '4.3'), ])
df.exp = rbind(df.exp, df[df$Project == 'lucene' & df$Version %in% c('2.0', '2.2', '2.4'), ])
df.exp = rbind(df.exp, df[df$Project == 'pBeans' & df$Version %in% c('1.0', '2.0'), ])
df.exp = rbind(df.exp, df[df$Project == 'poi' & df$Version %in% c('3.0'), ])
df.exp = rbind(df.exp, df[df$Project == 'synapse' & df$Version %in% c('1.1', '1.2'), ])
df.exp = rbind(df.exp, df[df$Project == 'Tomcat' & df$Version %in% c('6.0.389418'), ])
df.exp = rbind(df.exp, df[df$Project == 'velocity' & df$Version %in% c('1.4', '1.5', '1.6.1'), ])
df.exp = rbind(df.exp, df[df$Project == 'xalan' & df$Version %in% c('2.4.0', '2.5.0', '2.6.0', '2.7.0'), ])
df.exp = rbind(df.exp, df[df$Project == 'xerces' & df$Version %in% c('1.4.4'), ])

# Thresholds: 80% dos dados; Validação 20% dos dados.
df.exp.thresholds.all = df.exp[sample(1:nrow(df.exp), nrow(df.exp)*0.8), ]
df.exp.thresholds = df.exp.thresholds.all
df.exp.validation = df.exp[! rownames(df.exp) %in% rownames(df.exp.thresholds), ]

## Gerando os threhoslds
# Removendo classes com bugs
df.exp.thresholds = df.exp.thresholds[df.exp.thresholds$bugs <= 1, ]

### Calculate thresholds
threshold_calc = function(data, name, conf=c(0.8, 0.9, 0.95)){
  # convert 0 to 1
  data[data == 0] = 1
  # log conver
  data = log(data)
  ## threshold using bootstrap
  # Função de estatística, usamos todas as observações, retornando apenas o valor da métrica
  f = function(d, i){
    return(d[i])
  }
  bootloc = boot(data, f, R=10000)
  bootthreshold = boot.ci(bootloc, conf=conf, type="perc")
  return (data.frame(
      metric=name,
      P1=mean(c(exp(bootthreshold$percent[1, ][4]), exp(bootthreshold$percent[1, ][5]))),
      P2=mean(c(exp(bootthreshold$percent[2, ][4]), exp(bootthreshold$percent[2, ][5]))),
      P3=mean(c(exp(bootthreshold$percent[3, ][4]), exp(bootthreshold$percent[3, ][5])))
  ))
}

# Define new data.frame for thresholds
threshold = data.frame(metric=NULL, P1=NULL, P2=NULL, P3=NULL)

# Calcular Thresholds: cbo, dit, noc, lcom, rfc, wmc
threshold = rbind(threshold, threshold_calc(df.exp.thresholds$cbo, 'cbo', conf=c(0.8, 0.9, 0.95)))
threshold = rbind(threshold, threshold_calc(df.exp.thresholds$dit, 'dit', conf=c(0.7, 0.8, 0.9)))
threshold = rbind(threshold, threshold_calc(df.exp.thresholds$noc, 'noc', conf=c(0.8, 0.9, 0.95)))
threshold = rbind(threshold, threshold_calc(df.exp.thresholds$lcom, 'lcom', conf=c(0.7, 0.8, 0.9)))
threshold = rbind(threshold, threshold_calc(df.exp.thresholds$rfc, 'rfc', conf=c(0.8, 0.9, 0.95)))
threshold = rbind(threshold, threshold_calc(df.exp.thresholds$wmc, 'wmc', conf=c(0.8, 0.9, 0.95)))


## plots
df.exp.thresholds.all$cbo[df.exp.thresholds.all$cbo == 0] = 1

# CBO
plot.exp.cbo_before = ggplot(df.exp.thresholds.all, aes(cbo)) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="CBO histogram before log transformation", x="CBO", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(df.exp.thresholds.all$cbo), sd=sd(df.exp.thresholds.all$cbo)), colour="blue", size=1, lty=2)
df.exp.thresholds.all$cbo[df.exp.thresholds.all$cbo == 0] = 1
plot.exp.cbo_after = ggplot(df.exp.thresholds.all, aes(log(cbo))) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="CBO histogram after log transformation", x="CBO", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(log(df.exp.thresholds.all$cbo)), sd=sd(log(df.exp.thresholds.all$cbo))), colour="blue", size=1, lty=2)

multiplot(plot.exp.cbo_before, plot.exp.cbo_after, cols=2)

# DIT
plot.exp.dit_before = ggplot(df.exp.thresholds.all, aes(dit)) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="DIT histogram before log transformation", x="DIT", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(df.exp.thresholds.all$dit), sd=sd(df.exp.thresholds.all$dit)), colour="blue", size=1, lty=2)
df.exp.thresholds.all$dit[df.exp.thresholds.all$dit == 0] = 1
plot.exp.dit_after = ggplot(df.exp.thresholds.all, aes(log(dit))) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="DIT histogram after log transformation", x="DIT", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(log(df.exp.thresholds.all$dit)), sd=sd(log(df.exp.thresholds.all$dit))), colour="blue", size=1, lty=2)

multiplot(plot.exp.dit_before, plot.exp.dit_after, cols=2)

# NOC
plot.exp.noc_before = ggplot(df.exp.thresholds.all, aes(noc)) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="NOC histogram before log transformation", x="NOC", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(df.exp.thresholds.all$noc), sd=sd(df.exp.thresholds.all$noc)), colour="blue", size=1, lty=2)
df.exp.thresholds.all$noc[df.exp.thresholds.all$noc == 0] = 1
plot.exp.noc_after = ggplot(df.exp.thresholds.all, aes(log(noc))) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="NOC histogram after log transformation", x="NOC", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(log(df.exp.thresholds.all$noc)), sd=sd(log(df.exp.thresholds.all$noc))), colour="blue", size=1, lty=2)

multiplot(plot.exp.noc_before, plot.exp.noc_after, cols=2)

# LCOM
plot.exp.lcom_before = ggplot(df.exp.thresholds.all, aes(lcom)) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="LCOM histogram before log transformation", x="LCOM", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(df.exp.thresholds.all$lcom), sd=sd(df.exp.thresholds.all$lcom)), colour="blue", size=1, lty=2)
df.exp.thresholds.all$lcom[df.exp.thresholds.all$lcom == 0] = 1
plot.exp.lcom_after = ggplot(df.exp.thresholds.all, aes(log(lcom))) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="LCOM histogram after log transformation", x="LCOM", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(log(df.exp.thresholds.all$lcom)), sd=sd(log(df.exp.thresholds.all$lcom))), colour="blue", size=1, lty=2)

multiplot(plot.exp.lcom_before, plot.exp.lcom_after, cols=2)

# RFC
plot.exp.rfc_before = ggplot(df.exp.thresholds.all, aes(rfc)) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="RFC histogram before log transformation", x="RFC", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(df.exp.thresholds.all$rfc), sd=sd(df.exp.thresholds.all$rfc)), colour="blue", size=1, lty=2)
df.exp.thresholds.all$rfc[df.exp.thresholds.all$rfc == 0] = 1
plot.exp.rfc_after = ggplot(df.exp.thresholds.all, aes(log(rfc))) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="RFC histogram after log transformation", x="RFC", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(log(df.exp.thresholds.all$rfc)), sd=sd(log(df.exp.thresholds.all$rfc))), colour="blue", size=1, lty=2)

multiplot(plot.exp.rfc_before, plot.exp.rfc_after, cols=2)

# WMC
plot.exp.wmc_before = ggplot(df.exp.thresholds.all, aes(wmc)) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="WMC histogram before log transformation", x="WMC", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(df.exp.thresholds.all$wmc), sd=sd(df.exp.thresholds.all$wmc)), colour="blue", size=1, lty=2)
df.exp.thresholds.all$wmc[df.exp.thresholds.all$wmc == 0] = 1
plot.exp.wmc_after = ggplot(df.exp.thresholds.all, aes(log(wmc))) + 
  theme(legend.position="none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="WMC histogram after log transformation", x="WMC", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(log(df.exp.thresholds.all$wmc)), sd=sd(log(df.exp.thresholds.all$wmc))), colour="blue", size=1, lty=2)

multiplot(plot.exp.wmc_before, plot.exp.wmc_after, cols=2)

### Validation
umatrix = function(data, field, thresholds){
  ## Métrica >= Valor de referência 
  ## Moderate
  tmp.data = data[data[, field] >= thresholds$P1 & data[, field] < thresholds$P2, ]
  # Number of true positive
  print("true positive (moderate) %: ")
  print(round(nrow(tmp.data[tmp.data$bugs > 1, ])/nrow(data[data$bugs > 1, ])*100, 2))
  ## high
  tmp.data = data[data[, field] >= thresholds$P2 & data[, field] < thresholds$P3, ]
  # Number of true positive
  print("true positive (high) %: ")
  print(round(nrow(tmp.data[tmp.data$bugs > 1, ])/nrow(data[data$bugs > 1, ])*100, 2))
  ## very-high
  tmp.data = data[data[, field] >= thresholds$P3, ]
  # Number of true positive
  print("true positive (very-high) %: ")
  print(round(nrow(tmp.data[tmp.data$bugs > 1, ])/nrow(data[data$bugs > 1, ])*100, 2))
  ## Total
  tmp.data = data[data[, field] >= thresholds$P1, ]
  print("true positive (total) %: ")
  tmp.true_posivite = nrow(tmp.data[tmp.data$bugs > 1, ])/nrow(data[data$bugs > 1, ])
  print(round(tmp.true_posivite*100, 2))
  
  tmp.data = data[data[, field] >= thresholds$P1, ]
  # Number of fause positive
  print("fause positive %: ")
  tmp.false_positive = nrow(tmp.data[tmp.data$bugs <= 1, ])/nrow(data[data$bugs <= 1, ])
  print(round(tmp.false_positive*100, 2))
  
  ## Métrica < Valor de referência
  tmp.data = data[data[, field] < thresholds$P1, ]
  # Number of fause negative
  print("fause negative %: ")
  tmp.fause_negative = nrow(tmp.data[tmp.data$bugs <= 1, ])/nrow(data[data$bugs <= 1, ])
  print(round(tmp.fause_negative*100, 2))
  # Number of true negative
  print("true negative %: ")
  tmp.true_negative = nrow(tmp.data[tmp.data$bugs > 1, ])/nrow(data[data$bugs > 1, ])
  print(round(tmp.true_negative*100, 2))
  
  print("Eficiência %: ")
  print(round((tmp.true_posivite-tmp.false_positive)*100, 2))
  
  return(data.frame(accept=c(tmp.true_posivite, tmp.fause_negative), error=c(tmp.false_positive, tmp.true_negative)))
}




# Number of bugs
print("Bugs: ")
nrow(df.exp.validation[df.exp.validation$bugs > 1, ])
print("No bugs: ")
nrow(df.exp.validation[df.exp.validation$bugs <= 1, ])

# Validations
confusion = data.frame(accept=NULL, error=NULL)

confusion = rbind(confusion, umatrix(df.exp.validation, 'cbo', threshold[threshold$metric == 'cbo', ]))
confusion = rbind(confusion, umatrix(df.exp.validation, 'dit', threshold[threshold$metric == 'dit', ]))
confusion = rbind(confusion, umatrix(df.exp.validation, 'noc', threshold[threshold$metric == 'noc', ]))
confusion = rbind(confusion, umatrix(df.exp.validation, 'lcom', threshold[threshold$metric == 'lcom', ]))
confusion = rbind(confusion, umatrix(df.exp.validation, 'rfc', threshold[threshold$metric == 'rfc', ]))
confusion = rbind(confusion, umatrix(df.exp.validation, 'wmc', threshold[threshold$metric == 'wmc', ]))

## t-test
print(shapiro.test(confusion$accept))
print(shapiro.test(confusion$error))

print(t.test(confusion$accept, confusion$error, paired=FALSE, var.equal=FALSE, alternative="greater"))


