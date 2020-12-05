library(brms); library(ggplot2)

### Download data from Pisor & Gurven 2016
dat <- read.csv("Pisor & Gurven 2016 paper data.csv", header = T) #Update with URL once repo is public.

### Prepare variables

# Convert nominal variables to character strings
dat$Ill.This.Mo <- as.character(dat$Ill.This.Mo)
dat$Coop.Labor <- as.character(dat$Coop.Labor)

dat$Times.Church.Mo[is.na(dat$Times.Church.Mo)]<-0 # NA = did not attend church in the last month
dat$Age<-dat$Age-20 # Make minimum value for Age 0. Note that age is binned to protect participant identities.

colnames(dat)[1] <- "Agreeable" # Rename first column which has junk on the front of the name

dat <- dat[!is.na(dat$OG.Avg),] # Remove missing data so that brms doesn't throw you an error about it.
dat$censor <- ifelse(dat$OG.Avg == 0, -1, 0) # Tell the model that zeroes are censored on the left-hand side (-1), as they could theoretically be less than zero.

### Subset sample to plot three separate lines

datt <- dat[dat$Population == "Tsimane",]
datm <- dat[dat$Population == "Moseten",]
dati <- dat[dat$Population == "Intercultural",]

### Run models

# Using default weakly informative priors from brms as no issues with model fit and those priors are appropriate for the predictors involved.

modt <- brm(OG.Avg | cens (censor) ~ Market.Items.zscore + log.Subj.SES + Ill.This.Mo + Places.Lived + Coop.Labor + Media.zscore + Nonanonymous.Play + Times.Church.Mo, data = datt, control = list(adapt_delta = 0.99), family = gaussian)

modi <- brm(OG.Avg | cens (censor) ~ Market.Items.zscore + log.Subj.SES + Ill.This.Mo + Places.Lived + Coop.Labor + Media.zscore + Nonanonymous.Play + Times.Church.Mo, data = dati, control = list(adapt_delta = 0.99), family = gaussian, prior = priors)

modm <- brm(OG.Avg | cens (censor) ~ Market.Items.zscore + log.Subj.SES + Ill.This.Mo + Places.Lived + Coop.Labor + Media.zscore + Nonanonymous.Play + Times.Church.Mo, data = datm, control = list(adapt_delta = 0.99), family = gaussian, prior = priors)

### Predict values by population
# Why predict instead of just smooth the plot between market items and average amount to an out-group member? Because this method allows us to control for the influence of other variables known to matter in this data set.
# We're predicting only for the interval over which Tsimane' values were observed, which is just a subset of the total market items interval.

# User-defined function for mode.

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Values for market items over which to predict the outcome.

mi.t <- seq(min(datt$Market.Items.zscore), max(datt$Market.Items.zscore), length.out = 5)

# Provide average (for continuous) or modal (for nominal) values for all other variables

newt <- data.frame(Market.Items.zscore = mi.t, log.Subj.SES = mean(datt$log.Subj.SES), Places.Lived = mean(datt$Places.Lived), Media.zscore = mean(datt$Media.zscore), Times.Church.Mo = mean(datt$Times.Church.Mo), Ill.This.Mo = getmode(datt$Ill.This.Mo), Coop.Labor = getmode(datt$Coop.Labor), Nonanonymous.Play = getmode(datt$Nonanonymous.Play))

# Get predicted values by population with 90% credible intervals.

pt <- data.frame(predict(modt, newdata = newt, probs = c(0.05, 0.95)))
pt$Population <- "Tsimane'"
pt$Market.Items <- mi.t

newm <- data.frame(Market.Items.zscore = mi.t, log.Subj.SES = mean(datm$log.Subj.SES), Places.Lived = mean(datm$Places.Lived), Media.zscore = mean(datm$Media.zscore), Times.Church.Mo = mean(datm$Times.Church.Mo), Ill.This.Mo = getmode(datm$Ill.This.Mo), Coop.Labor = getmode(datm$Coop.Labor), Nonanonymous.Play = getmode(datm$Nonanonymous.Play))

pm <- data.frame(predict(modm, newdata = newm, probs = c(0.05, 0.95)))
pm$Population <- "Mosetén"
pm$Market.Items <- mi.t

newi <- data.frame(Market.Items.zscore = mi.t, log.Subj.SES = mean(dati$log.Subj.SES), Places.Lived = mean(dati$Places.Lived), Media.zscore = mean(dati$Media.zscore), Times.Church.Mo = mean(dati$Times.Church.Mo), Ill.This.Mo = getmode(dati$Ill.This.Mo), Coop.Labor = getmode(dati$Coop.Labor), Nonanonymous.Play = getmode(dati$Nonanonymous.Play))

pi <- data.frame(predict(modi, newdata = newi, probs = c(0.05, 0.95)))
pi$Population <- "Intercultural"
pi$Market.Items <- mi.t

pdat <- rbind(pt, pm, pi)

### Plot

cols <- c("Intercultural" = "black", "Mosetén" = "orange3", "Tsimane'" = "royalblue4")

(p <- ggplot(pdat, aes(x = Market.Items, y = Estimate, color = Population)) +
    geom_smooth(aes(ymin = Q5, ymax = Q95, color = Population, fill = Population), stat = "identity", alpha = 0.2, lwd = 2) +
    xlab("Market items owned (normalized)") + ylab("Predicted bolivianos to out-group recipient") +
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols) + 
    theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=12)) +
  theme(strip.text = element_text(size = 14, face = "bold"), axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size = 13, face = "bold"), panel.grid = element_blank()) + 
    theme(strip.text.y = element_text(angle = 360))  + 
    theme(panel.spacing = unit(1, "lines")) + 
    theme(panel.background = element_rect(fill = "white", color = "black", linetype = "solid")) + 
    theme(strip.background = element_rect(fill = "white", color = "black", linetype = "solid")) 
)

ggsave("Market items figure.pdf", p, height=5, width=4)