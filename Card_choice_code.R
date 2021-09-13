############## Pisor & Ross - Distinguishing intergroup and long-distance relationships ##############

### Run with R 4.0.2
### data and metadata available at www.github.com/annethro/parochialism
setwd() # Set to your own working directory.

pckgs<-c("brms", "mice", "ggplot2", "data.table")
lapply(pckgs, require, character.only = TRUE)
num.chains <- 4 ; num.cores <- 4 # These are the number of chains that will be run in the model fitting process, plus the number of cores to use on your processor when fitting. The latter may need adjustment based on your computer's capabilities.
semilla <- 207355 # Set a random seed for replicability

### There are two options for model fitting:

# Option 1: estimating the effects of each level separately, with a -1 indicating that a given level appears on the left-hand card and a 1 indicating that the level appears on the right. This coding scheme is explained in Supplementary Materials 1.3. The benefits of this approach is that one can disambiguate different preferences -- for example, if someone with a moderate level of wealth is preferred to someone that is very wealthy, or if someone from La Paz is preferred to someone from the same community but someone from elsewhere in the river valley is not preferred to someone from the same community, all these effects are estimated separately and thus easy to pinpoint. The cost of this option is that the model outputs are longer (and thus harder to sort through).

# Option 2: comparing only the differences between cards, such that a "higher value" on the left-hand side is coded with a -1 and a "higher value" on the right-hand side is coded with a 1. For example, if the card on the left says "very good person" and the card on the right says "not a good person," that would receive a -1, although we cannot see from this coding scheme how MUCH of a better person the left-hand card is. The benefit of this modeling approach is that it involves estimating fewer parameters and model outputs are shorter and thus easier to digest; the costs are that, aside from the estimates for pueblo indigena and religion, which are coded the same way under both Options 1 and 2, we can't disambiguate levels and thus find specific effects for the levels involved (e.g., if La Paz is much preferable to the same river valley).

# Because of the disambiguation possible with the first approach, we use that approach here. To explore model fitting using Option 2, see the bottom of this code for an example of how to utilize it.


##############################################################
######## What card preferences did participants have? ########
# This includes all participants who completed the choice task in 2017.

full <- read.csv("Bolivia_2017_dataset.csv", header = T, stringsAsFactors = F)

##### DATA PREP #####

### z-score (that is, normalize) variables such that results are given in standard deviations and zeroes are meaningful

full$NetIncome.z <- c(scale(full$NetIncome, center = TRUE, scale = TRUE))
full$Age.z <- c(scale(full$Age, center = TRUE, scale = TRUE))
full$School.z <- c(scale(full$School_Yrs, center = TRUE, scale = TRUE))

### Remove variables not using for analysis

full1 <- full[,!(colnames(full) %in% c("Order", "NetIncome", "Age", "School_Yrs"))]

### Impute: just for one person missing their years of schooling and for people with no religious affiliation (n=13)

# I'll need to tell mice (the function used for imputation) which variables to impute (rows) using which other variables (columns). Start by setting up a matrix full of zeroes, as my default will be to not impute.
which.mat<-matrix(rep(0,ncol(full1)*nrow(full1)), ncol = ncol(full1), nrow = ncol(full1))
rownames(which.mat) <- colnames(full1)
colnames(which.mat) <- colnames(full1)

# Next step: for variables that need imputing, say which other ones to use in imputation by assigning them 1s.

which.mat["Relig.Other_Diff", ] <- ifelse(colnames(which.mat) %in% c("Age.z", "NetIncome.z", "Population", "School.z", "Sex"), 1, 0) # Whether someone is male or female is often a good predictor of whether they attend church; a useful variable on which to impute here. Also reasonable to expect it to be related to how much school they attended (below). More common for folks in the Moseten community to say they are not religious (also fewer churches to choose from there), so I use population to impute religious affiliation too.
which.mat["School.z", ] <- ifelse(colnames(which.mat) %in% c("Age.z", "NetIncome.z", "Population", "Sex"), 1, 0) # Years in school is predicted by age (because more schooling was available later in time, as schools were constructed) and sex (females more likely to drop out or not go), population (Interculturales had a high school much earlier), and income (folks who make more may have had more schooling).

these<-rep("", length(full1))
these[which(colnames(full1) %in% row.names(which.mat)[rowSums(which.mat) > 0])]<-"pmm" # Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
full2 <- mice(full1, method = these, predictorMatrix = which.mat, seed = semilla, m = 10) # Impute. m=10 tells mice to make 10 imputations (10 data sets with imputed values filled in). (It returns a warning about two logged events, but it's just telling you that two variables are constant.) However, there's really no point to imputing ten times if you use the following...
full3 <- complete(full2) # ...this just fills in the missing values in the data with the imputed values from the first imputation.

### Convert to factors
# Relevel factors such that the absence of a level on the two cards is compared to the presence of that level, either on the left (-1) or on the right (1). In other words, 0 becomes the contrast level, and by coding these variables as factors, brms will automatically estimate parameters for -1 and 1.

full3$Loc.SameValley_Diff <- relevel(as.factor(full3$Loc.SameValley_Diff), "0")
full3$Loc.City_Diff <- relevel(as.factor(full3$Loc.City_Diff), "0")
full3$Wealth.High_Diff <- relevel(as.factor(full3$Wealth.High_Diff), "0")
full3$Wealth.Med_Diff <- relevel(as.factor(full3$Wealth.Med_Diff), "0")
full3$Trust.High_Diff <- relevel(as.factor(full3$Trust.High_Diff), "0")
full3$Trust.Med_Diff <- relevel(as.factor(full3$Trust.Med_Diff), "0")
full3$Good.High_Diff <- relevel(as.factor(full3$Good.High_Diff), "0")
full3$Good.Med_Diff <- relevel(as.factor(full3$Good.Med_Diff), "0")
full3$Relig.Other_Diff <- relevel(as.factor(full3$Relig.Other_Diff), "0")
full3$Pueblo.Other_Diff <- relevel(as.factor(full3$Pueblo.Other_Diff), "0")


##### MODELS #####

priors <- c(set_prior("normal(0,1)", class = "b"),
            set_prior("normal(0,1)", class = "Intercept"),
            set_prior("exponential(1)", class = "sd")) # Set weakly informative priors on the relevant scale of the parameters. See Supplementary Methods 1.3 for our discussion of our choice of priors.

### No Controls ###

nc17 <- brm(formula = Chose_R ~
              
              # Predictors of interest
              Loc.SameValley_Diff + Loc.City_Diff +
              Pueblo.Other_Diff + 
              Relig.Other_Diff +
              
              # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
              Good.High_Diff + Good.Med_Diff +
              Trust.High_Diff + Trust.Med_Diff +
              Wealth.High_Diff + Wealth.Med_Diff +
              
              # Random effects
              (1|RID) + (1|Population) + (1|Version),
            
            data = full3, family = bernoulli, prior = priors, control = list(adapt_delta = 0.9999, max_treedepth = 12), # Why all this extra stuff? brms (which runs a program called Stan in the background) defaults to more coarse-grained sampling of the parameter space to save computing time, but it may mean it throws divergent transitions (which could be because of coarse-grained sampling or because the model is misspecified) or because Stan is looking for a U-turn in sampling (google it!) and hasn't hit a U-turn (or divergence). Increasing adapt_delta and max_treedepth slows down computing time, but assuming you haven't misspecified your model, more detailed sampling means Stan is more likely to settle on the likely parameter estimates.
            chains = num.chains, cores = num.cores, seed = semilla)

### Controls ###

# For controls to make sense -- to control for third variables affecting whether someone picks a long-distance partner, for example, rather than just for whether they picked the right-hand card (which would just be saying something like "do wealthier people prefer whatever card's on the right?") -- we interact each control with location, pueblo indigena, and religion one at a time.


## Age

c17_a <- brm(formula = Chose_R ~
             # Predictors of interest
             
             Loc.SameValley_Diff * Age.z +
              Loc.City_Diff * Age.z +
              Pueblo.Other_Diff * Age.z +
              Relig.Other_Diff * Age.z +
             
             # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
              Good.High_Diff +
              Good.Med_Diff +
              Trust.High_Diff +
              Trust.Med_Diff +
              Wealth.High_Diff +
              Wealth.Med_Diff +
             
             # Random effects
              (1|RID) + (1|Population) + (1|Version),
           
            data = full3, family = bernoulli, prior = priors, control = list(adapt_delta = 0.9999, max_treedepth = 12),
            chains = num.chains, cores = num.cores, seed = semilla)

## Sex

c17_se <- brm(formula = Chose_R ~
             # Predictors of interest
             
             Loc.SameValley_Diff * Sex +
             Loc.City_Diff * Sex +
             Pueblo.Other_Diff * Sex +
             Relig.Other_Diff * Sex +
             
             # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
             Good.High_Diff +
             Good.Med_Diff +
             Trust.High_Diff +
             Trust.Med_Diff +
             Wealth.High_Diff +
             Wealth.Med_Diff +
             
             # Random effects
             (1|RID) + (1|Population) + (1|Version),
           
           data = full3, family = bernoulli, prior = priors, control = list(adapt_delta = 0.99999, max_treedepth = 12),
           chains = num.chains, cores = num.cores, seed = semilla)

## School

c17_sc <- brm(formula = Chose_R ~
             # Predictors of interest
             
             Loc.SameValley_Diff * School.z +
             Loc.City_Diff * School.z +
             Pueblo.Other_Diff * School.z +
             Relig.Other_Diff * School.z +
             
             # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
             Good.High_Diff +
             Good.Med_Diff +
             Trust.High_Diff +
             Trust.Med_Diff +
             Wealth.High_Diff +
             Wealth.Med_Diff +
             
             # Random effects
             (1|RID) + (1|Population) + (1|Version),
           
           data = full3, family = bernoulli, prior = priors, control = list(adapt_delta = 0.9999, max_treedepth = 12),
           chains = num.chains, cores = num.cores, seed = semilla)


## Net income

c17_i <- brm(formula = Chose_R ~
             # Predictors of interest
             
             Loc.SameValley_Diff * NetIncome.z +
             Loc.City_Diff * NetIncome.z +
             Pueblo.Other_Diff * NetIncome.z +
             Relig.Other_Diff * NetIncome.z +
             
             # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
             Good.High_Diff +
             Good.Med_Diff +
             Trust.High_Diff +
             Trust.Med_Diff +
             Wealth.High_Diff +
             Wealth.Med_Diff +
             
             # Random effects
             (1|RID) + (1|Population) + (1|Version),
           
           data = full3, family = bernoulli, prior = priors, control = list(adapt_delta = 0.999, max_treedepth = 12),
           chains = num.chains, cores = num.cores, seed = semilla)


############## Plot model without controls ##############

est_c17 <- data.frame(fixef(nc17, probs = c(0.05, 0.95))) # This pulls the parameter estimates for the fixed effects out of the model fit, including 90% credible intervals.
setDT(est_c17, keep.rownames = TRUE)[] # Convert row names into a column for use with the data.table package.
df_c17 <- data.frame(Variable = est_c17$rn, LI = exp(est_c17$Q5), Est = exp(est_c17$Estimate), HI = exp(est_c17$Q95)) # Convert to a data frame.

df_c17_1 <- df_c17[df_c17$Variable != "Intercept", ] # Intercept not plotted

# Make labels for plots more transparent for readers

matchem1 <- c("Pueblo.", "Relig.")

df_c17_1$Type <- "Other"
df_c17_1$Type[grep("Loc.", df_c17_1$Variable)] <- "Location"
df_c17_1$Type[grep(paste(matchem1, collapse = "|"), df_c17_1$Variable)] <- "Group"

df_c17_1$Side <- "Right"
df_c17_1$Side[grep("DiffM1", df_c17_1$Variable)] <- "Left"

df_c17_1$Type<-factor(df_c17_1$Type, levels = c("Location", "Group", "Other"))

df_c17_1$Variable[df_c17_1$Variable %in% "Loc.SameValley_DiffM1"] <- "Alter lives in a nearby community"
df_c17_1$Variable[df_c17_1$Variable %in% "Loc.SameValley_Diff1"] <- "Alter lives in a nearby community"
df_c17_1$Variable[df_c17_1$Variable %in% "Loc.City_DiffM1"] <- "Alter lives far away in La Paz" # Where does the M come from? R subsistutes this for a negative sign, as in M for "minus."
df_c17_1$Variable[df_c17_1$Variable %in% "Loc.City_Diff1"] <- "Alter lives far away in La Paz"
df_c17_1$Variable[df_c17_1$Variable %in% "Pueblo.Other_DiffM1"] <- "Alter is from other pueblo indígena" # If your text editor messes this up on download like mine does, that weird character is an i with an accent.
df_c17_1$Variable[df_c17_1$Variable %in% "Pueblo.Other_Diff1"] <- "Alter is from other pueblo indígena"
df_c17_1$Variable[df_c17_1$Variable %in% "Relig.Other_DiffM1"] <- "Alter has other religious affiliation"
df_c17_1$Variable[df_c17_1$Variable %in% "Relig.Other_Diff1"] <- "Alter has other religious affiliation"
df_c17_1$Variable[df_c17_1$Variable %in% "Good.High_DiffM1"] <- "Alter is a very good person"
df_c17_1$Variable[df_c17_1$Variable %in% "Good.High_Diff1"] <- "Alter is a very good person"
df_c17_1$Variable[df_c17_1$Variable %in% "Good.Med_DiffM1"] <- "Alter is a good person"
df_c17_1$Variable[df_c17_1$Variable %in% "Good.Med_Diff1"] <- "Alter is a good person"
df_c17_1$Variable[df_c17_1$Variable %in% "Trust.High_DiffM1"] <- "Alter is very trustworthy"
df_c17_1$Variable[df_c17_1$Variable %in% "Trust.High_Diff1"] <- "Alter is very trustworthy"
df_c17_1$Variable[df_c17_1$Variable %in% "Trust.Med_DiffM1"] <- "Alter is trustworthy"
df_c17_1$Variable[df_c17_1$Variable %in% "Trust.Med_Diff1"] <- "Alter is trustworthy"
df_c17_1$Variable[df_c17_1$Variable %in% "Wealth.High_DiffM1"] <- "Alter has a lot of money"
df_c17_1$Variable[df_c17_1$Variable %in% "Wealth.High_Diff1"] <- "Alter has a lot of money"
df_c17_1$Variable[df_c17_1$Variable %in% "Wealth.Med_DiffM1"] <- "Alter has some money"
df_c17_1$Variable[df_c17_1$Variable %in% "Wealth.Med_Diff1"] <- "Alter has some money"

df_c17_1$Variable <- factor(df_c17_1$Variable, levels = c("Alter lives far away in La Paz", "Alter lives in a nearby community", 
        "Alter has other religious affiliation", "Alter is from other pueblo indígena", 
        "Alter has some money", "Alter has a lot of money", "Alter is a good person", "Alter is a very good person", "Alter is trustworthy", "Alter is very trustworthy"
)) #This re-orders the data frame such that variables appear in the order discussed in the manuscript.

levels(df_c17_1$Variable) <- gsub("  ", "\n", levels(df_c17_1$Variable))

# To make the plot easier to digest for the reader, we remove the left-side estimates for the plot in the main text; the plot with left-side estimates included can be found in the supplement.

df_c17_R <- df_c17_1[df_c17_1$Side == 'Right',]

### MAIN TEXT PLOT (right only)

p <- ggplot(df_c17_R, aes(x = Variable, y = Est, ymin = LI, ymax = HI, color = Type)) + 
     geom_linerange(size = 1) + geom_point(size = 2) +
     geom_hline(aes(yintercept = 1), color = "blue", linetype = "dashed") +
     facet_grid(Type ~ . , scales = "free_y", space = "free_y") + scale_y_log10() +
     labs(y = "Odds of selecting the card", x = "Card attributes") + theme(strip.text.x = element_text(size = 14 , face = "bold"), 
     strip.text.y = element_text(size = 14, face = "bold"), axis.text = element_text(size = 10), axis.title = element_text(size = 12,
     face = "bold")) + theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(0.5, "lines")) + 
     scale_color_manual(values = c("Location" = "royalblue4", "Group" = "orange3", "Other" = "black")) + theme(legend.position = "none")

   p  
ggsave("Bolivia_CardChoice_Non-Standardized.pdf", p, height = 2.5, width = 6)

### SUPPLEMENT PLOT (includes estimates for picking both the left-hand and right-hand cards)

sp <- ggplot(df_c17_1, aes(x = Variable, y = Est, ymin = LI, ymax = HI)) + 
  geom_linerange(size = 1, position = position_dodge2(width = 0.25), color = c(rep(c("royalblue1", "royalblue4"), 2), rep(c("orange1", "orange3"), 2), rep(c("gray50", "black"), 6))) + 
  geom_point(size = 2, position = position_dodge2(width = 0.25), color = c(rep(c("royalblue1", "royalblue4"), 2), rep(c("orange1", "orange3"), 2), rep(c("gray50", "black"), 6))) +
  geom_hline(aes(yintercept = 1), color = "gray50", linetype = "dashed") +
  facet_grid(Type ~ . , scales = "free", space = "free") + scale_y_log10() +
  labs(y = "Odds of selecting the card", x = "Card attributes") + theme(strip.text.x = element_text(size = 14 , face = "bold"), 
  strip.text.y = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) + 
  theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) + 
  theme(legend.position = "none")

ggsave("Bolivia_Card Choice_Non-Standardized_Supp.pdf", sp, height = 9, width = 6.5)


############################################################################
######## Does game play in 2014-15 predict cards preferred in 2017? ########
# This is the subset of participants who participated in both 2014-15 and 2017.

part <- read.csv("Bolivia_longit_dataset.csv", header = T, stringsAsFactors = F)
part <- part[!is.na(part$Average_Given.Out),] # Not everyone was presented with an out-group in the 2014-15 economic game. See Pisor & Gurven (2018) for details (https://doi.org/10.1016/j.evolhumbehav.2017.09.003).

##### DATA PREP #####

### z-score (that is, normalize) variables such that results are given in standard deviations and zeroes are meaningful

part$NetIncome_17.z <- c(scale(part$NetIncome_2017, center = TRUE, scale = TRUE))
part$NetIncome_14.z <- c(scale(part$NetIncome_2014, center = TRUE, scale = TRUE))
part$Age.z <- c(scale(part$Age, center = TRUE, scale = TRUE))
part$School.z <- c(scale(part$School_Yrs, center = TRUE, scale = TRUE))
part$Give_Out.z <- c(scale(part$Average_Given.Out, center = TRUE, scale = TRUE))


### Dummy code
# Binary for whether participant learned recipients' pueblo indigena or religious affiliation when playing the economic game in 2014-15.
part$Type.Pueblo <- ifelse(part$Type == "Pueblo", 1, 0) # Zero implies that they were told about recipients' religious affiliation.


### Remove variables not using for analysis
part1 <- part[ ,!(colnames(part) %in% c("Order", "NetIncome_2017", "NetIncome_2014", "Age", "School_Yrs", "Type", "Average_Given.Out"))]


### Impute: just for one person missing their years of schooling and for people with no religious affiliation (n=3).

# I'll need to tell mice (the function used for imputation) which variables to impute (rows) using which other variables (columns). Start by setting up a matrix full of zeroes, as my default will be to not impute.
which.mat <- matrix(rep(0, ncol(part1) * nrow(part1)), ncol = ncol(part1), nrow = ncol(part1))
rownames(which.mat) <- colnames(part1)
colnames(which.mat) <- colnames(part1)

# Next step: for variables that need imputing, say which other ones to use in imputation by assigning them 1s.

which.mat["Relig.Other_Diff", ] <- ifelse(colnames(which.mat) %in% c("Age.z", "NetIncome_17.z", "Population", "School.z", "Sex"), 1, 0) # Whether someone is male or female is often a good predictor of whether they attend church; a useful variable on which to impute here. Also reasonable to expect it to be related to how much school they attended (below). More common for folks in the Moseten community to say they are not religious (also fewer churches to choose from there), so I use population to impute religious affiliation too.
which.mat["School.z", ] <- ifelse(colnames(which.mat) %in% c("Age.z", "NetIncome_17.z", "Population", "Sex"), 1, 0) # Years in school is predicted by age (because more schooling was available later in time) and sex (females more likely to drop out or not go), population (Interculturales had a high school much earlier), and income (folks who make more may have had more schooling).

these <- rep("", length(part1))
these[which(colnames(part1) %in% row.names(which.mat)[rowSums(which.mat) > 0])]<-"pmm" # Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
part2 <- mice(part1, method = these, predictorMatrix = which.mat, seed = semilla, m = 10) # Impute. m=10 tells mice to make 10 imputations (10 data sets with imputed values filled in). (It returns a warning about two logged events, but it's just telling you that two variables are constant.) However, there's really no point to imputing ten times if you use the following...
part3 <- complete(part2) # ...this just fills in the missing values in the data with the imputed values from the first imputation.


### Convert to factors
# Relevel factors such that the absence of a level on the two cards is compared to the presence of that level, either on the left (-1) or on the right (1). In other words, 0 becomes the contrast level, and by coding these variables as factors, brms will automatically estimate parameters for -1 and 1.

part3$Loc.SameValley_Diff <- relevel(as.factor(part3$Loc.SameValley_Diff), "0") 
part3$Loc.City_Diff <- relevel(as.factor(part3$Loc.City_Diff), "0")
part3$Pueblo.Other_Diff <- relevel(as.factor(part3$Pueblo.Other_Diff), "0")
part3$Relig.Other_Diff <- relevel(as.factor(part3$Relig.Other_Diff), "0")
part3$Good.High_Diff <- relevel(as.factor(part3$Good.High_Diff), "0")
part3$Good.Med_Diff <- relevel(as.factor(part3$Good.Med_Diff), "0")
part3$Trust.High_Diff <- relevel(as.factor(part3$Trust.High_Diff), "0")
part3$Trust.Med_Diff <- relevel(as.factor(part3$Trust.Med_Diff), "0")
part3$Wealth.High_Diff <- relevel(as.factor(part3$Wealth.High_Diff), "0")
part3$Wealth.Med_Diff <- relevel(as.factor(part3$Wealth.Med_Diff), "0")


##### MODELS #####

priors <- c(set_prior("normal(0,1)", class = "b"),
                set_prior("normal(0,1)", class = "Intercept"),
                set_prior("exponential(1)", class = "sd")) # Set weakly informative priors on the relevant scale of the parameters. See Supplementary Methods 1.3 for our discussion of our choice of priors.

### No Controls ###

nc14 <- brm(formula = Chose_R ~
              # Two way interactions: does out-group giving affect preferences for people living at a distance?
              Loc.SameValley_Diff * Give_Out.z +
              Loc.City_Diff * Give_Out.z + 
              
              # Three way interactions: given folks either knew the pueblo indigena or religious affiliation of recipients in the 2014-15 game, how much did they give, and is that related to preferences expressed in the choice task in 2017?
              Pueblo.Other_Diff * Give_Out.z * Type.Pueblo +
              Relig.Other_Diff * Give_Out.z * Type.Pueblo +
              
              # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
              Good.High_Diff +
              Good.Med_Diff +
              Trust.High_Diff +
              Trust.Med_Diff +
              Wealth.High_Diff +
              Wealth.Med_Diff +
              
              # Random effects 
              (1|RID) + (1|Population) + (1|Version),
            data = part3, family = bernoulli, prior = priors, seed = semilla,
            chains = num.chains, cores = num.cores, control = list(adapt_delta = 0.9999))


#### Controls ####

# As before, go through each one separately.

c14_a <- brm(formula = Chose_R ~
                # Two way interactions: does out-group giving affect preferences for people living at a distance?
                Loc.SameValley_Diff * Give_Out.z * Age.z +
                Loc.City_Diff * Give_Out.z * Age.z + 
                
             # Three way interactions: given folks either knew the pueblo indigena or religious affiliation of recipients in the 2014-15 game, how much did they give, and is that related to preferences expressed in the choice task in 2017?
                Pueblo.Other_Diff * Give_Out.z * Type.Pueblo * Age.z +
                Relig.Other_Diff * Give_Out.z * Type.Pueblo * Age.z +
                
                # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
                Good.High_Diff +
                Good.Med_Diff +
                Trust.High_Diff +
                Trust.Med_Diff +
                Wealth.High_Diff +
                Wealth.Med_Diff +
                
                # Random effects 
                (1|RID) + (1|Population) + (1|Version),
           
              data = part3, family = bernoulli, prior = priors, seed = semilla,
              chains = num.chains, cores = num.cores, control = list(adapt_delta = 0.999999, stepsize = 0.01))

c14_se <- brm(formula = Chose_R ~
             # Two way interactions: does out-group giving affect preferences for people living at a distance?
             Loc.SameValley_Diff * Give_Out.z * Sex +
             Loc.City_Diff * Give_Out.z * Sex + 
             
             # Three way interactions: given folks either knew the pueblo indigena or religious affiliation of recipients in the 2014-15 game, how much did they give, and is that related to preferences expressed in the choice task in 2017?
             Pueblo.Other_Diff * Give_Out.z * Type.Pueblo * Sex +
             Relig.Other_Diff * Give_Out.z * Type.Pueblo * Sex +
             
             # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
             Good.High_Diff +
             Good.Med_Diff +
             Trust.High_Diff +
             Trust.Med_Diff +
             Wealth.High_Diff +
             Wealth.Med_Diff +
             
             # Random effects 
             (1|RID) + (1|Population) + (1|Version),
           
           data = part3, family = bernoulli, prior = priors, seed = semilla,
           chains = num.chains, cores = num.cores, control = list(adapt_delta = 0.99999, stepsize = 0.01))

c14_sc <- brm(formula = Chose_R ~
                # Two way interactions: does out-group giving affect preferences for people living at a distance?
                Loc.SameValley_Diff * Give_Out.z * School.z +
                Loc.City_Diff * Give_Out.z * School.z + 
                
                # Three way interactions: given folks either knew the pueblo indigena or religious affiliation of recipients in the 2014-15 game, how much did they give, and is that related to preferences expressed in the choice task in 2017?
                Pueblo.Other_Diff * Give_Out.z * Type.Pueblo * School.z +
                Relig.Other_Diff * Give_Out.z * Type.Pueblo * School.z +
                
                # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
                Good.High_Diff +
                Good.Med_Diff +
                Trust.High_Diff +
                Trust.Med_Diff +
                Wealth.High_Diff +
                Wealth.Med_Diff +
                
                # Random effects 
                (1|RID) + (1|Population) + (1|Version),
              
              data = part3, family = bernoulli, prior = priors, seed = semilla,
              chains = num.chains, cores = num.cores, control = list(adapt_delta = 0.99999, stepsize = 0.01))

c14_i <- brm(formula = Chose_R ~
                # Two way interactions: does out-group giving affect preferences for people living at a distance?
                Loc.SameValley_Diff * Give_Out.z * NetIncome_17.z +
                Loc.City_Diff * Give_Out.z * NetIncome_17.z + 
                
                # Three way interactions: given folks either knew the pueblo indigena or religious affiliation of recipients in the 2014-15 game, how much did they give, and is that related to preferences expressed in the choice task in 2017?
                Pueblo.Other_Diff * Give_Out.z * Type.Pueblo * NetIncome_17.z +
                Relig.Other_Diff * Give_Out.z * Type.Pueblo * NetIncome_17.z +
                
                # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
                Good.High_Diff +
                Good.Med_Diff +
                Trust.High_Diff +
                Trust.Med_Diff +
                Wealth.High_Diff +
                Wealth.Med_Diff +
  
                # Random effects 
                (1|RID) + (1|Population) + (1|Version),
              
              data = part3, family = bernoulli, prior = priors, seed = semilla,
              chains = num.chains, cores = num.cores, control = list(adapt_delta = 0.99999, stepsize = 0.01))

c14_p <- brm(formula = Chose_R ~
               # Two way interactions: does out-group giving affect preferences for people living at a distance?
               Loc.SameValley_Diff * Give_Out.z * Nonanonymous_Play +
               Loc.City_Diff * Give_Out.z * Nonanonymous_Play + 
               
               # Three way interactions: given folks either knew the pueblo indigena or religious affiliation of recipients in the 2014-15 game, how much did they give, and is that related to preferences expressed in the choice task in 2017?
               Pueblo.Other_Diff * Give_Out.z * Type.Pueblo * Nonanonymous_Play +
               Relig.Other_Diff * Give_Out.z * Type.Pueblo * Nonanonymous_Play +
               
               # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
               Good.High_Diff +
               Good.Med_Diff +
               Trust.High_Diff +
               Trust.Med_Diff +
               Wealth.High_Diff +
               Wealth.Med_Diff +

               # Random effects 
               (1|RID) + (1|Population) + (1|Version),
             
             data = part3, family = bernoulli, prior = priors, seed = semilla,
             chains = num.chains, cores = num.cores, control = list(adapt_delta = 0.999999, stepsize = 0.01))


############## Plot model with controls ##############

est_c14 <- data.frame(fixef(nc14, probs = c(0.05, 0.95))) # This pulls the parameter estimates for the fixed effects out of the model fit, including 90% credible intervals.
setDT(est_c14, keep.rownames = TRUE)[] # Convert row names into a column for use with the data.table package.
df_c14 <- data.frame(Variable = est_c14$rn, LI = exp(est_c14$Q5), Est = exp(est_c14$Estimate), HI = exp(est_c14$Q95)) # Convert to a data frame.

df_c14_1 <- df_c14[df_c14$Variable != "Intercept", ] # Intercept is not plotted.

# Make labels for plots more transparent for readers

matchem1 <- c("Pueblo.", "Relig.")

df_c14_1$Type <- "Other"
df_c14_1$Type[grep("Loc.", df_c14_1$Variable)] <- "Location"
df_c14_1$Type[grep(paste(matchem1, collapse = "|"), df_c14_1$Variable)] <- "Group"

df_c14_1$Side <- "Right"
df_c14_1$Side[grep("DiffM1", df_c14_1$Variable)] <- "Left"

df_c14_1$Type<-factor(df_c14_1$Type, levels = c("Location", "Group", "Other"))

df_c14_1$Variable[df_c14_1$Variable %in% "Loc.SameValley_DiffM1"] <- "Alter lives in a nearby community" # Where does the M come from? R subsistutes this for a negative sign, as in M for "minus."
df_c14_1$Variable[df_c14_1$Variable %in% "Loc.SameValley_Diff1"] <- "Alter lives in a nearby community"
df_c14_1$Variable[df_c14_1$Variable %in% "Loc.City_DiffM1"] <- "Alter lives far away in La Paz"
df_c14_1$Variable[df_c14_1$Variable %in% "Loc.City_Diff1"] <- "Alter lives far away in La Paz"
df_c14_1$Variable[df_c14_1$Variable %in% "Pueblo.Other_DiffM1"] <- "Alter is from other pueblo indígena" # If your text editor messes this up on download like mine does, that weird character is an i with an accent.
df_c14_1$Variable[df_c14_1$Variable %in% "Pueblo.Other_Diff1"] <- "Alter is from other pueblo indígena"
df_c14_1$Variable[df_c14_1$Variable %in% "Relig.Other_DiffM1"] <- "Alter has other religious affiliation"
df_c14_1$Variable[df_c14_1$Variable %in% "Relig.Other_Diff1"] <- "Alter has other religious affiliation"
df_c14_1$Variable[df_c14_1$Variable %in% "Good.High_DiffM1"] <- "Alter is a very good person"
df_c14_1$Variable[df_c14_1$Variable %in% "Good.High_Diff1"] <- "Alter is a very good person"
df_c14_1$Variable[df_c14_1$Variable %in% "Good.Med_DiffM1"] <- "Alter is a good person"
df_c14_1$Variable[df_c14_1$Variable %in% "Good.Med_Diff1"] <- "Alter is a good person"
df_c14_1$Variable[df_c14_1$Variable %in% "Trust.High_DiffM1"] <- "Alter is very trustworthy"
df_c14_1$Variable[df_c14_1$Variable %in% "Trust.High_Diff1"] <- "Alter is very trustworthy"
df_c14_1$Variable[df_c14_1$Variable %in% "Trust.Med_DiffM1"] <- "Alter is trustworthy"
df_c14_1$Variable[df_c14_1$Variable %in% "Trust.Med_Diff1"] <- "Alter is trustworthy"
df_c14_1$Variable[df_c14_1$Variable %in% "Wealth.High_DiffM1"] <- "Alter has a lot of money"
df_c14_1$Variable[df_c14_1$Variable %in% "Wealth.High_Diff1"] <- "Alter has a lot of money"
df_c14_1$Variable[df_c14_1$Variable %in% "Wealth.Med_DiffM1"] <- "Alter has some money"
df_c14_1$Variable[df_c14_1$Variable %in% "Wealth.Med_Diff1"] <- "Alter has some money"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z"] <- "Average given to out-group alters"
df_c14_1$Variable[df_c14_1$Variable %in% "Type.Pueblo"] <- "Out-group is pueblo indígena"
df_c14_1$Variable[df_c14_1$Variable %in% "Loc.SameValley_DiffM1:Give_Out.z"] <- "Nearby community *  Average given"
df_c14_1$Variable[df_c14_1$Variable %in% "Loc.SameValley_Diff1:Give_Out.z"] <- "Nearby community *  Average given"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Loc.City_DiffM1"] <- "La Paz * Average given"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Loc.City_Diff1"] <- "La Paz * Average given"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Pueblo.Other_DiffM1"] <- "Different pueblo indígena *  Average given"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Pueblo.Other_Diff1"] <- "Different pueblo indígena *  Average given"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Relig.Other_DiffM1"] <- "Different religion *  Average given"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Relig.Other_Diff1"] <- "Different religion *  Average given"
df_c14_1$Variable[df_c14_1$Variable %in% "Pueblo.Other_DiffM1:Type.Pueblo"] <- "Different pueblo indígena *  Out-group is pueblo indígena"
df_c14_1$Variable[df_c14_1$Variable %in% "Pueblo.Other_Diff1:Type.Pueblo"] <- "Different pueblo indígena *  Out-group is pueblo indígena"
df_c14_1$Variable[df_c14_1$Variable %in% "Type.Pueblo:Relig.Other_DiffM1"] <- "Different religion *  Out-group is pueblo indígena"
df_c14_1$Variable[df_c14_1$Variable %in% "Type.Pueblo:Relig.Other_Diff1"] <- "Different religion *  Out-group is pueblo indígena"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Type.Pueblo"] <- "Average given *  Out-group is pueblo indígena"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Pueblo.Other_DiffM1:Type.Pueblo"] <- "Diff. pueblo * Avg. given * Pueblo out-group"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Pueblo.Other_Diff1:Type.Pueblo"] <- "Diff. pueblo * Avg. given * Pueblo out-group"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Type.Pueblo:Relig.Other_DiffM1"] <- "Diff. religion * Avg. given * Pueblo out-group"
df_c14_1$Variable[df_c14_1$Variable %in% "Give_Out.z:Type.Pueblo:Relig.Other_Diff1"] <- "Diff. religion * Avg. given * Pueblo out-group"

df_c14_1$Variable <- factor(df_c14_1$Variable, levels = c("La Paz * Average given", "Alter lives far away in La Paz", "Nearby community *  Average given", "Alter lives in a nearby community",  
                                                          
                                                          "Diff. religion * Avg. given * Pueblo out-group", "Different religion *  Out-group is pueblo indígena",  "Different religion *  Average given", "Alter has other religious affiliation",
                                                          
                                                          "Diff. pueblo * Avg. given * Pueblo out-group", "Different pueblo indígena *  Out-group is pueblo indígena", "Different pueblo indígena *  Average given", "Alter is from other pueblo indígena",
                                                          
                                                          "Average given *  Out-group is pueblo indígena", "Out-group is pueblo indígena", "Average given to out-group alters", "Alter has some money","Alter has a lot of money",  "Alter is a good person", "Alter is a very good person", "Alter is trustworthy", "Alter is very trustworthy")) #This re-orders the data frame such that variables appear in the order discussed in the manuscript.


levels(df_c14_1$Variable) <- gsub("  ", "\n", levels(df_c14_1$Variable))


### Plot it (this includes estimates for picking both the left-hand and right-hand cards)

p <- ggplot(df_c14_1, aes(x = Variable, y = Est, ymin = LI, ymax = HI, color = Side)) + 
  
  geom_hline(aes(yintercept = 1), color = "gray50", linetype = "dashed") +
  
  geom_linerange(size = 1, position = position_dodge2(width = 0.25), color = c(rep(c("royalblue1", "royalblue4"), 4), rep(c("orange1", "orange3"), 8), rep(c("black"), 3), rep(c("gray50", "black"), 6))) + 
  geom_point(size = 2, position = position_dodge2(width = 0.25), color = c(rep(c("royalblue1", "royalblue4"), 4), rep(c("orange1", "orange3"), 8), rep(c("black"), 3), rep(c("gray50", "black"), 6))) +
  
  facet_grid(Type ~ . , scales = "free", space = "free") + scale_y_log10() +
  labs(y = "Odds of selecting the card", x = "Card attributes") + theme(strip.text.x = element_text(size = 14 , face = "bold"), 
  strip.text.y = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) + 
  theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) + 
  theme(legend.position = "none")
  
ggsave("Bolivia_Card Choice and Game Play_Non-Standardized.pdf", p, height=9, width=6.5)


##### CHECK ROLE AND RELATIONSHIP OF 2014 AND 2017 INCOMES #####

cor(part3$NetIncome_17.z, part3$NetIncome_14.z) # Totally unrelated. Why? Collected during different months, for one.

### Re-run model with 2014 net income data: is that the better predictor? ###

c14_ni14 <- brm(formula = Chose_R ~
             # Two way interactions: does out-group giving affect preferences for people living at a distance?
             Loc.SameValley_Diff * Give_Out.z * NetIncome_14.z +
             Loc.City_Diff * Give_Out.z * NetIncome_14.z + 
             
             # Three way interactions: given folks either knew the pueblo indigena or religious affiliation of recipients in the 2014-15 game, how much did they give, and is that related to preferences expressed in the choice task in 2017?
             Pueblo.Other_Diff * Give_Out.z * Type.Pueblo * NetIncome_14.z +
             Relig.Other_Diff * Give_Out.z * Type.Pueblo * NetIncome_14.z +
             
             # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
             Good.High_Diff +
             Good.Med_Diff +
             Trust.High_Diff +
             Trust.Med_Diff +
             Wealth.High_Diff +
             Wealth.Med_Diff +
             
             # Random effects 
             (1|RID) + (1|Population) + (1|Version),
           
           data = part3, family = bernoulli, prior = priors, seed = semilla,
           chains = num.chains, cores = num.cores, control = list(adapt_delta = 0.99999, stepsize = 0.01))

### What about difference between net income in 2017 and 2014-15? ###

part3$NetIncome_Diff <- part3$NetIncome_17.z - part3$NetIncome_14.z

c14_nidiff <- brm(formula = Chose_R ~
             # Two way interactions: does out-group giving affect preferences for people living at a distance?
             Loc.SameValley_Diff * Give_Out.z * NetIncome_Diff +
             Loc.City_Diff * Give_Out.z * NetIncome_Diff + 
             
             # Three way interactions: given folks either knew the pueblo indigena or religious affiliation of recipients in the 2014-15 game, how much did they give, and is that related to preferences expressed in the choice task in 2017?
             Pueblo.Other_Diff * Give_Out.z * Type.Pueblo * NetIncome_Diff +
             Relig.Other_Diff * Give_Out.z * Type.Pueblo * NetIncome_Diff +
             
             # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
             Good.High_Diff +
             Good.Med_Diff +
             Trust.High_Diff +
             Trust.Med_Diff +
             Wealth.High_Diff +
             Wealth.Med_Diff +
             
             # Random effects 
             (1|RID) + (1|Population) + (1|Version),
           
           data = part3, family = bernoulli, prior = priors, seed = semilla,
           chains = num.chains, cores = num.cores, control = list(adapt_delta = 0.99999, stepsize = 0.01))

######################################## OPTION 2 ########################################

# Not sure what this is? See line 16 for a detailed explanation.

##### DATA PREP #####

# Which candidate friend in a pair lives further away?

full$Location <- c(0)
full$Location <- ifelse(full$Loc.City_Diff == 1 | full$Loc.SameValley_Diff == 1, 1, full$Location)
full$Location <- ifelse(full$Loc.City_Diff == -1 | full$Loc.SameValley_Diff == -1 & full$Loc.City_Diff != 1, -1, full$Location)

# Which has higher wealth?

full$Wealth <- c(0)
full$Wealth <- ifelse(full$Wealth.High_Diff == 1 | full$Wealth.Med_Diff == 1, 1, full$Wealth)
full$Wealth <- ifelse(full$Wealth.High_Diff == -1 | full$Wealth.Med_Diff == -1 & full$Wealth.High_Diff != 1, -1, full$Wealth)

# Which has higher trustworthiness?

full$Trust <- c(0)
full$Trust <- ifelse(full$Trust.High_Diff == 1 | full$Trust.Med_Diff == 1, 1, full$Trust)
full$Trust <- ifelse(full$Trust.High_Diff == -1 | full$Trust.Med_Diff == -1 & full$Trust.High_Diff != 1, -1, full$Trust)

# Which is a better person?

full$Good <- c(0)
full$Good <- ifelse(full$Good.High_Diff == 1 | full$Good.Med_Diff == 1, 1, full$Good)
full$Good <- ifelse(full$Good.High_Diff == -1 | full$Good.Med_Diff == -1 & full$Good.High_Diff != 1, -1, full$Good)

colnames(full)[colnames(full) == "Pueblo.Other_Diff"] <- "Pueblo" # Rename for consistency with other columns (and do the renaming of these two columns separately to avoid surprises)
colnames(full)[colnames(full) == "Relig.Other_Diff"] <- "Religion"

### z-score (that is, normalize) variables such that results are given in standard deviations and zeroes are meaningful

full$NetIncome.z <- c(scale(full$NetIncome, center = TRUE, scale = TRUE))
full$Age.z <- c(scale(full$Age, center = TRUE, scale = TRUE))
full$School.z <- c(scale(full$Age, center = TRUE, scale = TRUE))

### Remove variables not using for analysis (omit for Option 1 (see line 21 for description))

full1 <- full[ ,!(colnames(full) %in% c("Order", "NetIncome", "Age", "School_Yrs", "Loc.SameValley_Diff", "Loc.City_Diff", "Wealth.High_Diff", "Wealth.Med_Diff", "Trust.Med_Diff", "Trust.High_Diff",  "Good.Med_Diff", "Good.High_Diff"))]

### Impute: just for one person missing their years of schooling and for people with no religious affiliation (n=13)

# I'll need to tell mice (the function used for imputation) which variables to impute (rows) using which other variables (columns). Start by setting up a matrix full of zeroes, as my default will be to not impute.
which.mat<-matrix(rep(0,ncol(full1)*nrow(full1)), ncol = ncol(full1), nrow = ncol(full1))
rownames(which.mat) <- colnames(full1)
colnames(which.mat) <- colnames(full1)

# Next step: for variables that need imputing, say which other ones to use in imputation by assigning them 1s.

which.mat["Religion", ] <- ifelse(colnames(which.mat) %in% c("Age.z", "NetIncome.z", "Population", "School.z", "Sex"), 1, 0) # Whether someone is male or female is often a good predictor of whether they attend church; a useful variable on which to impute here. Also reasonable to expect it to be related to how much school they attended (below). More common for folks in the Moseten community to say they are not religious (also fewer churches to choose from there), so I use population to impute religious affiliation too.
which.mat["School.z", ] <- ifelse(colnames(which.mat) %in% c("Age.z", "NetIncome.z", "Population", "Sex"), 1, 0) # Years in school is predicted by age (because more schooling was available later in time) and sex (females more likely to drop out or not go), population (Interculturales had a high school much earlier), and income (folks who make more may have had more schooling).

these<-rep("", length(full1))
these[which(colnames(full1) %in% row.names(which.mat)[rowSums(which.mat) > 0])]<-"pmm" # Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
full2 <- mice(full1, method = these, predictorMatrix = which.mat, seed = semilla, m = 10) # Impute. m=10 tells mice to make 10 imputations (10 data sets with imputed values filled in). (It returns a warning about two logged events, but it's just telling you that two variables are constant.) However, there's really no point to imputing ten times if you use the following...
full3 <- complete(full2) # ...this just fills in the missing values in the data with the imputed values from the first imputation.

### Convert to factors
# Relevel factors such that a difference between the two cards is compared to no difference (coded as 0).

full3$Location <- relevel(as.factor(full3$Location), "0") 
full3$Pueblo <- relevel(as.factor(full3$Pueblo), "0")
full3$Religion <- relevel(as.factor(full3$Religion), "0")
full3$Good <- relevel(as.factor(full3$Good), "0")
full3$Trust <- relevel(as.factor(full3$Trust), "0")
full3$Wealth <- relevel(as.factor(full3$Wealth), "0")

##### MODELS #####

priors <- c(set_prior("normal(0,1)", class = "b"),
            set_prior("normal(0,1)", class = "Intercept"),
            set_prior("exponential(1)", class = "sd")) # Set weakly informative priors on the relevant scale of the parameters. See Supplementary Methods 1.3 for our discussion of our choice of priors.

### No Controls ###

nc17 <- brm(formula = Chose_R ~

              # Predictors of interest
              Location + Pueblo + Religion + 
              
              # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
              Good + Trust + Wealth +
              
              # Random effects
              (1|RID) + (1|Population) + (1|Version),
            
            data = full3, family = bernoulli, prior = priors, control = list(adapt_delta = 0.999999, max_treedepth = 12, stepsize = 0.01),
            chains = num.chains, cores = num.cores, seed = semilla)


### Controls ###

c17 <- brm(formula = Chose_R ~
             
             # Predictors of interest
             Location + Pueblo + Religion +
             
             # Marginal effects: the role of other things important to partner choice (see Pisor & Gurven 2018)
             Good + Trust + Wealth +
             
             # Controls 
             Age.z + Sex + School.z + NetIncome.z +
             
             # Random effects
             (1|RID) + (1|Population) + (1|Version),
           data = full3, family = bernoulli, prior = priors, control = list(adapt_delta = 0.99, max_treedepth = 12),
           chains = num.chains, cores = num.cores, seed = semilla)
