# This script analyses data on telemore length in lymphocytes and granulocytes
# Script by Gustav Nilsonne

##### Initialise
setwd("C:/Users/gusta/Box Sync/Gustavs_arbete/Pek/Beware the tissue blender/beware-the-tissue-blender")
require(nlme)
require(brms)
require(RColorBrewer)
cols <- brewer.pal(n = 5, name = "Dark2")

##### Read data

# Aubert et al dataset
data_Aubert <- read.csv2("journal.pgen.1002696.s005.csv")

# Werner et al dataset
data_Werner <- read.csv2("Werner_2013.csv")

# Alder et al dataset
data_Alder <- read.csv("data_Alder.csv")

# Merge data
data_Aubert <- subset(data_Aubert, select = c("age", "gender", "Lympho", "Granulo"))
names(data_Aubert) <- c("age", "sex", "tl_lymphoc", "tl_granuloc")
data_Aubert$dataset <- "Aubert"

data_Werner <- subset(data_Werner, select = c("Alter", "Ly_TEL", "Gran_TEL"))
names(data_Werner) <- c("age", "tl_lymphoc", "tl_granuloc")
data_Werner$sex <- NA
data_Werner$dataset <- "Werner"

data_Alder$sex <- NA
data_Alder$dataset <- "Alder"

data <- rbind(data_Aubert, data_Werner)
data <- rbind(data, data_Alder)

data_adults <- data[data$age > 17, ]

  
#### Plot data
# Lymphocyte TL vs granulocyte TL
min <- min(c(data$tl_lymphoc, data$tl_granuloc), na.rm = T) # To get all plots on same scale
max <- max(c(data$tl_lymphoc, data$tl_granuloc), na.rm = T)

plot(tl_lymphoc ~ tl_granuloc, data = data[data$dataset == "Aubert", ], xlim = c(min, max), ylim = c(min, max), frame.plot = F, type = "n", main = "Aubert, all data")
abline(a = 0, b = 1, col = "gray")
points(tl_lymphoc ~ tl_granuloc, data = data[data$dataset == "Aubert", ])
abline(lm(tl_lymphoc ~ tl_granuloc, data[data$dataset == "Aubert", ]), col = cols[1], lwd = 2)

plot(tl_lymphoc ~ tl_granuloc, data = data_adults[data_adults$dataset == "Aubert", ], xlim = c(min, max), ylim = c(min, max), frame.plot = F, type = "n", main = "Aubert, adults")
abline(a = 0, b = 1, col = "gray")
points(tl_lymphoc ~ tl_granuloc, data = data_adults[data_adults$dataset == "Aubert", ])
abline(lm(tl_lymphoc ~ tl_granuloc, data_adults[data_adults$dataset == "Aubert", ]), col = cols[1], lwd = 2)

plot(tl_lymphoc ~ tl_granuloc, data = data[data$dataset == "Werner", ], xlim = c(min, max), ylim = c(min, max), frame.plot = F, type = "n", main = "Werner")
abline(a = 0, b = 1, col = "gray")
points(tl_lymphoc ~ tl_granuloc, data = data[data$dataset == "Werner", ])
abline(lm(tl_lymphoc ~ tl_granuloc, data[data$dataset == "Werner", ]), col = cols[1], lwd = 2)

plot(tl_lymphoc ~ tl_granuloc, data = data[data$dataset == "Alder", ], xlim = c(min, max), ylim = c(min, max), frame.plot = F, type = "n", main = "Alder, all data")
abline(a = 0, b = 1, col = "gray")
points(tl_lymphoc ~ tl_granuloc, data = data[data$dataset == "Alder", ])
abline(lm(tl_lymphoc ~ tl_granuloc, data[data$dataset == "Alder", ]), col = cols[1], lwd = 2)

plot(tl_lymphoc ~ tl_granuloc, data = data_adults[data_adults$dataset == "Alder", ], xlim = c(min, max), ylim = c(min, max), frame.plot = F, type = "n", main = "Alder, adults")
abline(a = 0, b = 1, col = "gray")
points(tl_lymphoc ~ tl_granuloc, data = data_adults[data_adults$dataset == "Alder", ])
abline(lm(tl_lymphoc ~ tl_granuloc, data_adults[data_adults$dataset == "Alder", ]), col = cols[1], lwd = 2)

# Plot both cell types by age
plot(tl_lymphoc ~ age, data = data[data$dataset == "Aubert", ], ylim = c(min, max), frame.plot = F, type = "n", main = "Aubert", ylab = "TL, kbp")
points(tl_lymphoc ~ age, data = data[data$dataset == "Aubert", ], col = cols[2])
points(tl_granuloc ~ age, data = data[data$dataset == "Aubert", ], col = cols[3])
legend("topright", legend = c("lymphocytes", "granulocytes"), col = cols[2:3], pch = 1)
clip(18, 102, 0, 20) # Show regression for adults only
abline(lm(tl_lymphoc ~ age, data = data_adults[data_adults$dataset == "Aubert", ]), col = cols[2], lwd = 2)
abline(lm(tl_granuloc ~ age, data = data_adults[data_adults$dataset == "Aubert", ]), col = cols[3], lwd = 2)

plot(tl_lymphoc ~ age, data = data[data$dataset == "Werner", ], ylim = c(min, max), frame.plot = F, type = "n", main = "Werner", ylab = "TL, kbp")
points(tl_lymphoc ~ age, data = data[data$dataset == "Werner", ], col = cols[2])
points(tl_granuloc ~ age, data = data[data$dataset == "Werner", ], col = cols[3])
legend("topright", legend = c("lymphocytes", "granulocytes"), col = cols[2:3], pch = 1)

plot(tl_lymphoc ~ age, data = data[data$dataset == "Alder", ], ylim = c(min, max), frame.plot = F, type = "n", main = "Alder", ylab = "TL, kbp")
points(tl_lymphoc ~ age, data = data[data$dataset == "Alder", ], col = cols[2])
points(tl_granuloc ~ age, data = data[data$dataset == "Alder", ], col = cols[3])
legend("topright", legend = c("lymphocytes", "granulocytes"), col = cols[2:3], pch = 1)
clip(18, 85, 0, 20) # Show regression for adults only
abline(lm(tl_lymphoc ~ age, data = data_adults[data_adults$dataset == "Alder", ]), col = cols[2], lwd = 2)
abline(lm(tl_granuloc ~ age, data = data_adults[data_adults$dataset == "Alder", ]), col = cols[3], lwd = 2)


#### Modelling
lm1 <- lm(tl_lymphoc ~ tl_granuloc, data, na.action = "na.omit")
lm2 <- lm(tl_lymphoc ~ tl_granuloc + age, data, na.action = "na.omit")
lm3 <- lm(tl_lymphoc ~ tl_granuloc * age, data, na.action = "na.omit")
lm4 <- lme(tl_lymphoc ~ tl_granuloc * age, random = ~ 1|dataset, data, na.action = "na.omit")
lm5 <- lme(tl_lymphoc ~ tl_granuloc * age, random = ~ age|dataset, data, na.action = "na.omit")
lm6 <- lme(tl_lymphoc ~ tl_granuloc * age + I(age^2), random = ~ age|dataset, data, na.action = "na.omit")

lm1a <- lm(tl_lymphoc ~ tl_granuloc, data_adults, na.action = "na.omit")
lm2a <- lm(tl_lymphoc ~ tl_granuloc + age, data_adults, na.action = "na.omit")
lm3a <- lm(tl_lymphoc ~ tl_granuloc * age, data_adults, na.action = "na.omit")
lm4a <- lme(tl_lymphoc ~ tl_granuloc * age, random = ~ 1|dataset, data_adults, na.action = "na.omit")
lm5a <- lme(tl_lymphoc ~ tl_granuloc * age, random = ~ age|dataset, data_adults, na.action = "na.omit")
lm6a <- lme(tl_lymphoc ~ tl_granuloc * age + I(age^2), random = ~ age|dataset, data_adults, na.action = "na.omit")


# Bayesian modelling
# Not working at present
brm5a <- brm(formula = tl_lymphoc ~ tl_granuloc * age
            + (1 + age|dataset),
            data = data_adults, family = student(),
            prior = c(set_prior("normal(0,5)", class = "b"),
                      set_prior("normal(0,2)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 100, iter = 200, chains = 4,
            control = list(adapt_delta = 0.95))


