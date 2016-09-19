data <- read.csv2("C:/Users/Gustav Nilsonne/Box Sync/Gustavs_arbete/Pek/Beware the tissue blender/beware-the-tissue-blender/journal.pgen.1002696.s005.csv")

data <- data[data$age > 17, ]

min <- min(c(data$Lympho, data$Granulo), na.rm = T)
max <- max(c(data$Lympho, data$Granulo), na.rm = T)
  
plot(Lympho ~ age, data = data, ylim = c(min, max), frame.plot = F, ylab = "TL", main = "Lymphocytes")
abline(lm(Lympho ~ age, data = data), col = "red")

plot(Granulo ~ age, data = data, ylim = c(min, max), frame.plot = F, ylab = "TL", main = "Granulocytes")
abline(lm(Granulo ~ age, data = data), col = "blue")

plot(Lympho ~ age, data = data, ylim = c(min, max), frame.plot = F, ylab = "TL", main = "", col = "red")
points(Granulo ~ age, data = data, ylim = c(min, max), col = "blue")
abline(lm(Lympho ~ age, data = data), col = "red")
abline(lm(Granulo ~ age, data = data), col = "blue")
legend("topright", legend = c("Lymphocytes", "Granulocytes"), col = c("blue", "red"), pch = 1)

plot(Lympho~Granulo, data = data, xlim = c(min, max), ylim = c(min, max), frame.plot = F, pch = "", main = "Aubert 2012", xlab = "Granulocyte TL, kbp", ylab = "Lymphocyte TL, kbp")
abline(a = 0, b = 1, col = "gray")
points(Lympho~Granulo, data = data)
abline(lm(Lympho ~ Granulo, data = data), col = "green", lty = 2)
summary(lm(Lympho ~ Granulo, data = data))
       
mean_l <- mean(data$Lympho, na.rm = T)
mean_g <- mean(data$Granulo, na.rm = T)

lower <- 0.4*mean_g + 0.6*mean_l
upper <- 0.8*mean_g + 0.2*mean_l

plot(x = c(0.4, 0.8), y = c(lower, upper), type = "l", frame.plot= F, xlab = "Granulocyte fraction", ylab = "TL", ylim = c(6.5, 7.5), main = "")
abline(v = 0.57, col = "red")
abline(v = 0.61, col = "red")

Werner_2013 <- read.csv2("C:/Users/Gustav Nilsonne/Box Sync/Gustavs_arbete/Pek/Beware the tissue blender/beware-the-tissue-blender/Werner_2013.csv")
plot(Ly_TEL~Gran_TEL, data = Werner_2013, xlim = c(7, 14), ylim = c(7, 14), frame.plot = F, pch = "", main = "Werner 2015", xlab = "Granulocyte TL, kbp", ylab = "Lymphocyte TL, kbp")
abline(a = 0, b = 1, col = "gray")
points(Ly_TEL~Gran_TEL, data = Werner_2013)
abline(lm(Ly_TEL~Gran_TEL, data = Werner_2013), col = "green", lty = 2)

estimate_vanTiel <- (0.646*mean_g + (1-0.646)*mean_l) - (0.586*mean_g + (1-0.586)*mean_l)
estimate_freedman <- (0.605*mean_g + (1-0.605)*mean_l) - (0.589*mean_g + (1-0.589)*mean_l)
estimate_smith <- (0.619*mean_g + (1-0.619)*mean_l) - (0.609*mean_g + (1-0.609)*mean_l)

barplot(c(estimate_vanTiel, estimate_freedman, estimate_smith)*1000, names.arg = c("van Tiel", "Freedman", "Smith"), ylab = "base pairs", ylim = c(0, 80))
