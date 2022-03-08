translation <- read.csv("dictionary.csv", header = TRUE, encoding = "UTF-8", sep = ",", as.is = TRUE)

save(translation, file = "translation.bin")



