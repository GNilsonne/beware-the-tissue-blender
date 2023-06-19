alter_b <- read.csv2("./Alter_2007_fig1_digitized_2023-06-12/b-cells.csv")
alter_gran <- read.csv2("./Alter_2007_fig1_digitized_2023-06-12/Granulocytes.csv")
alter_lymph <- read.csv2("./Alter_2007_fig1_digitized_2023-06-12/lymphocytes.csv")
alter_mem <- read.csv2("./Alter_2007_fig1_digitized_2023-06-12/memory-t.csv")
alter_naive <- read.csv2("./Alter_2007_fig1_digitized_2023-06-12/naive-t.csv")
alter_nk <- read.csv2("./Alter_2007_fig1_digitized_2023-06-12/nk-cells.csv")

dfs <- list(list(alter_b, "B cells"), list(alter_gran, "Granulocytes"), list(alter_lymph, "Lymphocytes"), list(alter_mem, "Memory T cells"), list(alter_naive, "Naive T cells"), list(alter_nk, "NK cells"))

write_plots <- function(df_tuple) {
  df<- df_tuple[[1]]
  df_title <- df_tuple[[2]]
    colnames(df) <- c("age", "tellength")
    png(file.path("output", paste(df_title, "png", sep=".")))
    plot(df$tellength ~ df$age, xlab="Age (years)", ylab="Telomere length (kb)", xlim=c(0,100), ylim=c(0,14), main=df_title)
dev.off()
    }

sapply(dfs, write_plots)
