library(data.table)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(FRACTION)
library(DT)
library(stringr)
library(knitr)
library(kableExtra)

#---------------------------Daten-----------------------------------------------
basedata <- read.csv("Bodyfat.csv")
basedata_german <- data.frame(
  Dichte = basedata$Density,
  Körperfett_in_Prozent = basedata$bodyfat,
  Alter_in_Jahre = basedata$Age,
  Gewicht_in_kg = basedata$Weight,
  Größe_in_Zoll = basedata$Height,
  Halsumfang_in_cm = basedata$Neck,
  Brustumfang_in_cm = basedata$Chest,
  Bauchumfang_in_cm = basedata$Abdomen,
  Hüftumfang_in_cm = basedata$Hip,
  Oberschenkelumfang_in_cm = basedata$Thigh,
  Knieumfang_in_cm = basedata$Knee,
  Knöchelumfang_in_cm = basedata$Ankle,
  Bizepsumfang_in_cm = basedata$Biceps,
  Unterarmumfang_in_cm = basedata$Forearm,
  Handgelenkumfang_in_cm = basedata$Wrist
)

n <- nrow(basedata_german)

bodyfat_and_abdomen <-
  data.frame(
    Körperfett = basedata_german$Körperfett_in_Prozent,
    Bauchumfang = basedata_german$Bauchumfang_in_cm
  )

bodyfat_and_abdomen_sample <- head(bodyfat_and_abdomen, 10)

n_sample <- nrow(bodyfat_and_abdomen_sample)

ols <- data.frame(Xi = bodyfat_and_abdomen_sample$Körperfett,
                  Yi = bodyfat_and_abdomen_sample$Bauchumfang)
ols$XiYi <-
  (bodyfat_and_abdomen_sample$Körperfett * bodyfat_and_abdomen_sample$Bauchumfang)
ols$Xi2 <- '^'(bodyfat_and_abdomen_sample$Körperfett, 2)
ols$Yi2 <- '^'(bodyfat_and_abdomen_sample$Bauchumfang, 2)
ols_sum <- colSums(ols)
ols[nrow(ols) + 1, ] <- ols_sum

names(ols) <-
  c("$x_{i}$", "$y_{i}$", "$x_{i}y_{i}$", "$x^{2}$", "$y^{2}$")
knitr::kable(ols, escape = FALSE)

ols_table <- kable(ols) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(nrow(ols),
           bold = TRUE,
           color = "white",
           background = "grey")

delta <-
  ols_sum[['Xi2']] * n_sample - ols_sum[['Xi']] * ols_sum[['Xi']]
delta_a <-
  ols_sum[['XiYi']] * n_sample - ols_sum[['Xi']] * ols_sum[['Yi']]
delta_b <-
  ols_sum[['Xi2']] * ols_sum[['Yi']] - ols_sum[['XiYi']] * ols_sum[['Xi']]
a <- delta_a / delta
b <- delta_b / delta

XiYi_average <- ols_sum[['XiYi']]/n_sample
Xi_average <- ols_sum[['Xi']]/n_sample
Yi_average <- ols_sum[['Yi']]/n_sample
Xi_deviation <- sd(bodyfat_and_abdomen_sample$Körperfett)
Yi_deviation <- sd(bodyfat_and_abdomen_sample$Bauchumfang)
correlation <- cor(bodyfat_and_abdomen_sample$Körperfett, bodyfat_and_abdomen_sample$Bauchumfang)
#--------------------------Methoden---------------------------------------------
print_paged_table  <- function(dataset) {
  if (is.null(dataset)) {
    stop("Datensätze fehlen...")
  }
  else {
    datatable(
      dataset,
      rownames = FALSE,
      extensions = c('FixedColumns', "FixedHeader"),
      options = list(
        columnDefs = list(list(
          className = 'dt-center', targets = "_all"
        )),
        language = list(url = "de-DE.json"),
        pageLength = 20,
        scrollX = TRUE,
        fixedHeader = TRUE,
        rownames = FALSE
      )
    )
  }
}

print_scatterplot <-
  function(dataset, x_data, y_data, draw_regression = FALSE, plot_title="") {
    x_label <- "X"
    y_label <- "Y"
    x_parameter_name <- toString(deparse(substitute(x_data)))
    y_parameter_name <- toString(deparse(substitute(y_data)))
    if (str_detect(x_parameter_name, "$") == TRUE) {
      x_label <-
        strsplit(x_parameter_name, split = "\\$|,")[[1]][2]
    }
    else{
      x_label <- x_parameter_name
    }
    if (str_detect(y_parameter_name, "$") == TRUE) {
      y_label <-
        strsplit(y_parameter_name, split = "\\$|,")[[1]][2]
    }
    else{
      y_label <- y_parameter_name
    }
    
    gp <- ggplot(dataset, aes(x = x_data, y = y_data)) +
      geom_point() +
      theme_bw() +
      labs(x = paste(x_label, "(X)"), y = paste(y_label, "(Y)"), title=plot_title) +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(breaks = seq(0, 200, by = 5)) +
      scale_x_continuous(breaks = seq(0, 50, by = 5)) +
      if (draw_regression == TRUE) {
        stat_smooth(method = "lm", se = FALSE, aes(x = x_data, y = y_data))
      }
    
    
    ggplotly(gp)
  }
