##############
# Written by: @RProdigest
# Date: 9th April, 2023
#Description: Code to visualize count of downloads of ggplot2 and lattice
##############


# 1.0 Install necessary packages ----

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  cranlogs, #'API' to the database of 'CRAN' package downloads from the 'RStudio
  ggplot2, # plot the data
  )


# 2.0 Download the data using cran_downlaods from {cranlogs} ----

downloads <-
  cran_downloads(packages = c("lattice", "ggplot2"), when = "last-month")

# 3.0 glimpse the downloads dataframe and then plot it ----
glimpse(top_downloads)


# please note I use the native pipe, if your prefer the magrittr "%>%" pipe use it instead

downloads |>
  ggplot(aes(date, count, fill = package)) +
  scale_fill_manual(values = c("midnightblue", "gray50")) +
  geom_area() +
  labs(
    title = "A Tale of Two Libraries: ggplot2 vs Lattice in R Data Visualization",
    subtitle = "ggplot2 is the army swiss knife of data viz. with downloads almost peaking 100K in the last 30days",
    y = "download count",
    caption = "Source: cranlogs, plotted by @Rprodigest"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "week", date_labels = "%d-%b-%y") +
  scale_y_continuous(breaks = seq(0, 100000, 10000), labels = scales::comma) +
  theme(axis.line.y = element_line(colour = "gray7")) +
  theme(legend.position = "top", legend.title = element_blank()) 
  


# 4.0 You can draw your conclusions The END ---- 
