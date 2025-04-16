#Replication Project code
library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)
library(fixr)
library(ggplot2)
library(TropFishR)
library(zoo)

d <- read_csv("https://raw.githubusercontent.com/mon-doo/data-analysis-replication/refs/heads/main/data/Lionfish_length-frequency_data_-_Johnson_and_Swenarton.csv")
head(d)
d <- d |> 
  subset(select =-...11:-...21) |>
  fix_col_spaces()
d <- d |> subset(select = -April_2013:-August_2013) |>
  subset(select = -August_2015)
dat <- d |> subset(select = -January_2015)
d <- d |> pivot_longer(cols = c(2:7), names_to = "month", values_to = "value")
c1 <- c("Total_length_(TL)", "2014-04", "2014-07", "2014-08", "2014-10", "2014-11")
colnames(dat) = c1
d_new$month <- as.Date(as.yearmon(d_new$month))
dat <- dat |> pivot_longer(cols = c(2:6), names_to = "month", values_to = "value")
d_new <- tibble(tl = numeric(), month = character())
for (i in 1:nrow(dat)){
  records <- rep(dat[i,]$`Total_length_(TL)`, dat[i, ]$`value`)
  temp <- tibble(tl = records, month = dat[i, ]$`month`)
  d_new <- bind_rows(d_new, temp)
}

c1 <- c("Total_length_(TL)", "2014-04", "2014-07", "2014-08", "2014-10", "2014-11")
colnames(dat) = c1
dat <- dplyr::rename(d, 2014.04 = "April_2014", 2014.07 = "July_2014", 2014.08 = "August_2014", 2014.10 = "October_2014", 2014.11 = "November_2014")


nrow(d_new)
range(d_new$tl)
d_new$month[1]
d_new$month[3812]



(p <- ggplot(data = d_pivot, aes(x = `Total_length_(TL)`, y = value)) +
    geom_bar(stat = "identity") +
    facet_wrap(~factor(month, levels = c("April_2014", "October_2014","July_2014", "November_2014", "August_2014", "January_2015")), ncol =2, scales = "free"))







c1 <- c("Total_length_(TL)", "2014-04", "2014-07", "2014-08", "2014-10", "2014-11")
colnames(dat2) = c1
dat2 <- dplyr::rename(d, 2014.04 = "April_2014", 2014.07 = "July_2014", 2014.08 = "August_2014", 2014.10 = "October_2014", 2014.11 = "November_2014")




d_new$month <- as.Date(as.yearmon(d_new$month))
d_lfq <- lfqCreate(data=d_new, Lname="tl", Dname = "month", bin_size = 10)
lfqRestructure(d_lfq)
mod1 <- ELEFAN_SA(d_lfq, seasonalised = TRUE, low_par = list(Linf = 447.999999999, K=0.00, t_anchor=0, ts=0, C=0),
                  up_par = list(Linf = 448,K=4.00, t_anchor=1, ts=1, C=1), MA = 15, agemax = 3)
mod2 <- ELEFAN_SA(d_lfq, seasonalised = FALSE, low_par = list(Linf = 447.999999999, K=0.00, t_anchor=0, ts=0, C=0),
                  up_par = list(Linf = 448,K=4.00, t_anchor=1, ts=1, C=1), MA = 15, agemax = 3)
mod1$par
mod2$par

t <- seq(0, 3, length.out = 200)
Lt <- VBGF(param = list(Linf = 448, K = 0.4277936, t_anchor = 0.888901, C = 0.2310452, ts = 0.5349782), t =t)
plot(t, Lt)
Lt2 <- VBGF(param = list(Linf = 448, K = 0.4273379, t_anchor = 0.9203679), t =t)

plot(Lt+Lt2~t)
plot(Lt2)


0.4273379

$t_anchor
[1] 0.9203679

