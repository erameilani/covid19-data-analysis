#Load packages
library(httr)
library(stats)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(hrbrthemes)

#Mengakses API
resp_jkt <- GET("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json")
cov_jkt_raw <- content(resp_jkt, as = "parsed", simplifyVector = TRUE)

cov_jkt <- cov_jkt_raw$list_perkembangan
str(cov_jkt)

#Membersihkan data
new_cov_jkt <- 
  cov_jkt %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
new_cov_jkt

#Grafik untuk kasus harian positif
ggplot(new_cov_jkt, aes(tanggal, kasus_baru)) + 
  geom_col(fill = "salmon") + 
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di DKI Jakarta",
    subtitle = "Terjadi pelonjakan kasus di bulan Juli",
    caption = "Sumber data: covid19.go.id"
  ) + 
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) + 
  theme(plot.title.position = "plot")

#Grafik untuk kasus harian sembuh
ggplot(new_cov_jkt, aes(tanggal, sembuh)) + 
  geom_col(fill = "olivedrab2") + 
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di DKI Jakarta",
    caption = "Sumber data: covid19.go.id"
  ) + 
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) + 
  theme(plot.title.position = "plot")

#Grafik untuk kasus harian meninggal
ggplot(new_cov_jkt, aes(tanggal, meninggal)) + 
  geom_col(fill = "darkslategray4") + 
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di DKI Jakarta",
    caption = "Sumber data: covid19.go.id"
  ) + 
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) + 
  theme(plot.title.position = "plot")

#Data kasus pekanan
cov_jkt_pekanan <- 
  new_cov_jkt %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )
glimpse(cov_jkt_pekanan)

#Apakah pekan ini lebih baik?
cov_jkt_pekanan <- 
  cov_jkt_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(cov_jkt_pekanan)

#Grafik untuk kasus pekanan positif
ggplot(cov_jkt_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik)) + 
  geom_col(show.legend = FALSE) + 
  scale_x_continuous(breaks = 9:31, expand = c(0, 0)) + 
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) + 
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di DKI Jakarta",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu
pekan sebelumnya",
    caption = "Sumber data: covid19.go.id"
  ) + 
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) + 
  theme(plot.title.position = "plot")

#Data akumulasi
cov_jkt_akumulasi <-
  new_cov_jkt %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
tail(cov_jkt_akumulasi)

#Transformasi data
cov_jkt_akumulasi_pivot <- 
  cov_jkt_akumulasi %>% 
  pivot_longer(
    cols = -tanggal,
    names_to = "kategori",
    names_prefix = "akumulasi_",
    values_to = "jumlah"
  )
glimpse(cov_jkt_akumulasi_pivot)

#Grafik untuk dinamika kasus
ggplot(cov_jkt_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) + 
  geom_line(size = 0.9) + 
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) + 
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ), 
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) + 
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di DKI Jakarta",
    caption = "Sumber data: covid19.go.id"
  ) + 
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
