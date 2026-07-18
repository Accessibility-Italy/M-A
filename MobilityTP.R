## Working Directory -----------------------------------------------------------
getwd()
setwd("C:/Users/Feder/Documents/UniTus/Ricerca")

## Packages --------------------------------------------------------------------
library(readxl)
library(FuzzyPovertyR)
library(sf)
library(ggplot2)
library(spdep)

## Data ------------------------------------------------------------------------

### Shapefiles -----------------------------------------------------------------
shp <- read_sf(dsn = "C:/Users/Feder/Documents/UniSi/Scienze Statistiche per le Indagini Campionarie/Working Paper/Limiti01012021/Com01012021",
               layer = "Com01012021_WGS84")
shp[shp$COMUNE %in% c("Montecopiolo", "Sassofeltrio"), 1:8]
shp[shp$COMUNE %in% c("Montecopiolo", "Sassofeltrio"), 1] <- 2
shp[shp$COMUNE %in% c("Montecopiolo", "Sassofeltrio"), 2] <- 8
shp[shp$COMUNE %in% c("Montecopiolo", "Sassofeltrio"), 3] <- 99
shp[shp$COMUNE %in% c("Montecopiolo", "Sassofeltrio"), 5] <- 99
shp[shp$COMUNE == "Montecopiolo", 6] <- 99030
shp[shp$COMUNE == "Sassofeltrio", 6] <- 99031
shp[shp$COMUNE == "Montecopiolo", 7] <- "099030"
shp[shp$COMUNE == "Sassofeltrio", 7] <- "099031"
shp[shp$COMUNE %in% c("Montecopiolo", "Sassofeltrio"), 1:8]
shp <- st_make_valid(shp)

REG.shp <- read_sf(dsn = "C:/Users/Feder/Documents/UniSi/Scienze Statistiche per le Indagini Campionarie/Working Paper/Limiti01012026/Reg01012026",
                   layer = "Reg01012026_WGS84")
PROV.shp <- read_sf(dsn = "C:/Users/Feder/Documents/UniSi/Scienze Statistiche per le Indagini Campionarie/Working Paper/Limiti01012021/ProvCM01012021",
                    layer = "ProvCM01012021_WGS84")

### Accessibility of Municipalities --------------------------------------------
acc_municip <- read_excel("Data/Accessibility of Municipalities/20231219_Matrice_Indici_Accessibilità.xlsx",
                          sheet = "Accessibilità comuni",
                          skip = 1)
head(acc_municip)

acc_municip <- acc_municip[, c(1:5, 12:13, 16:19)]

colnames(acc_municip) <- c("COD_REG", "DEN_REG", "COD_UTS", "DEN_UTS", "CAPOLUOGO_UTS", 
                           "PROCOM_T", "DEN_COM", "STAZIONI_MIN", "AUTOSTRADA_MIN", 
                           "AEROPORTI_MIN", "PORTI_MIN")
head(acc_municip)

acc_municip[acc_municip$DEN_COM %in% c("Montecopiolo", "Sassofeltrio"), 1:7]
acc_municip[acc_municip$DEN_COM %in% c("Montecopiolo", "Sassofeltrio"), 1] <- "08"
acc_municip[acc_municip$DEN_COM %in% c("Montecopiolo", "Sassofeltrio"), 2] <- "Emilia-Romagna"
acc_municip[acc_municip$DEN_COM %in% c("Montecopiolo", "Sassofeltrio"), 3] <- "099"
acc_municip[acc_municip$DEN_COM %in% c("Montecopiolo", "Sassofeltrio"), 4] <- "Rimini"
acc_municip[acc_municip$DEN_COM == "Montecopiolo", 6] <- "099030"
acc_municip[acc_municip$DEN_COM == "Sassofeltrio", 6] <- "099031"
acc_municip[acc_municip$DEN_COM %in% c("Montecopiolo", "Sassofeltrio"), 1:7]

acc_municip$PROCOM_T2 <- ifelse(nchar(acc_municip$PROCOM_T) == 7, 
                                "058091", 
                                acc_municip$PROCOM_T)

# Section 2 --------------------------------------------------------------------

## Summary Statistics ----------------------------------------------------------
round(sapply(acc_municip[, 8:11], summary), 1)

# Section 3 --------------------------------------------------------------------
municip_sf <- merge(shp, acc_municip[, -c(1, 3, 7)], 
                    by.x = "PRO_COM_T", by.y = "PROCOM_T2")
head(municip_sf)

## 3.1. Mobility Transport Poverty ---------------------------------------------
fuzzy.TP <- data.frame(
  ID = municip_sf$PROCOM_T
)

### Dimension-specific indicators ----------------------------------------------
train.time <- fm_construct(predicate = municip_sf$STAZIONI_MIN,
                           ID = municip_sf$PROCOM_T,
                           fm = "cerioli",
                           z1 = quantile(municip_sf$STAZIONI_MIN, 0.01),
                           z2 = quantile(municip_sf$STAZIONI_MIN, 0.99))$results

fuzzy.TP <- merge(fuzzy.TP, train.time[, c(1, 4)],
                  by = "ID")
colnames(fuzzy.TP)[2] <- "train.time"
fuzzy.TP$train.time <- 1 - fuzzy.TP$train.time
head(fuzzy.TP)

motorway.time <- fm_construct(predicate = municip_sf$AUTOSTRADA_MIN,
                             ID = municip_sf$PROCOM_T,
                             fm = "cerioli",
                             z1 = quantile(municip_sf$AUTOSTRADA_MIN, 0.01),
                             z2 = quantile(municip_sf$AUTOSTRADA_MIN, 0.99))$results
fuzzy.TP <- merge(fuzzy.TP, motorway.time[, c(1, 4)],
                  by = "ID")
colnames(fuzzy.TP)[3] <- "motorway.time"
fuzzy.TP$motorway.time <- 1 - fuzzy.TP$motorway.time
head(fuzzy.TP)

airport.time <- fm_construct(predicate = municip_sf$AEROPORTI_MIN,
                             ID = municip_sf$PROCOM_T,
                             fm = "cerioli",
                             z1 = quantile(municip_sf$AEROPORTI_MIN, 0.01),
                             z2 = quantile(municip_sf$AEROPORTI_MIN, 0.99))$results
fuzzy.TP <- merge(fuzzy.TP, airport.time[, c(1, 4)],
                  by = "ID")
colnames(fuzzy.TP)[4] <- "airport.time"
fuzzy.TP$airport.time <- 1 - fuzzy.TP$airport.time
head(fuzzy.TP)

port.time <- fm_construct(predicate = municip_sf$PORTI_MIN,
                          ID = municip_sf$PROCOM_T,
                          fm = "cerioli",
                          z1 = quantile(municip_sf$PORTI_MIN, 0.01),
                          z2 = quantile(municip_sf$PORTI_MIN, 0.99))$results
fuzzy.TP <- merge(fuzzy.TP, port.time[, c(1, 4)],
                  by = "ID")
colnames(fuzzy.TP)[5] <- "port.time"
fuzzy.TP$port.time <- 1 - fuzzy.TP$port.time
head(fuzzy.TP)

### Composite Fuzzy Mobility Index ---------------------------------------------

#### Equal weights -------------------------------------------------------------
fuzzy.TP$CMTP.eq <- rowMeans(fuzzy.TP[, 2:5])

summary(fuzzy.TP$CMTP.eq)
boxplot(fuzzy.TP$CMTP.eq)

#### PCA-based weights ---------------------------------------------------------
X <- fuzzy.TP[, c(2:5)]
pca <- prcomp(X, scale = FALSE)
pca$sdev^2 / sum(pca$sdev^2)
loadings_pc1 <- pca$rotation[, 1]

round(w.pca <- abs(loadings_pc1) / sum(abs(loadings_pc1)), 3)

fuzzy.TP$CMTP.pca <- as.matrix(fuzzy.TP[, 2:5]) %*% w.pca
summary(fuzzy.TP$CMTP.pca)
boxplot(fuzzy.TP$CMTP.pca)

#### Passenger-flows-based weights ---------------------------------------------
w.flow <- c(
  0.057, # Railway
  0.739 + 0.109, # Road
  0.090, # Air
  0.01 # Sea
)
fuzzy.TP$CMTP.flow <- NA
for (i in 1:nrow(fuzzy.TP)) {
  fuzzy.TP[i, 8] <- weighted.mean(x = fuzzy.TP[i, 2:5], w = w.flow)
}
summary(fuzzy.TP$CMTP.flow)
boxplot(fuzzy.TP$CMTP.flow)

head(fuzzy.TP)
cor(fuzzy.TP[, 6:8])

### Spatial Distribution -------------------------------------------------------
municip_sf <- merge(municip_sf, fuzzy.TP[, c(1, 6:8)],
                    by.x = "PROCOM_T", by.y = "ID")

CMTP.pca.plot <- ggplot(data = municip_sf) +
  geom_sf(aes(fill = CMTP.pca, color = CMTP.pca)) +
#  geom_sf(data = REG.shp, fill = NA, color = "red", linewidth = 0.3) +
  geom_sf(data = PROV.shp, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "grey90", 
    high = "black",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    na.value = "white",
    name = "CMTP"
  ) +
  scale_color_gradient2(
    low = "grey90", 
    high = "black",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    na.value = "white",
    guide = NULL
  ) +
#  labs(title = "PCA-based Weights") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))
# png("Studium/Imgs/Fig_cart.png", width = 2000, height = 2500, res = 200, bg = "transparent")
CMTP.pca.plot
# dev.off()

#### Global Autocorrelation ----------------------------------------------------
neigh <- poly2nb(municip_sf)
queen <- nb2listw(neigh, style = "W", zero.policy = TRUE)

moran.test(x = municip_sf$CMTP.pca,
           listw = queen,
           zero.policy = T,
           randomisation = T)

#### Local Autocorrelation -----------------------------------------------------
set.seed(1)
LISA.queen <- localmoran_perm(c(municip_sf$CMTP.pca), nsim = 9999, queen)
p_localM <- cbind(LISA.queen[, 1:9], p.adjust(LISA.queen[, 6], method = "fdr"))
colnames(p_localM)[10] <- "FDR"
p_localM <- data.frame(p_localM)
p_localM$ID <- municip_sf$PROCOM_T
p_localM$sig <- ifelse(p_localM$Pr.z....E.Ii.. < 0.05,1,0)
p_localM$sigFDR <- ifelse(p_localM$FDR < 0.05,1,0)
p_localM$quadr <- attr(LISA.queen,"quadr")[, 1]
sapply(p_localM[, c(12:13)], table)
LISA1 <- merge(municip_sf[, c(1, 23)],
               p_localM[, c(11:14)],
               by.x = "PROCOM_T",
               by.y = "ID")
LISA1$quadr.sig.queen <- ifelse(LISA1$sig == 1, LISA1$quadr, "Not significant")
LISA1$quadr.sig.queen <- car::recode(LISA1$quadr.sig,
                                     "'1'='Low-Low';'2'='High-Low';'3'='Low-High';'4'='High-High'")
LISA1$quadr.sigFDR.queen <- ifelse(LISA1$sigFDR == 1, LISA1$quadr, "Not significant")
LISA1$quadr.sigFDR.queen <- car::recode(LISA1$quadr.sigFDR.queen,
                                        "'1'='Low-Low';'2'='High-Low';'3'='Low-High';'4'='High-High'")
table(LISA1$quadr.sig.queen, LISA1$quadr.sigFDR.queen, useNA = "ifany")
table(LISA1$quadr.sigFDR.queen, useNA = "ifany")

queen.plot <- ggplot(LISA1, aes(fill = quadr.sigFDR.queen, color = quadr.sigFDR.queen)) +
  geom_sf() +
#  geom_sf(data = REG.shp, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = PROV.shp, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    "Quadrant",
    values = c("Low-Low" = "green3",
               "Low-High" = "red4",
               "High-Low" = "red2",
               "High-High" = "green4",
               "Not significant" = "grey"),
    na.value = "white"
  ) +
  scale_color_manual(
    "Quadrant",
    values = c("Low-Low" = "green3",
               "Low-High" = "red4",
               "High-Low" = "red2",
               "High-High" = "green4",
               "Not significant" = "grey"),
    na.value = "white",
    guide = NULL
  ) +
  labs(title = "Local Moran's I") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

# png("Studium/Imgs/LISA.png", width = 2000, height = 2500, res = 250, bg = "transparent")
queen.plot
# dev.off()

HH.id <- LISA1[LISA1$quadr.sigFDR.queen == "High-High", ]$PROCOM_T
HH <- municip_sf[municip_sf$PROCOM_T %in% HH.id, c(1, 9, 16, 15, 23)]
View(HH)
HL.id <- LISA1[LISA1$quadr.sigFDR.queen == "High-Low", ]$PROCOM_T
HL <- municip_sf[municip_sf$PROCOM_T %in% HL.id, c(1, 9, 16, 15, 23)]
View(HL)
LL.id <- LISA1[LISA1$quadr.sigFDR.queen == "Low-Low", ]$PROCOM_T
LL <- municip_sf[municip_sf$PROCOM_T %in% LL.id, c(1, 9, 16, 15, 23)]
View(LL)