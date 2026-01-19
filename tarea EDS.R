#=============================================================
# PROBLEMA A RESOLVER
# Confeccionar un dashboad que resuma el "performance" de un  portfolio de inversiones.
#=============================================================
# PLANIFICACIÓN
# 1) Instalar librerias a utilizar
# 2) Descargar información financiera para una acción
# 3) Construir tabla con variables a utilizar
# Composición del cuadro:
# Nemotécnico | purchase_price | current_price | initial_value | current_value |
# Ganancia/Perdida | dividendos_recibidos | TIR_mensual_con_dividendos |
# SMA(200) | SMA1(50) | SlowD | RSI (última columna)
# 4) Luego agregar más acciones a la tabla, 5 o 6 más
# 5) Generar una alerta cuando precio llegue a un umbral y enviar mensaje por 
# whatsapp o correo electronico
# 6) Dar formato a la tabla y enviar semanalmente los días sábado
#=============================================================

# 1) 

library(quantmod)
library(TTR)
library(lubridate)
library(xts)

# ============================================================
# AGUAS-A (Chile) - Cuadro Indicadores Portafolio + TA
# ============================================================

# ----------------
# 2) Parámetros
# ----------------
nemotecnico <- "AGUAS-A"         # Nemotécnico (lo que quieres ver en la tabla)
ticker_yahoo <- "AGUAS-A.SN"     # Símbolo en Yahoo Finance
shares <- 5392
purchase_date <- as.Date("2025-06-27")

# Ventana suficientemente larga para SMA200 (y holgura)
from_date <- purchase_date - days(450)
to_date   <- Sys.Date()

# ----------------
# Descargar precios (OHLCV)
# ----------------
AGUAS <- getSymbols(ticker_yahoo, src = "yahoo",
                    from = from_date, to = to_date,
                    auto.assign = FALSE)

px_close <- Cl(AGUAS)
available_dates <- index(px_close)

# ----------------
# Precio de compra (si el día no existe, usa el día hábil anterior disponible)
# ----------------
if (all(available_dates > purchase_date)) {
  stop("No hay datos anteriores o iguales a la fecha de compra. Aumenta 'from_date'.")
}

if (!(purchase_date %in% available_dates)) {
  warning(paste0("El ", purchase_date, " no tiene precio (feriado/no transó). ",
                 "Usaré el día hábil ANTERIOR disponible."))
}

purchase_effective_date <- max(available_dates[available_dates <= purchase_date])
purchase_price <- as.numeric(px_close[purchase_effective_date])

# ----------------
# Precio actual (último cierre disponible)
# ----------------
current_date  <- max(available_dates)
current_price <- as.numeric(px_close[current_date])

# ----------------
# Valores del portafolio
# ----------------
initial_value <- shares * purchase_price
current_value <- shares * current_price
gain_loss <- current_value - initial_value   # (sin dividendos) como pediste

# ----------------
# Dividendos recibidos desde la compra
# ----------------
div_ps <- getDividends(ticker_yahoo, src = "yahoo",
                       from = purchase_effective_date, to = current_date,
                       auto.assign = FALSE)

div_ps <- na.omit(div_ps)
dividends_total <- if (NROW(div_ps) == 0) 0 else sum(as.numeric(div_ps)) * shares

# ----------------
# TIR mensual incluyendo dividendos
# ----------------
final_value_total <- current_value + dividends_total
months_elapsed <- time_length(interval(purchase_effective_date, current_date), unit = "months")

tir_monthly_div <- if (months_elapsed <= 0) {
  NA_real_
} else {
  (final_value_total / initial_value)^(1 / months_elapsed) - 1
}

# ----------------
# Indicadores técnicos (último período)
# ----------------
sma_50  <- SMA(px_close, n = 50)
sma_200 <- SMA(px_close, n = 200)

rsi_14 <- RSI(px_close, n = 14)

stoch_14_3_3 <- stoch(HLC(AGUAS),
                      nFastK = 14, nFastD = 3, nSlowD = 3,
                      maType = "SMA")

# Tomar "último valor válido" (por si hay NA al inicio)
last_num <- function(x) {
  x2 <- na.omit(x)
  if (NROW(x2) == 0) NA_real_ else as.numeric(last(x2))
}

sma_200_last <- last_num(sma_200)
sma_50_last  <- last_num(sma_50)
slowD_last   <- last_num(stoch_14_3_3[, "slowD"])
rsi_last     <- last_num(rsi_14)

# ----------------
# 3) Cuadro indicadores (1 fila) con orden exacto de columnas
# ----------------
cuadro_indicadores <- data.frame(
  Nemotecnico = nemotecnico,
  purchase_price = purchase_price,
  current_price  = current_price,
  initial_value  = initial_value,
  current_value  = current_value,
  ganancia_perdida = gain_loss,
  dividendos_recibidos = dividends_total,
  TIR_mensual_con_dividendos = tir_monthly_div,
  SMA  = sma_200_last,   # SMA (200) último
  SMA1 = sma_50_last,    # SMA1 (50) último
  SlowD = slowD_last,    # SlowD último
  RSI = rsi_last         # RSI última columna
)

# (Opcional) Redondeo para ver en consola más “humano”
cuadro_print <- cuadro_indicadores
cuadro_print$purchase_price <- round(cuadro_print$purchase_price, 4)
cuadro_print$current_price  <- round(cuadro_print$current_price, 4)
cuadro_print$initial_value  <- round(cuadro_print$initial_value, 2)
cuadro_print$current_value  <- round(cuadro_print$current_value, 2)
cuadro_print$ganancia_perdida <- round(cuadro_print$ganancia_perdida, 2)
cuadro_print$dividendos_recibidos <- round(cuadro_print$dividendos_recibidos, 2)
cuadro_print$TIR_mensual_con_dividendos <- round(100 * cuadro_print$TIR_mensual_con_dividendos, 4) # %
cuadro_print$SMA  <- round(cuadro_print$SMA, 4)
cuadro_print$SMA1 <- round(cuadro_print$SMA1, 4)
cuadro_print$SlowD <- round(cuadro_print$SlowD, 4)
cuadro_print$RSI <- round(cuadro_print$RSI, 4)

print(cuadro_print)

# ----------------
# 4) Luego agregar más acciones a la tabla, 5 o 6 más
# ----------------





# print(div_ps)