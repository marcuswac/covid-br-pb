library(dplyr)
library(forcats)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(readxl)
library(scales)
library(stringr)
library(zoo)

carrega_dados_ms_xlsx <- function(arquivo) {
  res <- read_xlsx(arquivo, guess_max = 100000) %>%
    distinct() %>%
    mutate(data = ymd(data),
           codmun = as.character(codmun),
           obitosAcumulados = as.integer(obitosAcumulado),
           casosAcumulados = as.integer(casosAcumulado)) %>%
    arrange(data)
  return(res)
}

carrega_dados_ms_csv <- function(arquivo) {
  res <- read.csv(arquivo, na.strings = "") %>%
    distinct() %>%
    mutate(data = ymd(data),
           obitosAcumulados = as.integer(obitosAcumulado),
           casosAcumulados = as.integer(casosAcumulado)) %>%
    arrange(data)
}

carrega_dados_municipios_ms <- function(arquivo) {
  dados_ms <- if (is.data.frame(arquivo)) arquivo else carrega_dados_ms_csv(arquivo)
  
  todos <- dados_ms %>%
    filter(!is.na(codmun), !is.na(estado))

  #update_vars <- c("municipio", "populacaoTCU2019", "codRegiaoSaude",
  #                 "nomeRegiaoSaude")
  
  #completo <- todos %>%
  #    filter(!is.na(municipio)) 
  
  #municipios <- completo %>%
  #    select(estado, codmun, all_of(update_vars)) %>%
  #    distinct()
  
  #incompleto <- todos %>%
  #  filter(is.na(municipio)) %>%
  #  select(-all_of(update_vars)) %>%
  #  mutate(codmun = str_sub(str_trim(codmun), end = -2)) %>%
  #  left_join(municipios, by = c("estado", "codmun"))
  
  #completo <- completo %>%
  #  bind_rows(incompleto)
  
  return(todos)
}

carrega_dados_estados_ms <- function(arquivo) {
  dados_ms <- if (is.data.frame(arquivo)) arquivo else carrega_dados_ms_csv(arquivo)
  res <- dados_ms %>%
    filter(is.na(codmun), !is.na(estado))
  return(res)
}

carrega_dados_brasil_ms <- function(arquivo) {
  dados_ms <- if (is.data.frame(arquivo)) arquivo else carrega_dados_ms_csv(arquivo)
  res <- dados_ms %>%
    filter(!is.na(regiao), is.na(codmun), is.na(estado))
  return(res)
}

carrega_dados_pb_boletim <- function(arquivo) {
  res <- read.csv(arquivo) %>%
    mutate(data = ymd(data))
  return(res)
}

# data: dia-mês-ano
carrega_dados_municipios_covid19pb <- function(arquivo) {
  res <- read.csv(arquivo) %>%
    rename(obitosAcumulados = mortesAcumuladas,
           casosAcumulados = confirmadosAcumulados,
           micro = microrregiao,
           meso = mesorregiao)
  return(res)
}

carrega_dados_municipios_brasilio <- function(arquivo) {
  res <- read.csv(arquivo) %>%
    filter(place_type == "city") %>%
    rename(obitosAcumulados = deaths,
           casosAcumulados = confirmed,
           municipio = city,
           estado = state) %>%
    mutate(data = ymd(date))
  return(res)
}

sumariza_casos_obitos <- function(dados, min_obitos = 5, agrupar_por = NULL) {
  if (!is.null(agrupar_por)) {
    dados <- group_by_(dados, agrupar_por)
  }
  
  res <- dados %>%
    arrange(data) %>%
    mutate(
      diasXobitos = as.integer(
        difftime(data, data[which.max(obitosAcumulados >= min_obitos)],
                 units = "days")),
      obitosNovos = obitosAcumulados - lag(obitosAcumulados),
      obitosFracaoAumento = ifelse(lag(obitosAcumulados) > 0,
                                   obitosNovos / lag(obitosAcumulados), NA),
      obitosFracaoAumentoMedia = rollmean(obitosFracaoAumento, 7, fill = NA,
                                          align = "right"),
      totalObitos = max(obitosAcumulados, na.rm = TRUE),
      obitosNovosMedia = rollmean(obitosNovos, 7, fill = NA, align = "right"),
      dataMetadeObitos = last(data[obitosAcumulados < last(obitosAcumulados)/2]),
      diasDobrouObitos = as.integer(last(data) - dataMetadeObitos),
      casosNovos = casosAcumulados - lag(casosAcumulados),
      casosFracaoAumento = ifelse(lag(casosAcumulados) > 0,
                                  casosNovos / lag(casosAcumulados), NA),
      casosFracaoAumentoMedia = rollmean(casosFracaoAumento, 7, fill = NA,
                                         align = "right"),
      totalCasos = max(casosAcumulados),
      casosNovosMedia = rollmean(casosNovos, 7, fill = NA, align = "right"),
      dataMetadeCasos = last(data[casosAcumulados < last(casosAcumulados)/2]),
      diasDobrouCasos = as.integer(last(data) - dataMetadeCasos))
}

plot_novos_por_dia <- function(dados, novos_metrica, media_metrica,
                               log_y = FALSE) {
  novos <- enquo(novos_metrica)
  media <- enquo(media_metrica)
  
  last_day <- filter(dados, data == max(data))
  
  media_novos_ratio <- pull(last_day, !!media) / pull(last_day, !!novos)
  
  if (media_novos_ratio > 1.2 || media_novos_ratio < 0.8) {
    vjust_media <- "center"
    vjust_novos <- "center"
    
  } else if (media_novos_ratio < 1) {
    vjust_media <- "top"
    vjust_novos <- "bottom"
  } else {
    vjust_media <- "bottom"
    vjust_novos <- "top"
  }
  
  p <- ggplot(dados, aes(data, !!novos)) +
    geom_line(aes(y = !!media), lty = 1, size = 0.8, alpha = 0.8) +
    geom_point(alpha = 0.8, col = "#d9534f") +
    geom_line(alpha = 0.8, col = "#d9534f", lty = 2) +
    geom_text(aes(label = !!novos),
              data = last_day, col = "#d9534f",
              nudge_x = 1, size = 4, hjust = "left", fontface = "bold",
              vjust = vjust_novos) +
    geom_text(aes(y = !!media, label = round(!!media)),
              data = last_day,
              nudge_x = 1, size = 4, hjust = "left", fontface = "bold",
              vjust = vjust_media) +
    scale_x_date("Dia", date_breaks = "2 weeks", date_labels = "%d %b",
                 expand = expansion(c(0, 0.08))) +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_blank())
  
  if (log_y) {
    p <- p +
      #scale_y_log10(breaks = c(1, 10, 100, 1000), limits = c(1, NA))
      scale_y_continuous(trans = "log2", breaks = 2^(0:20), limits = c(1, NA))
  }
  return(p)
}

plot_novos_por_grupo <- function(dados, novos_metrica, media_metrica,
                                 grupo_facet, grupo_cor, log_y = FALSE) {
  novos <- enquo(novos_metrica)
  media <- enquo(media_metrica)
  g_facet <- enquo(grupo_facet)
  g_cor <- enquo(grupo_cor)
  
  p <- ggplot(dados, aes(diasXobitos, !!novos, col = !!g_cor)) +
    geom_line(aes(y = !!media), size = 1) +
    #geom_smooth(se = FALSE) +
    geom_point(alpha = 0.3) +
    geom_text(aes(y = !!media, label = round(!!media)),
              data = filter(dados, data == max(data)),
              nudge_x = 1, size = 3, hjust = "left", fontface = "bold") +
    facet_wrap(vars(!!g_facet), scales = "free_x", ncol = 4) +
    scale_x_continuous("Dias desde o 5o óbito", expand = expansion(c(0, 0.25))) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = c(0.86, 0.07),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.line.x = element_line(color = "grey",
                                     arrow = arrow(length = unit(4, "pt"),
                                                   type = "open")),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank()
    )
  
  if (log_y) {
    p <- p + 
      #scale_y_log10(expand = c(0.05, 0.05), breaks = c(1, 10, 100, 1000))
                  #limits = c(NA, max(dados$obitosNovosMedia)+10),
      scale_y_continuous(trans = "log2", breaks = 4^(0:20), limits = c(1, NA))
                         #expand = c(0.05, 0.05))
  } else {
    p <- p + 
      scale_y_continuous(limits = c(0, NA))
  }
  return(p)
}

plot_fracao_novos_por_dia <- function(dados, fracao_metrica, media_metrica,
                                      log_y = FALSE) {
  fracao <- enquo(fracao_metrica)
  media <- enquo(media_metrica)
  p <- ggplot(dados, aes(data, !!fracao)) +
    geom_point(alpha = 0.8, col = "#d9534f") +
    geom_line(alpha = 0.8, col = "#d9534f", lty = 2) +
    geom_line(aes(y = !!media), lty = 1, size = 0.8) +
    geom_text(aes(y = !!media,
                  label = percent(!!media, accuracy = 1)),
              data = filter(dados, data == max(data)),
              nudge_x = 0.5, size = 4, hjust = "left", fontface = "bold") +
    theme(axis.title.x = element_blank())
  
  if (log_y) {
    p <- p +
      scale_y_log10("Diferença de óbitos (%)", labels = percent_format(1))
  } else {
    p <- p +
      scale_y_continuous("Diferença de óbitos (%)", labels = percent_format(1))
  }
  return(p)
}

plot_acumulados_por_dia <- function(dados, acumulados_metrica,
                                    fracao_metrica, metade_metrica,
                                    dobrou_metrica, log_y = FALSE) {
  acumulados <- enquo(acumulados_metrica)
  fracao <- enquo(fracao_metrica)
  metade <- enquo(metade_metrica)
  dobrou <- enquo(dobrou_metrica)

  p <- ggplot(dados, aes(data, !!acumulados)) +
    geom_area(data = filter(dados, data >= !!metade), fill = "#f4cfce",
              alpha = 0.4) +
    geom_line(size = 0.8) +
    geom_text(
      aes(label = paste0(!!acumulados, "\n", "(+",
                         percent(!!fracao, accuracy = 1), ")")),
      data = filter(dados, data == max(data)), vjust = 0.7,
      nudge_x = 0.5, size = 4, hjust = "left", fontface = "bold") +
    scale_x_date("Dia", date_breaks = "1 week", date_labels = "%d %b",
                 expand = expansion(c(0, 0.1))) +
    theme(
      legend.position = "none",
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.title = element_blank(),
      axis.line.x = element_line(color = "grey",
                                 arrow = arrow(length = unit(4, "pt"),
                                               type = "open")))
  
  if (log_y) {
    p <- p +
      geom_text(
        aes(y = 2^(log2(!!acumulados)/2),
            label = paste("dobrou em\n", !!dobrou, "dias")),
        data = filter(dados, data == !!metade + floor(!!dobrou/2)),
        size = 4, fontface = "bold", hjust = "center", col = 1, alpha = 0.8) +
      #scale_y_log10("Óbitos acumulados (log)", expand = c(0, 0),
      #              limits = c(1, max(pull(dados, !!acumulados))*1.4),
      #              breaks = c(1, 10, 100, 1000, 10000))
      scale_y_continuous("Óbitos acumulados (log)", trans = "log2",
                         breaks = 2^(0:20), expand = c(0, 0),
                         limits = c(1, max(pull(dados, !!acumulados))*1.4))
  } else {
    p <- p +
      geom_text(
        aes(y = !!acumulados / 2,
            label = paste("dobrou em\n", !!dobrou, "dias")),
        data = filter(dados, data == !!metade + floor(!!dobrou / 2)),
        size = 4, fontface = "bold", hjust = "center", col = 1, alpha = 0.8) +
      scale_y_continuous("Óbitos acumulados", expand = c(0, 0),
                         limits = c(0, max(pull(dados, !!acumulados))*1.05))
  }
  return(p)
  
}

plot_acumulados_por_grupo <- function(dados, acumulados_metrica,
                                      fracao_metrica, metade_metrica,
                                      dobrou_metrica, grupo_facet, grupo_cor,
                                      log_y = FALSE) {
  acumulados <- enquo(acumulados_metrica)
  fracao <- enquo(fracao_metrica)
  metade <- enquo(metade_metrica)
  dobrou <- enquo(dobrou_metrica)
  g_facet <- enquo(grupo_facet)
  g_cor <- enquo(grupo_cor)
  
  p <- ggplot(dados, aes(diasXobitos, !!acumulados, col = !!g_cor)) +
    geom_area(data = filter(dados, data >= !!metade), fill = "#f4cfce",
              alpha = 0.4, show.legend = FALSE) +
    geom_line(size = 0.8) +
    geom_text(
      aes(
        y = !!acumulados,
        label = paste0(!!acumulados, "\n", "(+",
                       percent(!!fracao, accuracy = 1), ")")
      ),
      data = filter(dados, data == max(data)), vjust = 0.7, show.legend = FALSE,
      nudge_x = 1, size = 3, hjust = "left", fontface = "bold") +
    facet_wrap(vars(!!g_facet), scales = "free_x", ncol = 4) +
    scale_x_continuous("Dias desde o 5o óbito", expand = c(0, 0),
                       limits = c(0, max(dados$diasXobitos)*1.3)) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = c(0.86, 0.07), panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.line.x = element_line(color = "grey",
                                     arrow = arrow(length = unit(4, "pt"),
                                                   type = "open")),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank()
    )
  
  if (log_y) {
    p <- p +
      geom_text(
        aes(y = 2^(log2(!!acumulados)/2), label = !!dobrou),
        data = filter(dados, data == !!metade + floor(!!dobrou / 2)),
        size = 3, fontface = "bold", hjust = "center", col = 1, alpha = 0.8) +
      #scale_y_log10(expand = c(0, 0),
      #              limits = c(1, max(pull(dados, !!acumulados)*4)),
      #              breaks = c(1, 10, 100, 1000, 10000))
      scale_y_continuous(trans = "log2",
                        breaks = 8^(0:20), expand = c(0, 0),
                        limits = c(1, max(pull(dados, !!acumulados))*4))
  } else {
    p <- p +
      geom_text(aes(y = !!acumulados/2, label = !!dobrou),
      data = filter(dados, data == !!metade + floor(!!dobrou / 2)),
      size = 3, fontface = "bold", hjust = "center", col = 1, alpha = 0.8) +
      scale_y_continuous("Óbitos acumulados", expand = c(0, 0),
                         limits = c(0, max(pull(dados, !!acumulados)*1.2)))
  }
  return(p)
}


read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>%
  filter(date == "2020-05-23", totalCases != totalCasesMS)
