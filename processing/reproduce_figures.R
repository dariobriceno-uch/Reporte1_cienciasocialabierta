# Reproducción de Figuras 1 y 2 del artículo:
# Lintner, T., Diviak, T., & Nekardova, B. (2024).
# Interaction dynamics in classroom group work.
# Social Networks, 79, 14-24.

library(ggplot2)
library(readxl)
library(dplyr)

# Rutas
DATA_DIR    <- "../input/data/original/"
OUTPUT_DIR  <- "../output/graphs/"
INTERACTIONAL_FILE <- paste0(DATA_DIR, "interactional data.xlsx")

# Crear directorio de output si no existe
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Colores
COLOR_ON  <- "#4682B4"   # Azul para on-task
COLOR_OFF <- "#CD6655"   # Rojo para off-task


# --- Figura 1: Eventos relacionales por grupo ---

make_figure1 <- function() {

  sheet_names <- excel_sheets(INTERACTIONAL_FILE)

  # Contar eventos por grupo
  group_counts <- data.frame(
    group   = character(),
    on_task = integer(),
    off_task = integer(),
    total   = integer(),
    stringsAsFactors = FALSE
  )

  for (sheet in sheet_names) {
    df <- read_excel(INTERACTIONAL_FILE, sheet = sheet)
    on_task  <- sum(df$type == "on",  na.rm = TRUE)
    off_task <- sum(df$type == "off", na.rm = TRUE)
    group_counts <- rbind(group_counts, data.frame(
      group   = sheet,
      on_task = on_task,
      off_task = off_task,
      total   = on_task + off_task
    ))
  }

  # Ordenar ascendente por total
  group_counts <- group_counts[order(group_counts$total), ]
  group_counts$group <- factor(group_counts$group,
                               levels = group_counts$group)

  # Formato largo para barras apiladas
  group_counts_long <- group_counts %>%
    tidyr::pivot_longer(
      cols = c(on_task, off_task),
      names_to = "event_type",
      values_to = "count"
    )

  group_counts_long$event_type <- factor(
    group_counts_long$event_type,
    levels = c("on_task", "off_task"),
    labels = c("on-task events", "off-task events")
  )

  # Gráfico
  p1 <- ggplot(group_counts_long, 
               aes(x = group, y = count, fill = event_type)) +
    geom_col(width = 0.85) +
    coord_flip() +
    scale_fill_manual(values = c("on-task events" = COLOR_ON,
                                  "off-task events" = COLOR_OFF)) +
    scale_y_continuous(
      breaks = seq(0, 250, 50),
      expand = expansion(mult = 0, add = c(0, 5))
    ) +
    scale_x_discrete(limits = levels(group_counts$group)) +
    labs(
      x = "individual groups",
      y = "number of relational events in the groups",
      fill = NULL
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.y       = element_blank(),
      axis.ticks.y      = element_blank(),
      axis.line.y       = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      legend.position   = "center right",
      legend.key        = element_rect(colour = NA),
      legend.key.size   = unit(0.5, "cm"),
      legend.background = element_blank(),
      panel.border      = element_rect(colour = "black", fill = NA, 
                                       linewidth = 0.5),
      plot.margin       = margin(5, 10, 5, 5)
    )

  ggsave(paste0(OUTPUT_DIR, "figure1_relational_events.png"),
         p1, width = 7, height = 10, dpi = 300)
  ggsave(paste0(OUTPUT_DIR, "figure1_relational_events.pdf"),
         p1, width = 7, height = 10)

  # Verificación
  cat("Figura 1 guardada\n")
  cat(sprintf("  Grupos: %d\n", nrow(group_counts)))
  cat(sprintf("  Eventos on-task: %d\n", sum(group_counts$on_task)))
  cat(sprintf("  Eventos off-task: %d\n", sum(group_counts$off_task)))
  cat(sprintf("  Media total: %.1f (artículo: 86.0)\n", 
              mean(group_counts$total)))
  cat(sprintf("  DE total: %.1f (artículo: 61.1)\n", 
              sd(group_counts$total)))
  cat(sprintf("  Media on-task: %.1f (artículo: 76.2)\n", 
              mean(group_counts$on_task)))
  cat(sprintf("  DE on-task: %.1f (artículo: 59.1)\n", 
              sd(group_counts$on_task)))
  cat(sprintf("  Media off-task: %.1f (artículo: 9.8)\n", 
              mean(group_counts$off_task)))
  cat(sprintf("  DE off-task: %.1f (artículo: 9.7)\n", 
              sd(group_counts$off_task)))

  invisible(p1)
}


# --- Figura 2: Tiempos de secuencia recíproca ---

# Para cada evento A->B en t1, busca el primer evento B->A en t2 > t1
# del mismo tipo y calcula t2 - t1.
compute_reciprocated_times <- function(df, event_type) {

  df_type <- df[df$type == event_type, ]

  if (nrow(df_type) == 0) return(numeric(0))

  # Estandarizar IDs a 4 dígitos
  df_type$sender   <- sprintf("%04d", as.integer(df_type$sender))
  df_type$receiver <- sprintf("%04d", as.integer(df_type$receiver))

  # Quitar eventos dirigidos a "all"
  df_type <- df_type[!df_type$receiver %in% c("0all", "all"), ]
  df_type$time <- as.numeric(df_type$time)
  df_type <- df_type[!is.na(df_type$time), ]

  # Para cada par (sender, receiver), guardar tiempos ordenados
  pair_times <- split(df_type$time,
                      paste(df_type$sender, df_type$receiver, sep = "->"))
  pair_times <- lapply(pair_times, sort)

  reciprocated_times <- c()

  for (pair_name in names(pair_times)) {
    parts <- strsplit(pair_name, "->")[[1]]
    sender   <- parts[1]
    receiver <- parts[2]

    # Buscar par inverso
    reverse_name <- paste(receiver, sender, sep = "->")
    if (!(reverse_name %in% names(pair_times))) next

    reverse_times <- pair_times[[reverse_name]]

    for (t1 in pair_times[[pair_name]]) {
      idx <- which(reverse_times > t1)
      if (length(idx) > 0) {
        t2 <- reverse_times[idx[1]]
        reciprocated_times <- c(reciprocated_times, t2 - t1)
      }
    }
  }

  return(reciprocated_times)
}


make_figure2 <- function() {

  sheet_names <- excel_sheets(INTERACTIONAL_FILE)

  # Calcular tiempos recíprocos en los 62 grupos
  all_on_times  <- c()
  all_off_times <- c()

  for (sheet in sheet_names) {
    df <- read_excel(INTERACTIONAL_FILE, sheet = sheet)
    df$sender   <- sprintf("%04d", as.integer(df$sender))
    df$receiver <- sprintf("%04d", as.integer(df$receiver))

    all_on_times  <- c(all_on_times,  compute_reciprocated_times(df, "on"))
    all_off_times <- c(all_off_times, compute_reciprocated_times(df, "off"))
  }

  cat(sprintf("Tiempos recíprocos on-task: %d\n", length(all_on_times)))
  cat(sprintf("Tiempos recíprocos off-task: %d\n", length(all_off_times)))

  # Histograma con bins de 1 segundo
  max_time <- max(c(all_on_times, all_off_times), na.rm = TRUE) + 1
  bins <- seq(0, max(max_time, 102), 1)

  on_hist  <- hist(all_on_times, breaks = bins, plot = FALSE, 
                   warn.unused = FALSE)
  off_hist <- hist(all_off_times, breaks = bins, plot = FALSE, 
                   warn.unused = FALSE)

  on_df <- data.frame(time = on_hist$mids, freq = on_hist$counts, 
                      type = "on-task events")
  off_df <- data.frame(time = off_hist$mids, freq = off_hist$counts, 
                       type = "off-task events")

  plot_df <- rbind(on_df, off_df)
  plot_df$type <- factor(plot_df$type, 
                         levels = c("on-task events", "off-task events"))

  # Gráfico
  p2 <- ggplot(plot_df, aes(x = time, y = freq, color = type)) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("on-task events" = COLOR_ON,
                                   "off-task events" = COLOR_OFF)) +
    scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 400, 100), limits = c(0, 400)) +
    labs(
      x = "sequence time in seconds",
      y = "frequencies",
      color = NULL
    ) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = "upper right",
      legend.key = element_rect(colour = NA, fill = NA),
      legend.key.size = unit(0.4, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = 2)))

  ggsave(paste0(OUTPUT_DIR, "figure2_reciprocated_times.png"),
         p2, width = 8, height = 5, dpi = 300)
  ggsave(paste0(OUTPUT_DIR, "figure2_reciprocated_times.pdf"),
         p2, width = 8, height = 5)

  cat("Figura 2 guardada\n")
  cat(sprintf("  Pico on-task: %d en t=%.1fs\n",
              max(on_hist$counts), on_hist$mids[which.max(on_hist$counts)]))
  cat(sprintf("  Pico off-task: %d en t=%.1fs\n",
              max(off_hist$counts), off_hist$mids[which.max(off_hist$counts)]))

  invisible(p2)
}


# Ejecución
cat("Reproduciendo Figura 1...\n")
make_figure1()

cat("\nReproduciendo Figura 2...\n")
make_figure2()

cat(sprintf("\nFiguras guardadas en: %s\n", OUTPUT_DIR))
