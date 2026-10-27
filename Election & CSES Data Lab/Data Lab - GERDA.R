# Title: Data Lab - GERDA.R
# Author: Stefan Höller, stefan.hoeller@uni-mannheim.de
# Date: 26.10.2026

##### 1. GERDA kennenlernen #####
# •	GERDA Usage Notes: Auswahl und Interpretation der Wahldaten
#   (https://www.german-elections.com/usage-notes/)
# •	GERDA R-Paket: Daten laden, Kovariaten ergänzen und Variablen nachschlagen
#   (https://www.german-elections.com/r-package/)
# •	Election Explorer: Datenangebot entdecken
#.  (https://www.german-elections.com/dashboard/)


##### 2. Ein erster Plot #####

install.packages("gerda")
install.packages("tidyverse")
library(gerda)
library(tidyverse)

# Datenangebot ansehen
gerda_data_list()

# 1. Kreisdaten der Bundestagswahl 2021 laden
election <- load_gerda_web("federal_cty_unharm",
                           on_error = "stop") |>
  filter(election_year == 2021)

parteien <- c("cdu_csu", "spd", "gruene", "fdp", "afd", "linke_pds")

# 2. Kreisanteile in Stimmenzahlen umrechnen und bundesweit addieren
ergebnis <- election |>
  summarise(
    across(
      all_of(parteien),
      ~ sum(.x * number_voters) / sum(valid_votes)
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "partei",
    values_to = "anteil"
  ) |>
  mutate(
    partei = recode(
      partei,
      cdu_csu = "CDU/CSU",
      spd = "SPD",
      gruene = "Grüne",
      fdp = "FDP",
      afd = "AfD",
      linke_pds = "Die Linke"
    )
  )

# Alle weiteren Parteien als Rest zusammenfassen
ergebnis <- ergebnis |>
  add_row(partei = "Sonstige", anteil = 1 - sum(ergebnis$anteil)) |>
  mutate(
    partei = factor(
      partei,
      levels = c("CDU/CSU", "SPD", "Grüne", "FDP",
                 "AfD", "Die Linke", "Sonstige")
    )
  )

# 3. Parteifarben festlegen
parteifarben <- c(
  "CDU/CSU"  = "#000000",
  "SPD"      = "#E3000F",
  "Grüne"    = "#1AA037",
  "FDP"      = "#FFED00",
  "AfD"      = "#009EE0",
  "Die Linke" = "#BE3075",
  "Sonstige" = "#999999"
)

# 4. Balkendiagramm zeichnen
ggplot(ergebnis, aes(x = partei, y = anteil, fill = partei)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = scales::percent(
      anteil, accuracy = 0.1, decimal.mark = ","
    )),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(values = parteifarben, guide = "none") +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Bundestagswahl 2021",
    subtitle = "Bundesweite Zweitstimmenanteile",
    x = NULL,
    y = "Anteil an den gültigen Zweitstimmen",
    caption = "Quelle: GERDA, federal_cty_unharm; ursprüngliches Ergebnis 2021"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())


##### 3. Eine Forschungsfrage und passende Variablen finden #####
# Eine Bundestagswahl auf Kreisebene auswählen
election <- load_gerda_web(
  "federal_cty_unharm",
  on_error = "stop"
) |>
  filter(election_year == 2021)

# Öffnet das Covariates Codebook
view(gerda_covariates_codebook())

# Regionale Merkmale ergänzen
election_cov <- add_gerda_covariates(election)

# Verknüpfung und Variablen prüfen
gerda_join_diagnostics(election_cov)

# Auswahl der Variablen
data_selection <- election_cov |> 
  select(county_code, election_year, state, state_name,
         afd,               # your dependent variable
         share_foreign)     # your independent variable(s)

glimpse(data_selection)


##### 4. Einen Zusammenhang sichtbar machen #####
