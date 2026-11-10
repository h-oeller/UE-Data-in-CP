# Title: Sitzung 10 - Kausalinferenz
# Author: Stefan Höller, stefan.hoeller@uni-mannheim.de
# Date: 10.11.2026

# 1. Daten eingeben --------------------------------------------------------

star <- data.frame(
  i = 1:6,
  Kleinklasse = c(1, 0, 0, 1, 1, 0),
  Lernerfolg = c(578, 612, 583, 661, 614, 610),
  Y1 = c(578, NA, NA, 661, 614, NA),
  Y0 = c(NA, 612, 583, NA, NA, 610)
)

# Kleinklasse: 1 = Kleinklasse, 0 = reguläre Klasse
# Y1: potenzieller Lernerfolg bei Zuweisung zur Kleinklasse
# Y0: potenzieller Lernerfolg bei Zuweisung zur regulären Klasse

star


# 2. Mittelwerte vergleichen ----------------------------------------------

# Beobachtete Ergebnisse nach Gruppe
klein <- star$Y1
regulaer <- star$Y0

mean(klein, na.rm = TRUE)         # 617.67
mean(regulaer, na.rm = TRUE)      # 601.67

# Difference-in-Means: Kleinklasse minus reguläre Klasse
differenz <- mean(klein, na.rm = TRUE) - mean(regulaer, na.rm = TRUE)  
differenz                         # 16 Punkte

# Übersicht nach Gruppe
aggregate(Lernerfolg ~ Kleinklasse, data = star, FUN = mean)


# 3. t-Test für unabhängige Gruppen ----------------------------------------

# H0: Die Mittelwerte beider Gruppen sind gleich.
# H1: Die Mittelwerte unterscheiden sich.
#
# Reihenfolge x, y: Kleinklasse minus reguläre Klasse.
# Welch-Test: keine Annahme gleicher Gruppenvarianzen.

test_welch <- t.test(
  x = klein,
  y = regulaer,
  alternative = "two.sided",
  conf.level = 0.95
)

test_welch

test_welch$statistic              # t-Wert = 0.621 Standard Errors
test_welch$parameter              # Degrees of Freedom
test_welch$p.value                # p-Wert
test_welch$conf.int               # 95%-KI der Mittelwertdifferenz


# 4. Mittelwertvergleich als lineare Regression ----------------------------

# Lernerfolg_i = alpha + beta * Kleinklasse_i + Fehler_i

modell <- lm(Lernerfolg ~ Kleinklasse, data = star)

summary(modell)
confint(modell)

# Intercept: Mittelwert der regulären Klasse
coef(modell)["(Intercept)"]       # 601.67

# Treatmentkoeffizient: Mittelwertdifferenz
coef(modell)["Kleinklasse"]       # 16

# Vorhergesagte Gruppenmittelwerte
predict(
  modell,
  newdata = data.frame(Kleinklasse = c(0, 1))
)                                # 601.67 und 617.67


# 5. Gleichheit der Punktschätzer überprüfen -------------------------------

c(
  Mittelwertdifferenz = differenz,
  Regressionskoeffizient = unname(coef(modell)["Kleinklasse"])
)


# 6. Welcher t-Test entspricht der gewöhnlichen OLS-Inferenz? ---------------

# Der klassische t-Test unter Annahme gleicher Gruppenvarianzen
# entspricht hier dem Test des Treatmentkoeffizienten in summary(modell).

test_student <- t.test(
  x = klein,
  y = regulaer,
  var.equal = TRUE
)

rbind(
  Student_t_Test = c(
    t = unname(test_student$statistic),
    df = unname(test_student$parameter),
    p = test_student$p.value
  ),
  OLS_Regression = c(
    t = coef(summary(modell))["Kleinklasse", "t value"],
    df = df.residual(modell),
    p = coef(summary(modell))["Kleinklasse", "Pr(>|t|)"]
  )
)

# 7. Konfidenzintervalle und Ergebnistabellen -------------------------------

library(ggplot2)

# A. Gruppenmittelwerte und ihre modellbasierten 95%-Konfidenzintervalle
# interval = "confidence": Unsicherheit des geschätzten Mittelwerts,
# nicht die Streuung einzelner Schülerinnen und Schüler.

vorhersagen <- data.frame(Kleinklasse = c(0, 1))

ki_mittelwerte <- predict(
  modell,
  newdata = vorhersagen,
  interval = "confidence",
  level = 0.95
)

ki_mittelwerte

vorhersagen$Mittelwert <- ki_mittelwerte[, "fit"]
vorhersagen$KI_unten   <- ki_mittelwerte[, "lwr"]
vorhersagen$KI_oben    <- ki_mittelwerte[, "upr"]

tabelle_gruppen <- data.frame(
  Gruppe = c("Reguläre Klasse", "Kleinklasse"),
  n = c(sum(!is.na(regulaer)), sum(!is.na(klein))),
  Mittelwert = vorhersagen$Mittelwert,
  KI_unten = vorhersagen$KI_unten,
  KI_oben = vorhersagen$KI_oben
)

tabelle_gruppen

# B. Geschätzte Differenz und ihre Unsicherheit
# Alle Verfahren betrachten Kleinklasse minus reguläre Klasse.

ols <- coef(summary(modell))["Kleinklasse", ]
ols_ki <- confint(modell, "Kleinklasse", level = 0.95)

ols
ols_ki # Konfidenzintervall für die ermittelte Mittelwertdifferenz von 16

tabelle_effekte <- data.frame(
  Methode = c("Welch-t-Test", "Student-t-Test", "OLS"),
  Differenz = c(
    differenz,
    differenz,
    unname(coef(modell)["Kleinklasse"])
  ),
  SE = c(
    unname(test_welch$stderr),
    unname(test_student$stderr),
    unname(ols["Std. Error"])
  ),
  df = c(
    unname(test_welch$parameter),
    unname(test_student$parameter),
    df.residual(modell)
  ),
  t = c(
    unname(test_welch$statistic),
    unname(test_student$statistic),
    unname(ols["t value"])
  ),
  p = c(
    test_welch$p.value,
    test_student$p.value,
    unname(ols["Pr(>|t|)"])
  ),
  KI_unten = c(
    test_welch$conf.int[1],
    test_student$conf.int[1],
    ols_ki[1, 1]
  ),
  KI_oben = c(
    test_welch$conf.int[2],
    test_student$conf.int[2],
    ols_ki[1, 2]
  )
)

tabelle_effekte$Null_im_KI <- with(
  tabelle_effekte,
  KI_unten <= 0 & KI_oben >= 0
)

# Tabellen für die Ausgabe runden.
# Die ursprünglichen Werte bleiben für die Plots erhalten.

runde_tabelle <- function(daten, stellen = 2) {
  numerisch <- vapply(daten, is.numeric, logical(1))
  daten[numerisch] <- lapply(daten[numerisch], round, digits = stellen)
  daten
}

runde_tabelle(tabelle_gruppen)
runde_tabelle(tabelle_effekte, stellen = 3)


# 8. Regressionsplot mit 95%-Konfidenzintervallen ---------------------------

plot_regression <- ggplot() +
  geom_point(
    data = star,
    aes(x = Kleinklasse, y = Lernerfolg),
    size = 3,
    color = "grey40"
  ) +
  geom_errorbar(
    data = vorhersagen,
    aes(x = Kleinklasse, ymin = KI_unten, ymax = KI_oben),
    width = 0.08,
    linewidth = 0.8,
    color = "#ED702D"
  ) +
  geom_line(
    data = vorhersagen,
    aes(x = Kleinklasse, y = Mittelwert),
    linewidth = 1,
    color = "#ED702D"
  ) +
  geom_point(
    data = vorhersagen,
    aes(x = Kleinklasse, y = Mittelwert),
    shape = 18,
    size = 5,
    color = "#ED702D"
  ) +
  geom_text(
    data = vorhersagen,
    aes(
      x = Kleinklasse,
      y = Mittelwert,
      label = sprintf("%.2f", Mittelwert)
    ),
    nudge_x = 0.06,
    hjust = 0,
    vjust = -0.6,
    color = "#ED702D",
    size = 4.5
  ) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Reguläre Klasse (0)", "Kleinklasse (1)"),
    expand = expansion(mult = 0.25)
  ) +
  labs(
    title = "Mittelwertvergleich als Regression",
    subtitle = sprintf(
      "Geschätzter Lernerfolg = %.2f + %.2f × Kleinklasse",
      coef(modell)[1], coef(modell)[2]
    ),
    x = "Klassentyp",
    y = "Lernerfolg (Punkte)",
    caption = paste(
      "Grau: beobachtete Werte. Blau: Gruppenmittelwerte mit 95%-KI.",
      "Intervalle aus OLS mit gemeinsamer Varianzannahme.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

plot_regression


# 9. Konfidenzintervalle der Mittelwertdifferenz ----------------------------

# Reihenfolge von oben nach unten im Plot
tabelle_effekte$Methode <- factor(
  tabelle_effekte$Methode,
  levels = c("OLS", "Student-t-Test", "Welch-t-Test")
)

plot_differenz <- ggplot(
  tabelle_effekte,
  aes(y = Methode)
) +
  # Nullhypothese: kein Mittelwertunterschied
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  # Horizontale Konfidenzintervalle
  geom_segment(
    aes(
      x = KI_unten,
      xend = KI_oben,
      yend = Methode
    ),
    linewidth = 1,
    color = "#ED702D"
  ) +
  geom_point(
    aes(x = Differenz),
    size = 3.5,
    color = "#ED702D"
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.1)
  ) +
  labs(
    title = "Mittelwertdifferenz und statistische Unsicherheit",
    subtitle = "Kleinklasse minus reguläre Klasse: 16 Punkte",
    x = "Geschätzte Mittelwertdifferenz mit 95%-Konfidenzintervall",
    y = NULL,
    caption = paste(
      "Die gestrichelte Linie markiert eine Differenz von null.",
      "Student-t-Test und gewöhnliche OLS liefern dasselbe Intervall.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

plot_differenz
