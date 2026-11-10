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
klein   <- star$Lernerfolg[star$Kleinklasse == 1]
regulaer <- star$Lernerfolg[star$Kleinklasse == 0]

mean(klein)                       # 617.67
mean(regulaer)                    # 601.67

# Difference-in-Means: Kleinklasse minus reguläre Klasse
differenz <- mean(klein) - mean(regulaer)
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

test_welch$statistic              # t-Wert
test_welch$parameter              # Freiheitsgrade
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