library(tidyverse)
library(ggplot2)

wallonie <- read.csv(file = "Jeux_de_données/signaletique-fase_FWB.csv",
                     header = TRUE,
                     sep = ";")
flandre <- read.csv(file = "Jeux_de_données/Onderwijs_CSV-Flamand-bx/POI_Onderwijs.csv",
                    header = TRUE,
                    sep = ";",
                    quote = "")
          
# Use the group_by() and summarise() functions to calculate percentages

# Stats sur les bassins
result_bassins <- wallonie %>%
  group_by(Bassin) %>%
  summarise(nombre = sum(wallonie$Bassin == Bassin),
            Pourcentage = n() / nrow(wallonie) * 100)

print(result_bassins)

# Stats sur les niveaux
result_niveau <- wallonie %>%
  group_by(Niveau) %>%
  summarise(nombre = sum(wallonie$Niveau == Niveau),
            Pourcentage = n() / nrow(wallonie) * 100)

print(result_niveau)


# Stat Niveaux par Bassins
result_niv_par_bassins <- wallonie %>%
  group_by(Bassin) %>%
  summarise(fondamental = sum(wallonie$Bassin == Bassin & wallonie$Niveau == "Fondamental"),
            secondaire = sum(wallonie$Bassin == Bassin & wallonie$Niveau == "Secondaire"),
            superieur = sum(wallonie$Bassin == Bassin & wallonie$Niveau == "Supérieur"),
            total = sum(wallonie$Bassin == Bassin),
            pourcentage_total = n() / nrow(wallonie) * 100)

print(result_niv_par_bassins)

# Stat Niveaux par Commune de l'implantation
result_niv_par_commune <- wallonie %>%
  group_by(Commune.de.l.implantation) %>%
  summarise(fondamental = sum(wallonie$Commune.de.l.implantation == Commune.de.l.implantation & wallonie$Niveau == "Fondamental"),
            secondaire = sum(wallonie$Commune.de.l.implantation == Commune.de.l.implantation & wallonie$Niveau == "Secondaire"),
            superieur = sum(wallonie$Commune.de.l.implantation == Commune.de.l.implantation & wallonie$Niveau == "Supérieur"),
            total = sum(wallonie$Commune.de.l.implantation == Commune.de.l.implantation),
            pourcentage_total = n() / nrow(wallonie) * 100)

print(result_niv_par_commune)

