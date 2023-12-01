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

result_bassins <- wallonie %>%
  group_by(Bassin) %>%
  summarise(nombre = n(),
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

library(dplyr)

# Count of schools in each "Bassin"
bassin_counts <- wallonie %>%
  group_by(Bassin) %>%
  summarise(Count = n())

# Count of schools by different values of "Niveau"
niveau_counts <- wallonie %>%
  group_by(Niveau) %>%
  summarise(Count = n())

# Print the results
print(bassin_counts)
print(niveau_counts)

# Count of schools in each "Bassin" and each value of "Niveau"
count_by_bassin_niveau <- wallonie %>%
  group_by(Bassin, Niveau) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Niveau, values_from = Count, values_fill = 0) %>%
  ungroup() %>%
  mutate(Total = rowSums(select(., starts_with("Fondamental"), starts_with("Secondaire"), starts_with("Supérieur"))),
         Percentage_Fondamental = Fondamental / Total * 100,
         Percentage_Secondaire = Secondaire / Total * 100,
         Percentage_Supérieur = Supérieur / Total * 100,
         Percentage_Total = Total / nrow(wallonie) * 100)%>%
  add_row(Bassin = "Total_column",
        Fondamental = sum(.$Fondamental),
        Secondaire = sum(.$Secondaire),
        Supérieur = sum(.$Supérieur),
        Total = sum(.$Total),
        Percentage_Fondamental = sum(.$Fondamental) / sum(.$Total) * 100,
        Percentage_Secondaire = sum(.$Secondaire) / sum(.$Total) * 100,
        Percentage_Supérieur = sum(.$Supérieur) / sum(.$Total) * 100,
        Percentage_Total = sum(.$Percentage_Total))


# Print the results
print(count_by_bassin_niveau)
