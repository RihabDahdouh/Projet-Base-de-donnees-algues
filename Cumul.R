# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)

# Sélection des 17 algues les plus dominantes
top_17_algues <- resultat %>%
  group_by(`Résultat : Nom du taxon référent`) %>%
  summarise(Total_Occurrences = sum(Occurrences, na.rm = TRUE)) %>%
  arrange(desc(Total_Occurrences)) %>%
  slice_head(n = 17)  # Prendre les 17 premières algues

# Filtrer les données pour ne garder que ces algues dominantes
data_top17 <- resultat %>%
  filter(`Résultat : Nom du taxon référent` %in% top_17_algues$`Résultat : Nom du taxon référent`) %>%
  group_by(`Passage : Année`, `Résultat : Nom du taxon référent`) %>%
  summarise(Total_Occurrences = sum(Occurrences, na.rm = TRUE)) %>%
  ungroup()

# Calculer le total des occurrences par année pour obtenir les pourcentages
data_top17 <- data_top17 %>%
  group_by(`Passage : Année`) %>%
  mutate(Percentage = (Total_Occurrences / sum(Total_Occurrences)) * 100) %>%  # Conversion en pourcentage
  ungroup()

# Création du graphique en barres empilées avec pourcentages
ggplot(data_top17, aes(x = factor(`Passage : Année`), y = Percentage, fill = `Résultat : Nom du taxon référent`)) +
  geom_bar(stat = "identity", position = "stack") +  # Barres empilées en %  
  labs(title = "Répartition en pourcentage des 17 algues les plus dominantes par année",
       x = "Année",
       y = "Pourcentage (%)",
       fill = "Algue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des années pour lisibilité
