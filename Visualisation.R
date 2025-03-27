# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)

# Nettoyer les données et calculer les occurrences mensuelles
resultat <- data %>%
  # Filtrer les colonnes nécessaires
  select(`Passage : Année`, `Passage : Mois`, `Résultat : Nom du taxon référent`, `Résultat : Valeur de la mesure`) %>%
  # Supprimer les lignes avec des valeurs manquantes
  filter(!is.na(`Passage : Année`) & 
           !is.na(`Passage : Mois`) & 
           !is.na(`Résultat : Nom du taxon référent`) & 
           !is.na(`Résultat : Valeur de la mesure`)) %>%
  # Regrouper par année, mois et taxon référent
  group_by(`Passage : Année`, `Passage : Mois`, `Résultat : Nom du taxon référent`) %>%
  # Calculer la somme des occurrences pour chaque groupe
  summarise(Occurrences = sum(`Résultat : Valeur de la mesure`, na.rm = TRUE)) %>%
  # Organiser les mois dans l'ordre chronologique avec des labels
  mutate(`Passage : Mois` = factor(`Passage : Mois`, levels = 1:12,
                                   labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                              "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))) %>%
  # Désactiver le regroupement pour une meilleure lisibilité des résultats
  ungroup()

# Exclure les années où il y a des NA pour au moins un mois
resultat <- resultat %>%
  group_by(`Passage : Année`) %>%
  filter(all(!is.na(Occurrences))) %>%
  ungroup()

# Afficher les résultats sous forme de tableau
print(resultat)

# Identifier les 3 algues les plus dominantes pour chaque année
dominant_algues <- resultat %>%
  # Regrouper par année et taxon référent
  group_by(`Passage : Année`, `Résultat : Nom du taxon référent`) %>%
  # Calculer la somme des occurrences sur l'année pour chaque algue
  summarise(Total_Occurrences = sum(Occurrences, na.rm = TRUE)) %>%
  # Trier par année et par nombre d'occurrences décroissant
  arrange(`Passage : Année`, desc(Total_Occurrences)) %>%
  # Identifier les 3 premières algues pour chaque année
  group_by(`Passage : Année`) %>%
  slice_max(Total_Occurrences, n = 3) %>%
  # Désactiver le regroupement pour une meilleure lisibilité
  ungroup()

# Afficher les résultats
print(dominant_algues)

# Extraire la liste unique des noms des algues dominantes
liste_algues_uniques <- dominant_algues %>%
  distinct(`Résultat : Nom du taxon référent`)

# Afficher la liste des algues uniques
print(liste_algues_uniques)

# Calculer les occurrences totales pour chaque algue dans l'ensemble
top_algues_global <- resultat %>%
  group_by(`Résultat : Nom du taxon référent`) %>%
  summarise(Total_Occurrences = sum(Occurrences, na.rm = TRUE)) %>%
  arrange(desc(Total_Occurrences)) %>%
  slice_max(Total_Occurrences, n = 3) %>%
  ungroup()

# Afficher les 3 algues les plus dominantes
print(top_algues_global)

# Liste des algues les plus dominantes
algues_top3 <- top_algues_global$`Résultat : Nom du taxon référent`

# Boucle pour créer un graphique par algue
for (algue in algues_top3) {
  # Filtrer les données pour l'algue actuelle
  data_algue <- resultat %>%
    filter(`Résultat : Nom du taxon référent` == algue)
  
  # Créer le graphique pour l'algue
  plot <- ggplot(data_algue, aes(x = `Passage : Mois`, y = Occurrences, 
                                 color = factor(`Passage : Année`), 
                                 group = `Passage : Année`)) +
    geom_line(size = 1) +  # Ajouter une ligne
    labs(
      title = paste("Évolution des occurrences pour l'algue :", algue),
      x = "Mois",
      y = "Occurrences",
      color = "Année"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  # Afficher le graphique
  print(plot)
}