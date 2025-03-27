# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)

# Nettoyer les données et calculer les occurrences mensuelles
resultat <- data %>%
  select(`Passage : Année`, `Passage : Mois`, `Résultat : Nom du taxon référent`, `Résultat : Valeur de la mesure`) %>%
  filter(!is.na(`Passage : Année`) & 
           !is.na(`Passage : Mois`) & 
           !is.na(`Résultat : Nom du taxon référent`) & 
           !is.na(`Résultat : Valeur de la mesure`)) %>%
  group_by(`Passage : Année`, `Passage : Mois`, `Résultat : Nom du taxon référent`) %>%
  summarise(Occurrences = sum(`Résultat : Valeur de la mesure`, na.rm = TRUE)) %>%
  mutate(`Passage : Mois` = factor(`Passage : Mois`, levels = 1:12,
                                   labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                              "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))) %>%
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
  group_by(`Passage : Année`, `Résultat : Nom du taxon référent`) %>%
  summarise(Total_Occurrences = sum(Occurrences, na.rm = TRUE)) %>%
  arrange(`Passage : Année`, desc(Total_Occurrences)) %>%
  group_by(`Passage : Année`) %>%
  slice_max(Total_Occurrences, n = 3) %>%
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
  
  # Fermer tout dispositif graphique ouvert
  while (!is.null(dev.list())) dev.off()
  
  # Réinitialiser les paramètres graphiques
  par(mfrow = c(1, 1))  # Un seul graphique par fenêtre
  par(mar = c(5, 5, 4, 2) + 0.1)  # Ajuster les marges
  
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

# Liste des 3 algues les plus dominantes
algues_top3 <- top_algues_global$`Résultat : Nom du taxon référent`
par(mfrow = c(1,1))  # Assurer une seule fenêtre pour le graphique
par(mar = c(5, 5, 4, 2) + 0.1)  # Définir des marges raisonnables

# Boucle pour chaque algue dominante
for (algue in algues_top3) {
  # Enregistrer chaque graphique en PNG
  png(filename = paste0(algue, "_elbow_plot.png"), width = 800, height = 600)
  
  # Filtrer les données pour l'algue actuelle
  data_algue <- resultat %>%
    filter(`Résultat : Nom du taxon référent` == algue) %>%
    group_by(`Passage : Année`) %>%
    summarise(Total_Occurrences = sum(Occurrences, na.rm = TRUE)) %>%
    ungroup()
  
  # Créer un vecteur du nombre d'années où l'algue est présente
  nb_annees <- nrow(data_algue)
  k_max <- max(1, nb_annees - 1)  # Nombre maximal de clusters (éviter 0)
  
  # Vérifier si l'algue est présente sur plusieurs années
  if (k_max > 1) {
    # Créer un dataframe pour K-means
    data_kmeans <- data.frame(data_algue$Total_Occurrences)
    
    # Calculer la somme des carrés intra-cluster (WSS) pour différents k
    wss <- numeric(k_max)
    for (k in 1:k_max) {
      wss[k] <- kmeans(data_kmeans, centers = k, nstart = 10)$tot.withinss
    }
    
    # Tracer la courbe de la méthode du coude
    plot(1:k_max, wss, type = "b", pch = 19, frame = FALSE,
         xlab = "Nombre de clusters",
         ylab = "Somme des carrés intra-cluster (WSS)",
         main = paste("Méthode du coude pour l'algue:", algue))
  } else {
    print(paste("Pas assez de données pour appliquer la méthode du coude pour l'algue:", algue))
  }
  
  # Fermer le fichier PNG
  dev.off()
}

# Ajuster les marges
par(mar = c(4, 4, 2, 2))  # marges autour du graphique : bas, gauche, haut, droite

# Boucle pour chaque algue dominante
for (algue in algues_top3) {
  # Filtrer les données pour l'algue actuelle
  data_algue <- resultat %>%
    filter(`Résultat : Nom du taxon référent` == algue) %>%
    group_by(`Passage : Année`) %>%
    summarise(Total_Occurrences = sum(Occurrences, na.rm = TRUE)) %>%
    ungroup()
  
  # Créer un vecteur du nombre d'années où l'algue est présente
  nb_annees <- nrow(data_algue)
  k_max <- max(1, nb_annees - 1)  # Nombre maximal de clusters (éviter 0)
  
  # Vérifier si l'algue est présente sur plusieurs années
  if (k_max > 1) {
    # Créer un dataframe pour K-means
    data_kmeans <- data.frame(data_algue$Total_Occurrences)
    
    # Initialiser un vecteur pour stocker les scores de silhouette
    silhouette_scores <- numeric(k_max)
    
    # Calculer les scores de silhouette pour différents k
    for (k in 2:k_max) {
      kmeans_result <- kmeans(data_kmeans, centers = k, nstart = 10)
      sil_score <- silhouette(kmeans_result$cluster, dist(data_kmeans))
      silhouette_scores[k] <- mean(sil_score[, 3])  # Moyenne des scores de silhouette
    }
    
    # Tracer la courbe des scores de silhouette
    plot(2:k_max, silhouette_scores[2:k_max], type = "b", pch = 19, frame = FALSE,
         xlab = "Nombre de clusters",
         ylab = "Score de Silhouette moyen",
         main = paste("Méthode de la silhouette pour l'algue:", algue))
    
  } else {
    print(paste("Pas assez de données pour appliquer la méthode de la silhouette pour l'algue:", algue))
  }
}

algues_k <- list(
  "Phaeocystis" = 13,
  "Asterionellopsis glacialis" = 2,
  "Pseudo-nitzschia, complexe delicatissima, groupe des fines (calliantha + delicatissima + pseudodelicatissima + subcurvata)" = 2
)

analyser_algue <- function(nom_algue, k) {
  # Filtrer les données
  data_algue <- resultat %>%
    filter(`Résultat : Nom du taxon référent` == nom_algue) %>%
    select(`Passage : Année`, `Passage : Mois`, Occurrences) %>%
    pivot_wider(names_from = `Passage : Mois`, values_from = Occurrences, values_fill = list(Occurrences = 0))
  
  # Vérifier que les données ne sont pas vides
  if (nrow(data_algue) == 0) {
    message(paste("⚠️ Aucune donnée trouvée pour", nom_algue))
    return(NULL)
  }
  
  # Normaliser les données
  data_norm <- as.data.frame(scale(data_algue[,-1]))  # Exclure l'année pour la normalisation
  
  # 📌 Déterminer la courbe du coude pour visualiser le choix de k
  wss <- sapply(1:15, function(k) kmeans(data_norm, centers = k, nstart = 20)$tot.withinss)
  
  
  
  # 📌 Appliquer K-means avec le k spécifié
  set.seed(123)
  km_result <- kmeans(data_norm, centers = k, nstart = 25)
  
  print(paste("✅ Clustering avec k =", k, "pour", nom_algue))
  
  # Ajouter les clusters aux données
  data_algue$Cluster <- factor(km_result$cluster)
  
  # Restructurer les données pour ggplot
  data_long <- data_algue %>%
    gather(key = "Mois", value = "Occurrences", -`Passage : Année`, -Cluster)
  
  # Forcer l'ordre des mois
  data_long$Mois <- factor(data_long$Mois, 
                           levels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                      "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
  
  # 📌 Visualisation des profils temporels avec les clusters
  cluster_plot <- ggplot(data_long, aes(x = Mois, y = Occurrences, color = Cluster, group = `Passage : Année`)) +
    geom_line(size = 1) +
    labs(title = paste("Profils temporels de", nom_algue, "par cluster (k =", k, ")"),
         x = "Mois", y = "Occurrences", color = "Cluster") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(cluster_plot)  # 🔥 Afficher la courbe des clusters
}

# 📌 Appliquer la fonction sur chaque algue avec son k spécifique
resultats_clusters <- lapply(names(algues_k), function(a) analyser_algue(a, algues_k[[a]]))

analyser_algue <- function(nom_algue, k) {
  # Filtrer les données
  data_algue <- resultat %>%
    filter(`Résultat : Nom du taxon référent` == nom_algue) %>%
    select(`Passage : Année`, `Passage : Mois`, Occurrences) %>%
    pivot_wider(names_from = `Passage : Mois`, values_from = Occurrences, values_fill = list(Occurrences = 0))
  
  # Vérifier que les données ne sont pas vides
  if (nrow(data_algue) == 0) {
    message(paste("⚠️ Aucune donnée trouvée pour", nom_algue))
    return(NULL)
  }
  
  # Normaliser les données
  data_norm <- as.data.frame(scale(data_algue[,-1]))  # Exclure l'année pour la normalisation
  
  # Appliquer K-means avec le k spécifié
  set.seed(123)
  km_result <- kmeans(data_norm, centers = k, nstart = 25)
  
  print(paste("✅ Clustering avec k =", k, "pour", nom_algue))
  
  # Ajouter les clusters aux données
  data_algue$Cluster <- factor(km_result$cluster)
  
  # Restructurer les données pour ggplot
  data_long <- data_algue %>%
    gather(key = "Mois", value = "Occurrences", -`Passage : Année`, -Cluster)
  
  # Forcer l'ordre des mois
  data_long$Mois <- factor(data_long$Mois, 
                           levels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                      "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
  
  # Calculer les centroïdes (moyennes par mois pour chaque cluster)
  centroides <- data_long %>%
    group_by(Cluster, Mois) %>%
    summarise(Centroid = mean(Occurrences, na.rm = TRUE)) %>%
    ungroup()
  
  # Visualisation des centroïdes
  centroide_plot <- ggplot(centroides, aes(x = Mois, y = Centroid, color = Cluster, group = Cluster)) +
    geom_line(size = 1.5) +  # Tracer les centroïdes
    geom_point(size = 3) +  # Ajouter des points pour les centroïdes
    labs(title = paste("Centroïdes des clusters pour", nom_algue),
         x = "Mois", y = "Occurrences moyennes", color = "Cluster") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(centroide_plot)  # Afficher le graphique des centroïdes
}

# Appliquer la fonction sur chaque algue avec son k spécifique
resultats_clusters <- lapply(names(algues_k), function(a) analyser_algue(a, algues_k[[a]]))

visualiser_clusters_annees <- function(nom_algue, k) {
  # Filtrer les données
  data_algue <- resultat %>%
    filter(`Résultat : Nom du taxon référent` == nom_algue) %>%
    select(`Passage : Année`, `Passage : Mois`, Occurrences) %>%
    pivot_wider(names_from = `Passage : Mois`, values_from = Occurrences, values_fill = list(Occurrences = 0))
  
  # Vérifier que les données ne sont pas vides
  if (nrow(data_algue) == 0) {
    message(paste("⚠️ Aucune donnée trouvée pour", nom_algue))
    return(NULL)
  }
  
  # Normaliser les données
  data_norm <- as.data.frame(scale(data_algue[,-1]))  # Exclure l'année pour la normalisation
  
  # Appliquer K-means avec le k spécifié
  set.seed(123)
  km_result <- kmeans(data_norm, centers = k, nstart = 25)
  
  print(paste("✅ Clustering avec k =", k, "pour", nom_algue))
  
  # Ajouter les clusters aux données
  data_algue$Cluster <- factor(paste0("c", km_result$cluster))  # Nommer les clusters (c1, c2, etc.)
  
  # Créer un graphique pour représenter les années par cluster
  cluster_plot <- ggplot(data_algue, aes(x = `Passage : Année`, y = Cluster, color = Cluster)) +
    geom_point(size = 4, alpha = 0.8) +  # Représenter les années par des points
    scale_x_continuous(breaks = unique(data_algue$`Passage : Année`)) +  # Afficher toutes les années
    labs(title = paste("Répartition des années par cluster pour", nom_algue),
         x = "Année", y = "Cluster", color = "Cluster") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  
  print(cluster_plot)  # Afficher le graphique
}

# Appliquer la fonction sur chaque algue avec son k spécifique
resultats_clusters <- lapply(names(algues_k), function(a) visualiser_clusters_annees(a, algues_k[[a]]))