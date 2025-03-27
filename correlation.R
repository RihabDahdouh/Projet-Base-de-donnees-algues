library(dplyr)

#selectionner les colonnes necessaires
new_data <- data2 %>%select(`Passage : Année`, `Résultat : Valeur de la mesure`, `Résultat : Libellé paramètre`)

#selectionner uniquement les lignes avec le paramètre température de l'eau
filtered_data <- new_data %>%
  filter(`Résultat : Libellé paramètre` == "Température de l'eau")

# Vérifier et convertir les valeurs en numérique
filtered_data <- filtered_data %>%
  mutate(`Résultat : Valeur de la mesure` = as.numeric(`Résultat : Valeur de la mesure`))

# Résumé statistique par année
stats_temp <- filtered_data %>%
  group_by(`Passage : Année`) %>%
  summarise(
    min_temp = min(`Résultat : Valeur de la mesure`, na.rm = TRUE),
    med_temp = median(`Résultat : Valeur de la mesure`, na.rm = TRUE),
    max_temp = max(`Résultat : Valeur de la mesure`, na.rm = TRUE)
  )
stats_temp <- stats_temp %>%
  rename(Année = `Passage : Année`)
##############################"
#résultats clusters contient une classification de chaque année dans un cluster
class(stats_temp)
class(resultaaats_clusters)
str(resultaaats_clusters[[1]]$data)
str(resultaaats_clusters[[2]]$data)
str(resultaaats_clusters[[3]]$data)
########################################
#chaque resultat d'algue dans une variable
resultats_cluster1 <- resultaaats_clusters[[1]]$data
resultats_cluster2 <- resultaaats_clusters[[2]]$data
resultats_cluster3 <- resultaaats_clusters[[3]]$data
print(resultats_cluster1)
print(resultats_cluster2)
print(resultats_cluster3)
#changer le nom de la colonne année 
resultats_cluster1 <- resultats_cluster1 %>%
  rename(Année = `Passage : Année`)
resultats_cluster2 <- resultats_cluster2 %>%
  rename(Année = `Passage : Année`)
resultats_cluster3 <- resultats_cluster3 %>%
  rename(Année = `Passage : Année`)
##############################################################
#construire un tableau pour visualiser l'appartenance des années à des algues 
df_clusters1 <- resultats_cluster1 %>%
  select(Année, Cluster) %>%
  pivot_wider(names_from = Cluster, values_from = Cluster, 
              values_fn = length, values_fill = list(Cluster = 0))
print(df_clusters1)
df_clusters2 <- resultats_cluster2 %>%
  select(Année, Cluster) %>%
  pivot_wider(names_from = Cluster, values_from = Cluster, 
              values_fn = length, values_fill = list(Cluster = 0))
print(df_clusters2)
df_clusters3 <- resultats_cluster3 %>%
  select(Année, Cluster) %>%
  pivot_wider(names_from = Cluster, values_from = Cluster, 
              values_fn = length, values_fill = list(Cluster = 0))
print(df_clusters3)

# Fusionner les valeurs  de température avec les centres des clusters
final_data1 <- left_join(stats_temp, df_clusters1, by = "Année")
print(final_data1)
final_data2 <- left_join(stats_temp, df_clusters2, by = "Année")
print(final_data2)
final_data3 <- left_join(stats_temp, df_clusters3, by = "Année")
final_data3<- na.omit(final_data3)
print(final_data3)

# Calculer les  matrices de corrélation
cor_matrix1 <- cor(final_data1[, -1], use = "complete.obs", method = "pearson")
cor_matrix2 <- cor(final_data2[, -1], use = "complete.obs", method = "pearson")
cor_matrix3 <- cor(final_data3[, -1], use = "complete.obs", method = "pearson")
#Affichage
print(cor_matrix1)
print(cor_matrix2)
print(cor_matrix3)













