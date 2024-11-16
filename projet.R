## Étape 1 : Chargement et Visualisation de la Structure des Données

# Charger les bibliothèques nécessaires
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(corrplot)) install.packages("corrplot")
if(!require(VIM)) install.packages("VIM")
library(ggplot2)
library(corrplot)
library(VIM)

# Charger les données
projet <- read.csv("projet.csv", sep = ",", dec = ".")
projet_new <- read.csv("projet_new.csv", sep = ",", dec = ".")

# 1. Exploration initiale des données
# Structure et résumé statistique
str(projet)
summary(projet)

# 2. Visualisation des valeurs manquantes
aggr(projet, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, 
     labels = names(projet), cex.axis = .7, gap = 3, 
     ylab = c("Données complètes", "Données manquantes"))

colSums(is.na(projet))


# 3. Traitement des valeurs extrêmes
# Remplacer 999 par NA dans `age` et `adresse`
projet$age[projet$age == 999] <- NA
projet$adresse[projet$adresse == 999] <- NA

# Supprimer les valeurs extrêmes des revenus (5e et 95e percentiles)
projet_filtered <- subset(projet, revenus >= quantile(revenus, 0.05) & revenus <= quantile(revenus, 0.95))

# 4. Visualisations des distributions et relations

# Histogramme de la distribution de l'âge (exclut les valeurs extrêmes)
ggplot(projet, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribution de l'âge des clients", x = "Âge", y = "Fréquence") + 
  scale_x_continuous(limits = c(15, 80), breaks = seq(15, 80, by = 5)) +
  theme_minimal()

# Histogramme de la distribution des revenus après suppression des valeurs extrêmes
ggplot(projet_filtered, aes(x = revenus)) + 
  geom_histogram(binwidth = 10, fill = "green", color = "black") + 
  labs(title = "Distribution des revenus (sans valeurs extrêmes)", x = "Revenus", y = "Fréquence") + 
  scale_x_continuous(breaks = seq(0, max(projet_filtered$revenus, na.rm = TRUE), by = 10)) +
  theme_minimal()

# Diagramme circulaire pour la répartition de `defaut`
defaut_data <- as.data.frame(table(projet$defaut))
colnames(defaut_data) <- c("Defaut", "Count")

ggplot(defaut_data, aes(x = "", y = Count, fill = Defaut)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Répartition des défauts de paiement") +
  theme_minimal() +
  scale_fill_manual(values = c("Non" = "lightblue", "Oui" = "salmon"))

# Relation entre les revenus et le ratio débit/crédit
ggplot(projet_filtered, aes(x = revenus, y = debcred, color = defaut)) + 
  geom_point(alpha = 0.7) + 
  labs(title = "Relation entre revenus et ratio débit/crédit", x = "Revenus", y = "Débit/Crédit") +
  scale_color_manual(values = c("Non" = "blue", "Oui" = "red")) +
  scale_x_continuous(breaks = seq(0, max(projet_filtered$revenus, na.rm = TRUE), by = 10)) +
  theme_minimal()

# Nuage de points pour visualiser la relation entre l'âge et les revenus, coloré selon `defaut`
ggplot(projet_filtered, aes(x = age, y = revenus, color = defaut)) + 
  geom_jitter(height = 0.3, width = 0.3, alpha = 0.6) + 
  labs(title = "Relation entre l'âge et les revenus des clients", x = "Âge", y = "Revenus") +
  scale_color_manual(values = c("Non" = "blue", "Oui" = "red")) +
  theme_minimal()

# 5. Analyse des corrélations
# Calcul et affichage de la matrice de corrélation
numeric_vars <- projet[, sapply(projet, is.numeric)]
cor_matrix <- cor(na.omit(numeric_vars))
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.cex = 0.8, number.cex = 0.7, 
         title = "Matrice de corrélation des variables numériques", 
         addCoef.col = "black", mar = c(0, 0, 1, 0))

# 6. Tables de fréquence et de contingence
# Effectifs de `defaut`
table(projet$defaut)

# Table de contingence pour `education` et `defaut`
table(projet$education, projet$defaut)

# Table de contingence avec proportions pour `education` et `defaut`
prop.table(table(projet$education, projet$defaut)) * 100


## Étape 2 : Pré-traitement des Données

# 1. Imputer les valeurs manquantes par la moyenne pour les variables continues
projet$age[is.na(projet$age)] <- mean(projet$age, na.rm = TRUE)
projet$adresse[is.na(projet$adresse)] <- mean(projet$adresse, na.rm = TRUE)

# 2. Vérifier et supprimer les doublons
projet <- projet[!duplicated(projet), ]

# 3. Convertir les variables en facteurs (ordonnées pour education)
projet$education <- factor(projet$education, 
                           levels = c("Niveau bac", "Bac+2", "Bac+3", "Bac+4", "Bac+5 et plus"), 
                           ordered = TRUE)
projet$defaut <- as.factor(projet$defaut)

# 4. Normaliser les variables continues
variables_a_normaliser <- c("revenus", "debcred", "debcarte", "autres")
projet[variables_a_normaliser] <- scale(projet[variables_a_normaliser])

# 5. Vérifier la répartition de la variable cible `defaut`
prop.table(table(projet$defaut)) * 100

# 6. Sauvegarder les données prétraitées
write.csv(projet, "projet_preprocessed.csv", row.names = FALSE)

# Vérifier la structure et un résumé des données après pré-traitement
str(projet)
summary(projet)


## Étape 3 : Clustering des Données

# 1. Préparation des données pour le clustering
# On utilise uniquement les variables numériques normalisées
variables_clustering <- projet[, c("age", "revenus", "debcred", "debcarte", "autres")]

# 2. Choix du nombre de clusters (méthode de l'inertie)
# Calcul de l'inertie pour k de 1 à 10 et tracé de la courbe pour trouver le "coude"
wss <- sapply(1:10, function(k) {
  kmeans(variables_clustering, centers = k, nstart = 20)$tot.withinss
})

# Visualisation de la méthode du coude pour choisir k
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Nombre de clusters K", 
     ylab = "Inertie intra-cluster totale (Within Sum of Squares)",
     main = "Méthode du Coude pour choisir le nombre de clusters")

# 3. Exécution de k-means avec le nombre optimal de clusters (k = 3)
set.seed(123)  # Pour des résultats reproductibles
k_optimal <- 3  
kmeans_result <- kmeans(variables_clustering, centers = k_optimal, nstart = 20)

# 4. Ajout des clusters aux données originales
projet$cluster <- as.factor(kmeans_result$cluster)

# 5. Visualisation des clusters
# Visualisation des clusters par âge et revenus
ggplot(projet, aes(x = age, y = revenus, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = paste("Clustering des clients (k =", k_optimal, ")"), x = "Âge", y = "Revenus") +
  theme_minimal() +
  scale_color_manual(values = rainbow(k_optimal))

# Visualisation des clusters par revenus et ratio débit/crédit
ggplot(projet, aes(x = revenus, y = debcred, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = paste("Clustering des clients selon revenus et ratio débit/crédit (k =", k_optimal, ")"), 
       x = "Revenus", y = "Débit/Crédit") +
  theme_minimal() +
  scale_color_manual(values = rainbow(k_optimal))

# 6. Visualisation supplémentaire avec boxplots
# Boxplot pour visualiser la distribution de l'âge par cluster
ggplot(projet, aes(x = cluster, y = age, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Distribution de l'âge par cluster", x = "Cluster", y = "Âge") +
  theme_minimal()

# Boxplot pour visualiser la distribution des revenus par cluster
ggplot(projet, aes(x = cluster, y = revenus, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Distribution des revenus par cluster", x = "Cluster", y = "Revenus") +
  theme_minimal()

# 7. Validation de la qualité du clustering avec l'indice de silhouette
# Charger le package cluster si nécessaire
if(!require(cluster)) install.packages("cluster")
library(cluster)

# Calcul et tracé du score de silhouette
silhouette_scores <- silhouette(kmeans_result$cluster, dist(variables_clustering))
plot(silhouette_scores, main = "Silhouette plot pour le clustering")

# 8. Analyse des clusters
# Calcul de statistiques descriptives pour chaque cluster
library(dplyr)
projet %>% 
  group_by(cluster) %>% 
  summarise(
    age_moyen = mean(age),
    revenus_moyen = mean(revenus),
    debcred_moyen = mean(debcred),
    debcarte_moyen = mean(debcarte),
    autres_moyen = mean(autres),
    effectif = n()
  )

# Calcul du taux de défaut dans chaque cluster pour analyser les différences de risque
projet %>% 
  group_by(cluster) %>% 
  summarise(
    taux_defaut = mean(defaut == "Oui") * 100,
    effectif = n()
  )


## Étape 4 : Définition de la Méthode d'Évaluation des Classifieurs

# 1. Division des données en ensembles d'entraînement et de test
set.seed(123)  # Pour assurer la reproductibilité
index <- sample(1:nrow(projet), size = 0.8 * nrow(projet))
train_data <- projet[index, ]
test_data <- projet[-index, ]

# Vérification des dimensions des ensembles
cat("Nombre d'observations dans l'ensemble d'entraînement :", nrow(train_data), "\n")
cat("Nombre d'observations dans l'ensemble de test :", nrow(test_data), "\n")

# 2. Définition des métriques d'évaluation
# Nous utiliserons les métriques suivantes :
# - Précision : VP / (VP + FP)
# - Rappel : VP / (VP + FN)
# - F1-Score : 2 * (Précision * Rappel) / (Précision + Rappel)
# - Taux de succès : (VP + VN) / Total
# - AUC : Aire sous la courbe ROC

## Étape 5 : Construction et Évaluation des Classifieurs

# Installer et charger les packages nécessaires
if(!require(randomForest)) install.packages("randomForest")
library(randomForest)

if(!require(rpart)) install.packages("rpart")
library(rpart)

if(!require(kknn)) install.packages("kknn")
library(kknn)

if(!require(caret)) install.packages("caret")
library(caret)

if(!require(ROCR)) install.packages("ROCR")
library(ROCR)

# 1. Régression Logistique
cat("### Régression Logistique ###\n")
log_model <- glm(defaut ~ ., data = train_data, family = binomial)
log_pred <- predict(log_model, test_data, type = "response")
log_class <- ifelse(log_pred > 0.5, "Oui", "Non")
log_conf_matrix <- confusionMatrix(as.factor(log_class), as.factor(test_data$defaut))
print(log_conf_matrix)

# Calcul des métriques
log_precision <- log_conf_matrix$byClass['Pos Pred Value']
log_recall <- log_conf_matrix$byClass['Sensitivity']
log_f1_score <- 2 * (log_precision * log_recall) / (log_precision + log_recall)
cat("Précision:", log_precision, "\n")
cat("Rappel:", log_recall, "\n")
cat("F1 Score:", log_f1_score, "\n\n")

# 2. Arbre de Décision
cat("\n### Arbre de Décision ###\n")
tree_model <- rpart(defaut ~ ., data = train_data, method = "class")
tree_pred <- predict(tree_model, test_data, type = "class")
tree_conf_matrix <- confusionMatrix(as.factor(tree_pred), as.factor(test_data$defaut))
print(tree_conf_matrix)

# Calcul des métriques
tree_precision <- tree_conf_matrix$byClass['Pos Pred Value']
tree_recall <- tree_conf_matrix$byClass['Sensitivity']
tree_f1_score <- 2 * (tree_precision * tree_recall) / (tree_precision + tree_recall)
cat("Précision:", tree_precision, "\n")
cat("Rappel:", tree_recall, "\n")
cat("F1 Score:", tree_f1_score, "\n\n")

# 3. Forêt Aléatoire
cat("\n### Forêt Aléatoire ###\n")

# Imputation des valeurs manquantes pour log_debcred
train_data$log_debcred[is.na(train_data$log_debcred)] <- mean(train_data$log_debcred, na.rm = TRUE)

# Vérifier qu'il n'y a plus de valeurs manquantes
print(colSums(is.na(train_data)))

# Entraîner le modèle
rf_model <- randomForest(defaut ~ ., data = train_data, ntree = 100)

# Prédiction
rf_pred <- predict(rf_model, test_data)

# Matrice de confusion
rf_conf_matrix <- confusionMatrix(rf_pred, test_data$defaut)
print(rf_conf_matrix)

# Calcul des métriques
rf_precision <- rf_conf_matrix$byClass['Pos Pred Value']
rf_recall <- rf_conf_matrix$byClass['Sensitivity']
rf_f1_score <- 2 * (rf_precision * rf_recall) / (rf_precision + rf_recall)
cat("Précision:", rf_precision, "\n")
cat("Rappel:", rf_recall, "\n")
cat("F1 Score:", rf_f1_score, "\n")

#4. Entraîner le modèle k-NN
# 1. Vérification des données et gestion des valeurs manquantes

cat("\n### Vérification des données ###\n")
cat("Dimensions de train_data :", dim(train_data), "\n")
cat("Dimensions de test_data :", dim(test_data), "\n")
cat("Valeurs manquantes dans test_data :\n")
print(colSums(is.na(test_data)))

# Imputation des valeurs manquantes
test_data$log_debcred[is.na(test_data$log_debcred)] <- mean(test_data$log_debcred, na.rm = TRUE)

# Vérification des valeurs manquantes après imputation
cat("\n### Après imputation ###\n")
print(colSums(is.na(test_data)))

# 2. Aligner les colonnes entre train_data et test_data
cat("\n### Alignement des colonnes ###\n")
common_columns <- intersect(colnames(train_data), colnames(test_data))
train_data_knn <- train_data[, common_columns]
test_data_knn <- test_data[, common_columns]

cat("Dimensions des données pour k-NN :\n")
cat("Train data :", dim(train_data_knn), "\n")
cat("Test data :", dim(test_data_knn), "\n")

# 3. Entraîner le modèle k-NN et prédire
cat("\n### K-Nearest Neighbors ###\n")

# Entraîner le modèle k-NN
knn_model <- kknn(defaut ~ ., train = train_data_knn, test = test_data_knn, k = 10, distance = 2)

# Obtenir les prédictions
knn_pred <- predict(knn_model)

# Ajuster les niveaux des prédictions pour correspondre aux niveaux de la variable cible
knn_pred <- factor(knn_pred, levels = levels(test_data$defaut))

# Vérification des longueurs
cat("Longueur des prédictions :", length(knn_pred), "\n")
cat("Longueur des labels réels :", length(test_data$defaut), "\n")

# 4. Calcul de la matrice de confusion
if (length(knn_pred) == length(test_data$defaut)) {
  knn_conf_matrix <- confusionMatrix(knn_pred, test_data$defaut)
  print(knn_conf_matrix)
} else {
  cat("Erreur : Les longueurs des prédictions et des labels réels ne correspondent toujours pas.\n")
}

# 5. Calcul des métriques d’évaluation
if (exists("knn_conf_matrix")) {
  precision <- knn_conf_matrix$byClass['Pos Pred Value']  # Précision
  recall <- knn_conf_matrix$byClass['Sensitivity']        # Rappel
  f1_score <- 2 * (precision * recall) / (precision + recall)  # F1 Score
  
  # Affichage des métriques
  cat("\n### Résultats des métriques ###\n")
  cat("Précision :", precision, "\n")
  cat("Rappel :", recall, "\n")
  cat("F1 Score :", f1_score, "\n")
}


## Étape 6 : Optimisation du Modèle avec Régularisation Lasso

# 1. Création de x_train et y_train synchronisés
complete_indices <- complete.cases(train_data_clean)  # Identifier les lignes complètes
x_train <- model.matrix(defaut ~ . - 1, data = train_data_clean[complete_indices, ])
y_train <- as.numeric(train_data_clean$defaut[complete_indices]) - 1  # 0 = "Non", 1 = "Oui"

# Vérification des dimensions
cat("Dimensions de x_train :", dim(x_train), "\n")
cat("Longueur de y_train :", length(y_train), "\n")

# 2. Validation croisée pour trouver le lambda optimal
library(glmnet)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial", type.measure = "auc")
cat("Lambda optimal trouvé :", cv_lasso$lambda.min, "\n")

# 3. Entraîner le modèle final avec le lambda optimal
final_lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min, family = "binomial")

# 4. Calcul de l'AUC sur les données d'entraînement (ou validation)
library(ROCR)
log_pred_lasso <- predict(final_lasso_model, newx = x_train, type = "response")
pred_obj <- prediction(log_pred_lasso, y_train)
auc_lasso <- performance(pred_obj, "auc")@y.values[[1]]
cat("AUC pour le modèle optimisé :", auc_lasso, "\n")

# 5. Visualisation de la courbe ROC
perf_lasso <- performance(pred_obj, "tpr", "fpr")
plot(perf_lasso, main = "Courbe ROC pour le modèle Lasso régularisé")

## Étape 7 : Application du Modèle aux Données projet_new et Génération des Prédictions

# 1. Prétraitement des données projet_new
projet_new$age[projet_new$age == 999] <- NA
projet_new$adresse[projet_new$adresse == 999] <- NA
projet_new$age[is.na(projet_new$age)] <- mean(train_data_clean$age, na.rm = TRUE)
projet_new$adresse[is.na(projet_new$adresse)] <- mean(train_data_clean$adresse, na.rm = TRUE)

projet_new$education[is.na(projet_new$education)] <- "Inconnu"
projet_new$education <- factor(projet_new$education, levels = levels(train_data_clean$education))

variables_a_normaliser <- c("revenus", "debcred", "debcarte", "autres")
projet_new[variables_a_normaliser] <- scale(projet_new[variables_a_normaliser])

# 2. Préparation des données pour le modèle
x_new <- model.matrix(~ . - 1, data = projet_new)

# Ajouter les colonnes manquantes pour correspondre à x_train
missing_cols <- setdiff(colnames(x_train), colnames(x_new))
for (col in missing_cols) {
  x_new <- cbind(x_new, setNames(data.frame(rep(0, nrow(x_new))), col))
}

x_new <- x_new[, colnames(x_train)]  # Réorganiser les colonnes
x_new <- as.matrix(x_new)  # Conversion explicite en matrice

# Vérifications finales
cat("Nombre de colonnes dans x_train :", ncol(x_train), "\n")
cat("Nombre de colonnes dans x_new :", ncol(x_new), "\n")

# 3. Prédictions avec le modèle optimisé Lasso
log_pred_new <- predict(final_lasso_model, newx = x_new, type = "response")

# 4. Ajout des prédictions
log_class_new <- ifelse(log_pred_new > 0.4, "Oui", "Non")
projet_new$defaut <- log_class_new

# 5. Sauvegarde des résultats
write.csv(projet_new, "projet_new_predictions.csv", row.names = FALSE)

# 6. Résumé des prédictions
cat("\n### Résumé des prédictions ###\n")
table(Predicted = log_class_new)


