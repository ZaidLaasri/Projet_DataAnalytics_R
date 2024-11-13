## Étape 1 : Chargement et Visualisation de la Structure des Données

# Charger les données
projet <- read.csv("projet.csv", sep = ",", dec = ".")
projet_new <- read.csv("projet_new.csv", sep = ",", dec = ".")

# Vérifier la structure des données pour identifier les types de variables
str(projet)
str(projet_new)

# Résumé statistique de toutes les variables
summary(projet)

# Installer et charger ggplot2 pour les graphiques
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Histogrammes pour les variables numériques (exemple : age, revenus, debcred)
ggplot(projet, aes(x = age)) + geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribution de l'âge", x = "Âge", y = "Fréquence")

ggplot(projet, aes(x = revenus)) + geom_histogram(binwidth = 100, fill = "green", color = "black") + 
  labs(title = "Distribution des revenus", x = "Revenus", y = "Fréquence")

# Diagramme circulaire pour une variable catégorielle (exemple : defaut)
pie(table(projet$defaut), main = "Répartition des défauts de paiement")

# Table des effectifs de la variable defaut
table(projet$defaut)

# Table de contingence pour observer la distribution de deux variables catégorielles (exemple : education et defaut)
table(projet$education, projet$defaut)

# Table de contingence avec proportions
prop.table(table(projet$education, projet$defaut)) * 100

# Nuage de points pour deux variables continues (exemple : revenus et debcred) avec la couleur selon defaut
ggplot(projet, aes(x = revenus, y = debcred, color = defaut)) + 
  geom_point() + 
  labs(title = "Relation entre revenus et ratio débit/crédit", x = "Revenus", y = "Débit/Crédit")

# Nuage de points avec jitter pour mieux voir les valeurs de certaines variables discrètes
ggplot(projet, aes(x = age, y = revenus, color = defaut)) + 
  geom_jitter(height = 0.2) + 
  labs(title = "Nuage de points des âges et revenus", x = "Âge", y = "Revenus")

# Remplacer 999 par NA dans les colonnes concernées
projet$age[projet$age == 999] <- NA
projet$adresse[projet$adresse == 999] <- NA

colSums(is.na(projet))

## Étape 2 : Pré-traitement des Données

# Imputer les valeurs manquantes par la moyenne pour les variables continues
projet$age[is.na(projet$age)] <- mean(projet$age, na.rm = TRUE)
projet$adresse[is.na(projet$adresse)] <- mean(projet$adresse, na.rm = TRUE)

# Convertir les variables en facteurs
projet$education <- as.factor(projet$education)
projet$defaut <- as.factor(projet$defaut)

# Normaliser les variables continues
variables_a_normaliser <- c("revenus", "debcred", "debcarte", "autres")
projet[variables_a_normaliser] <- scale(projet[variables_a_normaliser])

# Vérifier la structure et un résumé des données après pré-traitement
str(projet)
summary(projet)

## Étape 3 : Clustering des Données

# Créer un sous-ensemble des variables pour le clustering
clustering_data <- projet[, c("age", "revenus", "debcred", "debcarte", "autres")]

if(!require(factoextra)) install.packages("factoextra")
library(factoextra)

# Calculer et visualiser le coude
fviz_nbclust(clustering_data, kmeans, method = "wss") +
  labs(title = "Méthode du Coude pour déterminer le nombre optimal de clusters")

set.seed(123)  # Pour garantir des résultats reproductibles
kmeans_result <- kmeans(clustering_data, centers = 3, nstart = 25)

# Ajouter les clusters au dataset pour analyse
projet$cluster <- as.factor(kmeans_result$cluster)

# Moyennes des variables dans chaque cluster
aggregate(cbind(age, revenus, debcred, debcarte, autres) ~ cluster, data = projet, mean)

# Distribution de la variable cible defaut dans chaque cluster
table(projet$cluster, projet$defaut)

# Visualiser les clusters avec deux variables continues pour représenter les groupes
fviz_cluster(kmeans_result, data = clustering_data,
             geom = "point", ellipse.type = "norm") +
  labs(title = "Visualisation des Clusters avec k = 3")

## Étape 4 : Définition de la Méthode d'Évaluation des Classifieurs

set.seed(123)  # Pour la reproductibilité
index <- sample(1:nrow(projet), size = 0.8 * nrow(projet))
train_data <- projet[index, ]
test_data <- projet[-index, ]

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

# 1. Régression Logistique
log_model <- glm(defaut ~ ., data = train_data, family = binomial)
log_pred <- predict(log_model, test_data, type = "response")
log_class <- ifelse(log_pred > 0.5, "Oui", "Non")
confusionMatrix(as.factor(log_class), as.factor(test_data$defaut))

# 2. Arbre de Décision
tree_model <- rpart(defaut ~ ., data = train_data, method = "class")
tree_pred <- predict(tree_model, test_data, type = "class")
confusionMatrix(as.factor(tree_pred), as.factor(test_data$defaut))

# 3. RandomForest
rf_model <- randomForest(defaut ~ ., data = train_data, ntree = 100)
rf_pred <- predict(rf_model, test_data)
confusionMatrix(as.factor(rf_pred), as.factor(test_data$defaut))

# 4. K-Nearest Neighbors (k-NN)
knn_model <- kknn(defaut ~ ., train = train_data, test = test_data, k = 10, distance = 2)
knn_pred <- fitted(knn_model)
confusionMatrix(as.factor(knn_pred), as.factor(test_data$defaut))

## Étape 6 : Optimiser le Modèle de Régression Logistique avec Régularisation Lasso

# Charger les packages nécessaires
if(!require(glmnet)) install.packages("glmnet")
if(!require(caret)) install.packages("caret")
library(glmnet)
library(caret)

# Nettoyer les données de test pour correspondre au modèle
test_data_clean <- na.omit(test_data)

# Créer x_test en s'assurant qu'il correspond bien à x_train
x_test <- model.matrix(defaut ~ . - 1, data = test_data_clean)

# Vérifier et ajouter les colonnes manquantes dans x_test pour correspondre à x_train
missing_cols_test <- setdiff(colnames(x_train), colnames(x_test))
for (col in missing_cols_test) {
  x_test <- cbind(x_test, setNames(data.frame(rep(0, nrow(x_test))), col))
}

# Réorganiser les colonnes de x_test pour correspondre exactement à celles de x_train
x_test <- x_test[, colnames(x_train)]

# Convertir x_test en matrice pour la compatibilité avec le modèle Lasso
x_test <- as.matrix(x_test)

# Utiliser le meilleur lambda (déjà trouvé lors de l'entraînement du modèle)
# Effectuer des prédictions sur l'ensemble de test nettoyé avec le modèle Lasso
log_pred_test <- predict(final_lasso_model, newx = x_test, s = best_lambda, type = "response")
log_class_test <- ifelse(log_pred_test > 0.4, "Oui", "Non")

# Calculer et afficher la matrice de confusion
confusion_matrix <- confusionMatrix(as.factor(log_class_test), as.factor(test_data_clean$defaut))
print(confusion_matrix)


## Étape 7 : Courbe ROC pour Comparaison des Modèles

# Installer et charger le package pROC pour les courbes ROC
if(!require(pROC)) install.packages("pROC")
library(pROC)

# Créer des objets ROC pour chaque modèle
rf_roc <- roc(test_data$defaut, as.numeric(rf_pred == "Oui"))
log_roc <- roc(test_data$defaut, as.numeric(log_class == "Oui"))
tree_roc <- roc(test_data$defaut, as.numeric(tree_pred == "Oui"))
knn_roc <- roc(test_data$defaut, as.numeric(knn_pred == "Oui"))

# Tracer toutes les courbes ROC sur un même graphique
plot(rf_roc, col="blue", main="Courbes ROC des Modèles")
plot(log_roc, col="green", add=TRUE)
plot(tree_roc, col="red", add=TRUE)
plot(knn_roc, col="purple", add=TRUE)
legend("bottomright", legend=c("Random Forest", "Logistic Regression", "Decision Tree", "KNN"), 
       col=c("blue", "green", "red", "purple"), lwd=2)


## Étape 8 : Application du Modèle aux Données projet_new et Génération des Prédictions

# Normaliser les variables continues dans projet_new et remplacer les valeurs manquantes
projet_new$age[projet_new$age == 999] <- NA
projet_new$adresse[projet_new$adresse == 999] <- NA
projet_new$age[is.na(projet_new$age)] <- mean(train_data_clean$age, na.rm = TRUE)
projet_new$adresse[is.na(projet_new$adresse)] <- mean(train_data_clean$adresse, na.rm = TRUE)

# Assurez-vous que `education` dans `projet_new` a les mêmes niveaux que dans les données d'entraînement
projet_new$education <- factor(projet_new$education, levels = levels(train_data_clean$education))

# Normaliser les variables continues
variables_a_normaliser <- c("revenus", "debcred", "debcarte", "autres")
projet_new[variables_a_normaliser] <- scale(projet_new[variables_a_normaliser])

# Créer `x_new` avec les mêmes colonnes que `x_train`
x_new <- model.matrix(~ . - 1, data = projet_new)

# Vérifiez et ajoutez les colonnes manquantes pour correspondre à `x_train`
missing_cols <- setdiff(colnames(x_train), colnames(x_new))
for (col in missing_cols) {
  x_new <- cbind(x_new, setNames(data.frame(rep(0, nrow(x_new))), col))
}

# Réorganisez les colonnes de `x_new` pour correspondre à `x_train`
x_new <- x_new[, colnames(x_train)]

# Convertir `x_new` en matrice pour la compatibilité avec le modèle Lasso
x_new <- as.matrix(x_new)

# Effectuer les prédictions sur `x_new` avec le meilleur lambda
log_pred_new <- predict(final_lasso_model, newx = x_new, s = best_lambda, type = "response")
log_class_new <- ifelse(log_pred_new > 0.4, "Oui", "Non")

# Création du tableau final avec les informations demandées
resultats <- data.frame(
  client_id = projet_new$client,    # Numéro d'identification du client
  classe_predite = log_class_new,   # Classe prédite ("Oui" ou "Non")
  probabilite = log_pred_new        # Probabilité associée
)

# Sauvegarder le fichier avec les prédictions
write.csv(resultats, "projet_new_predictions.csv", row.names = FALSE)

# Affichage du résumé des prédictions
cat("Résumé des prédictions :\n")
print(table(Predicted = log_class_new))



