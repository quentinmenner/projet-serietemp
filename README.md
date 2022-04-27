# Projet de série temporelle linéaire

Consignes générales : Ce projet doit être réalisé sous R. La notation tiendra compte de la rigueur dans la
mise en œuvre des outils économétriques, de la concision et de la clarté de la présentation des résultats. Le
rapport (6 pages de contenu maximum hors annexe, en français ou en anglais) fera figurer les programmes
(commentés) en annexe. Ce tutorat doit être effectué de préférence en binôme.
Le barème (à titre indicatif) est le suivant : Partie 1 : 30%; Partie 2 : 30%; Partie 3 : 40%.
On s’intéresse à la modélisation et la prévision d’une série temporelle observée en France. Il est fortement
conseillé de choisir un indice de production industrielle en France (lien des s´eries de l’IPI). Vous ne travaillez
que sur les données observées. A partir du répertoire des séries chronologiques de l’INSEE, vous devez choisir 
une série agrégée corrigée des variations saisonnières et des jours ouvrés (CVS-CJO), mensuelle ou trimestrielle,
correspondant à n’importe quel secteur d’activité (à votre convenance) et contenant au moins 100 observations.
Une autre série (éviter des données en valeur nominale, de prix ou financières) est possible, tant qu’elle respecte
les caractéristiques énoncées précédemment.

Partie I : Les données

1. Que représente la s´erie choisie ? (secteur, périmètre, traitements éventuels, transformation logarithmique, etc.)

2. Transformer si besoin la série pour la rendre stationnaire (différentiation, suppression de la tendance
déterministe, etc.). Justifier soigneusement vos choix.

3. Repr´esenter graphiquement la s´erie choisie avant et après transformation.

Partie II : Modèles ARMA

4. Choisir, en le justifiant, un modèle ARMA(p,q) pour votre série corrigée Xt. Estimer les paramères
du modèle et vérifier sa validité.

5. Exprimer le modèle ARIMA(p,d,q) pour la s´erie choisie.

Partie III : Prévision

On note T la longueur de la série. On suppose que les résidus de la série sont gaussiens.

6. Ecrire l’équation vérifiée par la région de confiance de niveau α sur les valeurs futures (XT +1, XT +2).

7. Préciser les hypothèses utilisées pour obtenir cette région.

8. Représenter graphiquement cette région pour α = 95%. Commenter.

9. Question ouverte : soit Yt une série stationnaire disponible de t = 1 à T. On suppose que YT +1
est disponible plus rapidement que XT +1. Sous quelle(s) condition(s) cette information permet-elle
d’améliorer la prévision de XT +1 ? Comment la (les) testeriez-vous ?
