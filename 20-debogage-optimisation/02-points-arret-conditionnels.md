🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.2 Points d'Arrêt Conditionnels

## Introduction

Les points d'arrêt conditionnels sont l'une des fonctionnalités les plus puissantes du débogueur. Contrairement aux points d'arrêt simples qui stoppent l'exécution à chaque passage, les points d'arrêt conditionnels ne s'activent que lorsqu'une condition spécifique est remplie. Cette capacité transforme le débogage de tâches fastidieuses en investigations ciblées et efficaces.

**Dans cette section, vous apprendrez à :**
- Créer et configurer des points d'arrêt conditionnels
- Utiliser différents types de conditions
- Résoudre des problèmes complexes de manière élégante
- Optimiser votre processus de débogage

---

## 1. Le Problème : Pourquoi les Points d'Arrêt Conditionnels ?

### 1.1 Scénario Classique

Imaginez que vous avez ce code :

```pascal
program TraiterCommandes;

var
  i: Integer;
  commande: Integer;

begin
  for i := 1 to 10000 do
  begin
    commande := ObtenirCommande(i);
    TraiterCommande(commande);     // Point d'arrêt ici
  end;
end.
```

**Problème :** Vous savez que la commande numéro 7856 cause une erreur, mais vous ne voulez pas appuyer sur F9 (Continuer) 7856 fois !

**Solution :** Un point d'arrêt conditionnel avec `i = 7856`.

### 1.2 Autre Exemple Courant

```pascal
procedure TFormPrincipal.GererUtilisateurs;  
var  
  utilisateur: TUtilisateur;
begin
  for utilisateur in ListeUtilisateurs do
  begin
    if utilisateur.Actif then
      ProcesserUtilisateur(utilisateur);   // Bug avec un utilisateur spécifique
  end;
end;
```

Vous voulez arrêter uniquement pour l'utilisateur "Jean Dupont", pas pour les 5000 autres utilisateurs.

**Sans point d'arrêt conditionnel :** Des heures de F9 répétitifs.  
**Avec point d'arrêt conditionnel :** Arrêt immédiat sur le bon utilisateur.  

---

## 2. Créer un Point d'Arrêt Conditionnel

### 2.1 Méthode Standard

**Étape par Étape :**

1. **Placez un point d'arrêt normal**
   - Cliquez dans la marge gauche de l'éditeur (zone grise à gauche des numéros de ligne)
   - Un cercle rouge apparaît

2. **Accédez aux propriétés**
   - **Clic droit** sur le cercle rouge
   - Sélectionnez **"Propriétés du point d'arrêt"**

   Ou bien :
   - Menu **Exécuter** → **Points d'arrêt** → **Propriétés du point d'arrêt**

3. **Configurez la condition**
   - Dans la fenêtre qui s'ouvre, repérez le champ **"Expression"** ou **"Condition"**
   - Entrez votre condition (voir syntaxe ci-dessous)
   - Cliquez **OK**

4. **Vérification visuelle**
   - Le cercle rouge peut afficher un symbole supplémentaire (selon la version de Lazarus)
   - Passez la souris dessus pour voir la condition définie

### 2.2 Méthode Rapide (Versions Récentes)

Certaines versions récentes de Lazarus permettent :
- **Clic droit** sur le cercle rouge → **"Ajouter une condition..."**
- Entrée directe de la condition dans une boîte de dialogue simplifiée

### 2.3 Fenêtre de Gestion des Points d'Arrêt

**Accès :** Menu **Voir** → **Fenêtres de débogage** → **Points d'arrêt**

Cette fenêtre liste TOUS vos points d'arrêt avec :
- Le fichier et la ligne
- La condition (si définie)
- Le statut (actif/désactivé)
- Le nombre de passages

**Avantages :**
- Vue d'ensemble de tous vos points d'arrêt
- Modification rapide des conditions
- Activation/désactivation en masse

---

## 3. Syntaxe des Conditions

### 3.1 Opérateurs de Comparaison

Les opérateurs disponibles sont ceux du Pascal :

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `=` | Égal | `i = 100` |
| `<>` | Différent | `code <> 0` |
| `<` | Inférieur | `prix < 50` |
| `>` | Supérieur | `age > 18` |
| `<=` | Inférieur ou égal | `score <= 100` |
| `>=` | Supérieur ou égal | `temperature >= 20` |

**Exemples Pratiques :**

```pascal
// Arrêt quand le compteur atteint 500
i = 500

// Arrêt quand l'âge dépasse la limite
age > 65

// Arrêt quand le prix est dans une fourchette
(prix >= 100) and (prix <= 200)
```

### 3.2 Conditions sur les Chaînes de Caractères

Pour comparer des chaînes, utilisez l'opérateur `=` :

```pascal
// Arrêt pour un utilisateur spécifique
utilisateur.Nom = 'Dupont'

// Arrêt pour un code d'erreur spécifique
codeErreur = 'ERR_CONNEXION'

// Comparaison case-insensitive (insensible à la casse)
LowerCase(nom) = 'dupont'
```

**Important :** Les chaînes sont sensibles à la casse par défaut !
- `'Dupont'` ≠ `'dupont'`
- Utilisez `LowerCase()` ou `UpperCase()` pour ignorer la casse

### 3.3 Opérateurs Logiques

Combinez plusieurs conditions :

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `and` | ET logique | `(i > 100) and (i < 200)` |
| `or` | OU logique | `(code = 1) or (code = 2)` |
| `not` | NON logique | `not actif` |

**Exemples Combinés :**

```pascal
// Arrêt si l'indice est pair ET supérieur à 100
(i mod 2 = 0) and (i > 100)

// Arrêt si le statut est en erreur OU en attente
(statut = 'ERREUR') or (statut = 'ATTENTE')

// Arrêt si le montant est hors limites
(montant < 0) or (montant > 10000)

// Arrêt si utilisateur inactif ET ancien
(not utilisateur.Actif) and (utilisateur.DateCreation < '2020-01-01')
```

### 3.4 Conditions sur les Booléens

Pour les variables booléennes, deux syntaxes sont possibles :

```pascal
// Syntaxe complète
actif = True  
valide = False  

// Syntaxe abrégée (recommandée)
actif        // Équivaut à : actif = True  
not valide   // Équivaut à : valide = False  
```

**Exemple :**

```pascal
// Arrêt uniquement pour les utilisateurs actifs ayant un solde positif
utilisateur.Actif and (utilisateur.Solde > 0)
```

### 3.5 Utilisation de Fonctions

Vous pouvez utiliser certaines fonctions Pascal dans les conditions :

**Fonctions Courantes :**

```pascal
// Fonctions mathématiques
(i mod 10 = 0)        // Tous les 10 passages
(Abs(valeur) > 100)   // Valeur absolue

// Fonctions de chaînes
Length(nom) > 50      // Nom trop long  
Pos('error', message) > 0   // Message contient 'error'  
LowerCase(code) = 'urgent'  // Comparaison insensible à la casse  
Copy(ref, 1, 3) = 'CMD'     // Commence par 'CMD'  

// Fonctions de date (si disponibles selon contexte)
// Note : Peut ne pas fonctionner dans tous les débogueurs
FormatDateTime('YYYY', date) = '2025'
```

### 3.6 Accès aux Champs d'Enregistrements et Objets

Vous pouvez accéder aux champs de structures complexes :

```pascal
// Enregistrement (Record)
personne.Age > 18  
personne.Nom = 'Martin'  

// Objet
client.EstActif  
commande.Montant >= 1000  
utilisateur.Email = 'test@example.com'  

// Imbrication
commande.Client.Pays = 'France'  
produit.Categorie.Nom = 'Électronique'  
```

---

## 4. Cas d'Usage Pratiques

### 4.1 Débogage de Boucles : Trouver une Itération Spécifique

**Problème :** Votre boucle traite 10 000 éléments, et l'erreur survient à l'élément 8742.

```pascal
for i := 1 to 10000 do  
begin  
  element := Tableau[i];
  Traiter(element);   // Point d'arrêt ici
end;
```

**Solution :**
- Point d'arrêt sur `Traiter(element);`
- Condition : `i = 8742`
- Le programme s'arrêtera UNIQUEMENT à l'itération 8742

**Variante : Intervalle**

```pascal
// S'arrêter entre 8700 et 8800
(i >= 8700) and (i <= 8800)
```

### 4.2 Débogage d'une Valeur Problématique

**Problème :** Un calcul produit une valeur négative inappropriée.

```pascal
procedure CalculerPrix(var prix: Double);  
begin  
  prix := PrixBase * (1 - Remise);
  AppliquerTaxes(prix);        // Point d'arrêt ici
  AfficherPrix(prix);
end;
```

**Solution :**
- Point d'arrêt sur `AppliquerTaxes(prix);`
- Condition : `prix < 0`
- Vous identifiez immédiatement quand le prix devient négatif

**Autres Conditions Utiles :**

```pascal
// Détecter une division par zéro imminente
diviseur = 0

// Détecter un débordement
valeur > 2147483647   // Max Integer 32 bits

// Détecter une valeur NaN (Not a Number)
// Note : Peut nécessiter IsNaN() selon le contexte
```

### 4.3 Recherche dans une Liste

**Problème :** Vous avez une liste d'objets et cherchez un objet spécifique.

```pascal
procedure TraiterClients;  
var  
  client: TClient;
begin
  for client in ListeClients do
  begin
    ProcesserCommandes(client);   // Point d'arrêt ici
  end;
end;
```

**Solution :**
- Point d'arrêt sur `ProcesserCommandes(client);`
- Condition : `client.ID = 12345`
- Ou : `client.Nom = 'Société XYZ'`

### 4.4 Débogage de Fichiers : Ligne Spécifique

**Problème :** Vous lisez un fichier de 100 000 lignes, et l'erreur est à la ligne 87 654.

```pascal
procedure LireFichier;  
var  
  ligne: String;
  numLigne: Integer;
begin
  numLigne := 0;
  while not Eof(fichier) do
  begin
    Inc(numLigne);
    ReadLn(fichier, ligne);
    TraiterLigne(ligne);    // Point d'arrêt ici
  end;
end;
```

**Solution :**
- Point d'arrêt sur `TraiterLigne(ligne);`
- Condition : `numLigne = 87654`

### 4.5 Conditions Multi-Critères Complexes

**Scénario Réel :** Application de gestion de commandes.

```pascal
procedure ValiderCommande(cmd: TCommande);  
begin  
  // Point d'arrêt sur la ligne suivante
  if cmd.Valide then
    EnregistrerCommande(cmd);
end;
```

**Besoin :** Arrêter uniquement pour :
- Commandes du client ID 5678
- Montant supérieur à 5000€
- Pays = France
- Statut urgent

**Condition :**

```pascal
(cmd.ClientID = 5678) and (cmd.Montant > 5000) and (cmd.Pays = 'France') and cmd.Urgent
```

### 4.6 Détecter des États Incohérents

**Problème :** Deux variables devraient toujours être en phase, mais parfois elles divergent.

```pascal
procedure SynchroniserDonnees;  
begin  
  // compteurA et compteurB devraient être égaux
  Traiter();   // Point d'arrêt ici
end;
```

**Solution :**
- Condition : `compteurA <> compteurB`
- Vous détectez IMMÉDIATEMENT quand la synchronisation est perdue

**Autres Exemples d'Incohérences :**

```pascal
// Solde négatif alors que compte actif
(compte.Actif) and (compte.Solde < 0)

// Date de fin avant date de début
dateFin < dateDebut

// Pointeur null alors qu'il devrait être alloué
objet = nil

// Tableau vide alors qu'il devrait contenir des données
Length(tableau) = 0
```

---

## 5. Compteur de Passages (Hit Count)

### 5.1 Concept

Le compteur de passages permet de spécifier : **"Arrête-toi au Nième passage"**.

C'est différent d'une condition classique : le point d'arrêt compte ses activations et ne s'arrête qu'après un certain nombre.

### 5.2 Configuration

**Dans les Propriétés du Point d'Arrêt :**
- Champ **"Nombre de passages"** ou **"Hit Count"**
- Entrez une valeur : par exemple, `50`
- Le point s'activera au 50ème passage

**Modes Disponibles (selon version) :**
- **Égal à N** : S'arrête exactement au Nième passage
- **Multiple de N** : S'arrête tous les N passages
- **Supérieur ou égal à N** : S'arrête à partir du Nième passage

### 5.3 Cas d'Usage

**Exemple 1 : Problème Intermittent**

```pascal
for i := 1 to 1000 do  
begin  
  Traiter();   // Fonctionne 99% du temps, plante rarement
end;
```

Vous suspectez que le problème survient après plusieurs passages. Configurez un compteur de passages à 50 pour voir l'état après 50 itérations.

**Exemple 2 : Débogage Périodique**

```pascal
while not Termine do  
begin  
  MettreAJour();   // Point d'arrêt tous les 100 passages
end;
```

Compteur configuré en "Multiple de 100" : vous vérifiez l'état tous les 100 passages sans ralentir excessivement.

### 5.4 Combiner Condition et Compteur

**Puissance Maximale :** Vous pouvez combiner une condition ET un compteur de passages !

**Exemple :**
- Condition : `client.Pays = 'Belgique'`
- Compteur : 10
- **Résultat :** S'arrête au 10ème client belge

```pascal
for client in ListeClients do  
begin  
  Traiter(client);   // Condition: Pays = Belgique, Compteur: 10
end;
```

---

## 6. Actions sur Point d'Arrêt (Advanced)

### 6.1 Points d'Arrêt avec Actions

Certaines versions avancées de GDB permettent d'exécuter des actions au lieu de simplement s'arrêter.

**Actions Possibles :**
- Afficher un message dans la console
- Logger une valeur
- Continuer automatiquement après l'action

**Configuration (si disponible) :**
- Propriétés du point d'arrêt
- Section "Actions" ou "Commands"
- Entrez des commandes GDB

**Exemple de Commande :**
```gdb
print "Valeur de i:", i  
continue  
```

**Résultat :** Affiche la valeur de `i` à chaque passage SANS arrêter le programme.

### 6.2 Points d'Arrêt de Traçage (Tracepoints)

Les tracepoints sont des points d'arrêt qui collectent des données sans arrêter l'exécution.

**Usage :**
- Suivre l'évolution d'une variable dans une boucle
- Logger les appels de fonction
- Analyser les performances

**Note :** Fonctionnalité avancée, support variable selon la configuration.

---

## 7. Débogage Multi-plateforme

### 7.1 Différences Windows/Linux

**Généralement :**
- La syntaxe des conditions est identique sur Windows et Linux
- Les deux utilisent GDB (GNU Debugger)

**Particularités Linux :**
- Chemins de fichiers avec `/` au lieu de `\`
- Sensibilité à la casse des noms de fichiers
- Peut nécessiter `sudo` pour certains déboguages système

**Particularités Windows :**
- GDB embarqué avec Lazarus
- Chemins avec `\` ou `\\`
- Moins de restrictions de permissions

### 7.2 Tester sur les Deux Plateformes

**Bonne Pratique :**

Si vous développez du code multi-plateforme, testez vos points d'arrêt conditionnels sur les deux systèmes :

```pascal
{$IFDEF WINDOWS}
  // Code spécifique Windows
  chemin := 'C:\Data\fichier.txt';
{$ENDIF}

{$IFDEF LINUX}
  // Code spécifique Linux
  chemin := '/home/user/data/fichier.txt';
{$ENDIF}

// Point d'arrêt ici avec condition sur 'chemin'
```

**Condition adaptable :**
```pascal
Pos('erreur', LowerCase(chemin)) > 0
```

---

## 8. Limitations et Pièges à Éviter

### 8.1 Expressions Non Supportées

**Certaines limitations du débogueur :**

❌ **Appels de fonctions complexes**
```pascal
// Peut ne pas fonctionner :
MaFonctionPersonnalisee(x) = 10
```

❌ **Création de variables temporaires**
```pascal
// Ne fonctionne pas :
var temp := x + y; temp > 100
```

❌ **Expressions avec effets de bord**
```pascal
// Dangereux : modifie les données !
Inc(compteur)
```

✅ **Restez simple :**
- Comparaisons directes
- Fonctions standard du Pascal
- Opérateurs logiques de base

### 8.2 Performance

**Attention :** Les points d'arrêt conditionnels ralentissent l'exécution !

**Pourquoi ?**
- Le débogueur évalue la condition à chaque passage
- Dans une boucle de 1 million d'itérations, la condition sera évaluée 1 million de fois

**Bonnes Pratiques :**
- Utilisez des conditions simples (évitez les calculs lourds)
- Combinez avec des compteurs pour réduire les évaluations
- Désactivez les points d'arrêt une fois le débogage terminé

### 8.3 Conditions Invalides

**Que se passe-t-il si la condition est invalide ?**

Le débogueur peut :
- Ignorer le point d'arrêt
- Afficher une erreur dans la console de débogage
- S'arrêter systématiquement (comme un point d'arrêt normal)

**Vérification :**
- Testez votre condition sur une itération manuelle
- Vérifiez la console de débogage pour les messages d'erreur

### 8.4 Variables Hors de Portée

**Problème :**

```pascal
procedure A;  
var  
  x: Integer;
begin
  x := 10;
  B();   // Point d'arrêt dans B avec condition sur x
end;

procedure B;  
begin  
  // La variable x n'existe pas ici !
  Traiter();   // Condition : x = 10  ❌ ERREUR
end;
```

**Solution :**
- La condition doit porter sur des variables **visibles** à l'endroit du point d'arrêt
- Variables locales de la fonction courante
- Variables globales
- Paramètres de la fonction

---

## 9. Astuces d'Expert

### 9.1 Stratégie de Réduction Progressive

Quand vous cherchez un bug difficile :

1. **Premier point d'arrêt :** Condition large
   ```pascal
   (i >= 8000) and (i <= 9000)
   ```

2. **Analyse :** Vous constatez que l'erreur est entre 8400 et 8500

3. **Réduction :** Ajustez la condition
   ```pascal
   (i >= 8400) and (i <= 8500)
   ```

4. **Répétez** jusqu'à trouver l'itération exacte

### 9.2 Conditions "Sentinelle"

Utilisez des conditions pour détecter les situations qui "ne devraient jamais arriver" :

```pascal
// Cette variable ne devrait JAMAIS être négative
inventaire.Quantite < 0

// Ce pointeur ne devrait JAMAIS être nil ici
client = nil

// Cette somme devrait toujours être égale
SommeA <> SommeB

// Cet état est impossible selon la logique
(statut = 'TERMINE') and (progression < 100)
```

**Avantage :** Vous détectez les bugs logiques avant qu'ils ne causent des erreurs visibles.

### 9.3 Logging Conditionnel avec WriteLn

Combinez points d'arrêt et logging pour un débogage efficace :

```pascal
if ConditionDeDebug then  
begin  
  WriteLn('Debug: i=', i, ' valeur=', valeur);
  // Point d'arrêt ici avec condition : ConditionDeDebug
end;
```

**Avantage :**
- Vous voyez l'historique dans la console
- Le point d'arrêt vous permet d'inspecter en détail

### 9.4 Utiliser des Constantes de Débogage

```pascal
const
  DEBUG_CLIENT_ID = 12345;
  DEBUG_MONTANT_MIN = 1000;

// Dans votre code :
if (client.ID = DEBUG_CLIENT_ID) and (montant > DEBUG_MONTANT_MIN) then  
begin  
  // Point d'arrêt ici
  TraiterCasSpecial();
end;
```

**Avantage :** Modifiez les constantes pour ajuster facilement vos cibles de débogage.

### 9.5 Points d'Arrêt Temporaires

Vous pouvez créer des points d'arrêt qui se désactivent automatiquement après le premier arrêt :

**Astuce (manuelle) :**
1. Créez un point d'arrêt conditionnel
2. Après l'arrêt, désactivez-le immédiatement
3. Ou supprimez-le

**Cas d'usage :**
- Vérification unique d'un état initial
- Entrée dans une fonction rarement appelée

---

## 10. Exemples Complets de Situations Réelles

### 10.1 Recherche de Fuite Mémoire

**Contexte :** Vous créez des objets dans une boucle, et suspectez un oubli de libération.

```pascal
for i := 1 to 10000 do  
begin  
  client := TClient.Create;
  Traiter(client);
  client.Free;   // Point d'arrêt ici
end;
```

**Condition :**
```pascal
client = nil
```

**Objectif :** Détecter si `client` est nil avant Free (ce qui indiquerait un double Free ailleurs).

### 10.2 Analyse de Performance : Opération Lente

**Contexte :** Une opération devient lente après plusieurs passages.

```pascal
procedure OptimiserCache;  
var  
  tempDebut: TDateTime;
begin
  tempDebut := Now;
  EffectuerOperation();   // Point d'arrêt ici
  // Condition : (Now - tempDebut) > 1/(24*60*60)  (> 1 seconde)
end;
```

**Note :** Cette condition peut être complexe pour GDB, simplifiez si nécessaire.

### 10.3 Débogage de Transactions Base de Données

**Contexte :** Une transaction échoue de façon intermittente.

```pascal
try
  Connexion.StartTransaction;
  for enreg in Enregistrements do
  begin
    Enregistrer(enreg);   // Point d'arrêt ici
  end;
  Connexion.Commit;
except
  Connexion.Rollback;
end;
```

**Condition :**
```pascal
enreg.ID = 87456   // L'ID problématique identifié dans les logs
```

### 10.4 Validation de Données Complexes

**Contexte :** Vous importez des données, certaines sont invalides.

```pascal
for ligne in FichierCSV do  
begin  
  donnees := ParseLigne(ligne);
  if Valider(donnees) then
    Importer(donnees);   // Point d'arrêt ici
end;
```

**Condition :**
```pascal
(donnees.Nom = '') or (donnees.Age < 0) or (donnees.Email = '')
```

**Objectif :** Arrêter sur les données invalides pour comprendre pourquoi la validation a échoué.

---

## 11. Récapitulatif et Bonnes Pratiques

### 11.1 Quand Utiliser les Points d'Arrêt Conditionnels

✅ **À utiliser :**
- Boucles avec beaucoup d'itérations
- Recherche d'une valeur spécifique
- Débogage d'erreurs intermittentes
- Analyse de cas limites

❌ **À éviter :**
- Code exécuté rarement (un point d'arrêt simple suffit)
- Conditions très complexes (ralentissement)
- Fonctions appelées des millions de fois (impact performance majeur)

### 11.2 Checklist des Bonnes Pratiques

**Configuration :**
- [ ] Vérifiez que les informations de débogage sont activées
- [ ] Testez d'abord avec un point d'arrêt simple
- [ ] Commencez par une condition simple, affinez progressivement

**Syntaxe :**
- [ ] Utilisez des parenthèses pour clarifier les priorités
- [ ] Préférez `LowerCase()` pour les comparaisons de chaînes
- [ ] Évitez les fonctions avec effets de bord

**Performance :**
- [ ] Désactivez les points d'arrêt conditionnels une fois le débogage terminé
- [ ] Combinez avec des compteurs pour réduire les évaluations
- [ ] Documentez vos conditions complexes

**Multi-plateforme :**
- [ ] Testez vos conditions sur Windows ET Linux
- [ ] Utilisez des chemins portables
- [ ] Vérifiez la sensibilité à la casse

### 11.3 Aide-Mémoire Rapide

**Syntaxe de Base :**
```pascal
// Valeurs simples
i = 500  
prix > 100  
(age >= 18) and (age <= 65)

// Chaînes
nom = 'Dupont'  
LowerCase(code) = 'urgent'  

// Booléens
actif  
not termine  

// Conditions complexes
(client.Pays = 'France') and (commande.Montant > 5000)

// Détection d'anomalies
valeur < 0  
pointeur = nil  
compteurA <> compteurB  
```

---

## 12. Dépannage et Solutions

### 12.1 Le Point d'Arrêt Ne S'Active Jamais

**Causes possibles :**
1. La condition n'est jamais vraie
2. Erreur de syntaxe dans la condition
3. Variable hors de portée

**Solutions :**
- Simplifiez la condition pour tester (ex: `True`)
- Vérifiez la console de débogage pour les erreurs
- Utilisez un WriteLn pour vérifier que le code est atteint

### 12.2 Le Débogueur S'Arrête à Chaque Passage

**Causes possibles :**
1. La condition est toujours vraie
2. Erreur de syntaxe qui est ignorée

**Solutions :**
- Vérifiez la syntaxe de votre condition
- Testez avec une condition impossible (ex: `i = -999999`)
- Consultez la fenêtre Points d'Arrêt pour voir la condition enregistrée

### 12.3 Message d'Erreur "Cannot evaluate expression"

**Causes :**
- Variable non existante dans le contexte
- Fonction non supportée par le débogueur
- Syntaxe invalide

**Solutions :**
- Vérifiez que la variable existe à cet endroit
- Simplifiez l'expression
- Utilisez uniquement des opérateurs standard

---

## 13. Conclusion

Les points d'arrêt conditionnels sont un outil indispensable pour le débogage efficace. Ils transforment des heures de débogage fastidieux en quelques minutes de travail ciblé.

**Points Clés à Retenir :**
1. Les conditions utilisent la syntaxe Pascal standard
2. Vous pouvez combiner conditions et compteurs de passages
3. Commencez simple, affinez progressivement
4. Attention à la performance dans les boucles intensives
5. Testez sur les deux plateformes (Windows/Linux)

**Prochaine Étape :** Dans la section 20.3 (Inspection de variables et expressions), nous verrons comment analyser en profondeur l'état de votre programme une fois arrêté au bon endroit grâce à vos points d'arrêt conditionnels.

---

**Multi-plateforme Windows/Ubuntu** ✓  
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**  

⏭️ [Inspection de variables et expressions](/20-debogage-optimisation/03-inspection-variables-expressions.md)
