🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.2 Fichiers texte : ouverture, lecture, écriture

## Introduction

Les fichiers texte sont les plus simples à manipuler en Pascal. Dans cette section, nous allons apprendre à :
- **Ouvrir** un fichier (pour le lire ou y écrire)
- **Lire** son contenu
- **Écrire** des données dedans
- **Fermer** le fichier proprement

**Analogie :** Manipuler un fichier, c'est comme manipuler un livre :
- Vous devez d'abord **l'ouvrir** (à la première page ou à la fin)
- Vous pouvez ensuite **lire** ce qu'il contient
- Ou **écrire** de nouvelles pages
- Enfin, vous devez **le fermer** avant de le ranger

---

## Les étapes de manipulation d'un fichier texte

Voici les étapes obligatoires pour travailler avec un fichier texte :

1. **Déclarer** une variable de type `TextFile`
2. **Associer** le fichier à un nom physique sur le disque (`Assign`)
3. **Ouvrir** le fichier dans le mode approprié (`Reset`, `Rewrite`, ou `Append`)
4. **Lire ou écrire** les données
5. **Fermer** le fichier (`Close`)

---

## 1. Déclaration et association

### Déclaration d'une variable fichier

```pascal
var
  MonFichier: TextFile;
```

Cette ligne crée une variable `MonFichier` qui représentera notre fichier dans le programme.

### Association avec un fichier physique : Assign

```pascal
Assign(MonFichier, 'donnees.txt');
```

La procédure `Assign` établit le lien entre la variable `MonFichier` et le fichier réel `donnees.txt` sur le disque dur.

**Important :**
- Le fichier n'est pas encore ouvert à ce stade
- Vous pouvez spécifier un chemin complet : `'C:\Mes Documents\donnees.txt'` (Windows)
- Ou un chemin relatif : `'donnees.txt'` (dans le répertoire du programme)

---

## 2. Ouverture d'un fichier texte

Il existe trois façons d'ouvrir un fichier texte, selon ce que vous voulez faire :

### Reset : Ouvrir en lecture

```pascal
Reset(MonFichier);
```

- Ouvre le fichier en **lecture seule**
- Le curseur est placé au **début** du fichier
- **Erreur** si le fichier n'existe pas
- Utilisez cette méthode quand vous voulez lire un fichier existant

**Analogie :** C'est comme ouvrir un livre à la première page pour le lire.

### Rewrite : Ouvrir en écriture (création)

```pascal
Rewrite(MonFichier);
```

- Ouvre le fichier en **écriture**
- **Crée** le fichier s'il n'existe pas
- **Efface** tout le contenu si le fichier existe déjà (attention !)
- Le curseur est placé au début du fichier (maintenant vide)
- Utilisez cette méthode pour créer un nouveau fichier ou remplacer complètement un fichier existant

**Analogie :** C'est comme prendre un cahier neuf (ou arracher toutes les pages d'un ancien cahier) pour écrire depuis le début.

### Append : Ouvrir en ajout

```pascal
Append(MonFichier);
```

- Ouvre le fichier en **écriture**
- **Erreur** si le fichier n'existe pas (contrairement à `Rewrite`)
- **Conserve** le contenu existant
- Le curseur est placé à la **fin** du fichier
- Utilisez cette méthode pour ajouter des données à un fichier existant

**Analogie :** C'est comme ouvrir un cahier à la dernière page écrite pour continuer à écrire à la suite.

---

## 3. Lecture de fichiers texte

### Lire une ligne : ReadLn

```pascal
var
  Ligne: string;
begin
  ReadLn(MonFichier, Ligne);
end;
```

- Lit une ligne complète du fichier
- Place le contenu dans la variable `Ligne`
- Passe automatiquement à la ligne suivante

### Lire un mot ou un nombre : Read

```pascal
var
  Mot: string;
  Nombre: Integer;
begin
  Read(MonFichier, Mot);      // Lit jusqu'au prochain espace ou retour à la ligne
  Read(MonFichier, Nombre);   // Lit un nombre
end;
```

- Lit un élément (mot, nombre) sans passer à la ligne suivante
- S'arrête au premier espace, tabulation ou retour à la ligne

### Vérifier la fin du fichier : EOF

```pascal
while not EOF(MonFichier) do
begin
  ReadLn(MonFichier, Ligne);
  WriteLn(Ligne);  // Affiche la ligne à l'écran
end;
```

**EOF** (End Of File) retourne `True` quand on a atteint la fin du fichier.

**Important :** Toujours vérifier EOF avant de lire pour éviter une erreur !

### Vérifier la fin de ligne : EOLn

```pascal
while not EOLn(MonFichier) do
begin
  Read(MonFichier, Caractere);
  // Traite le caractère
end;
```

**EOLn** (End Of Line) retourne `True` quand on a atteint la fin de la ligne courante.

---

## 4. Écriture dans un fichier texte

### Écrire une ligne : WriteLn

```pascal
WriteLn(MonFichier, 'Bonjour tout le monde !');
WriteLn(MonFichier, 'Ceci est la deuxième ligne.');
```

- Écrit le texte dans le fichier
- Ajoute automatiquement un retour à la ligne

### Écrire sans retour à la ligne : Write

```pascal
Write(MonFichier, 'Prénom : ');
Write(MonFichier, 'Jean');
WriteLn(MonFichier);  // Passe à la ligne
```

- Écrit le texte sans passer à la ligne
- Utile pour écrire plusieurs éléments sur une même ligne

### Écrire des nombres

```pascal
var
  Age: Integer;
  Prix: Real;
begin
  Age := 25;
  Prix := 19.99;

  WriteLn(MonFichier, 'Age : ', Age);
  WriteLn(MonFichier, 'Prix : ', Prix:0:2);  // Format : 2 décimales
end;
```

Les nombres sont automatiquement convertis en texte lors de l'écriture.

---

## 5. Fermeture du fichier

```pascal
Close(MonFichier);
```

**Très important :** Toujours fermer un fichier après utilisation !

**Pourquoi ?**
- Libère les ressources système
- Garantit que toutes les données sont bien écrites sur le disque
- Permet à d'autres programmes d'accéder au fichier
- Évite la corruption de données

**Analogie :** C'est comme refermer un livre et le ranger sur l'étagère.

---

## Exemple complet : Écrire dans un fichier

```pascal
program EcrireFichier;

var
  MonFichier: TextFile;
  i: Integer;

begin
  // 1. Association
  Assign(MonFichier, 'nombres.txt');

  // 2. Ouverture en écriture (création)
  Rewrite(MonFichier);

  // 3. Écriture
  WriteLn(MonFichier, 'Liste des premiers nombres :');
  WriteLn(MonFichier, '----------------------------');

  for i := 1 to 10 do
    WriteLn(MonFichier, 'Nombre ', i, ' : ', i * i);

  // 4. Fermeture
  Close(MonFichier);

  WriteLn('Fichier créé avec succès !');
end.
```

**Résultat dans le fichier `nombres.txt` :**
```
Liste des premiers nombres :
----------------------------
Nombre 1 : 1
Nombre 2 : 4
Nombre 3 : 9
Nombre 4 : 16
...
```

---

## Exemple complet : Lire un fichier

```pascal
program LireFichier;

var
  MonFichier: TextFile;
  Ligne: string;
  NumLigne: Integer;

begin
  // 1. Association
  Assign(MonFichier, 'nombres.txt');

  // 2. Ouverture en lecture
  Reset(MonFichier);

  // 3. Lecture ligne par ligne
  NumLigne := 1;

  while not EOF(MonFichier) do
  begin
    ReadLn(MonFichier, Ligne);
    WriteLn('Ligne ', NumLigne, ' : ', Ligne);
    Inc(NumLigne);
  end;

  // 4. Fermeture
  Close(MonFichier);

  WriteLn('Lecture terminée !');
end.
```

---

## Exemple complet : Ajouter à un fichier existant

```pascal
program AjouterFichier;

var
  MonFichier: TextFile;

begin
  // 1. Association
  Assign(MonFichier, 'journal.txt');

  // 2. Ouverture en mode ajout
  Append(MonFichier);

  // 3. Ajout de nouvelles lignes
  WriteLn(MonFichier, '--- Nouvelle entrée ---');
  WriteLn(MonFichier, 'Date : 13/10/2025');
  WriteLn(MonFichier, 'Message : Programme exécuté avec succès');
  WriteLn(MonFichier);  // Ligne vide

  // 4. Fermeture
  Close(MonFichier);

  WriteLn('Données ajoutées au journal !');
end.
```

---

## Gestion basique des erreurs

Il est important de vérifier que le fichier existe avant de le lire :

```pascal
program LectureSecurisee;

var
  MonFichier: TextFile;
  Ligne: string;

begin
  Assign(MonFichier, 'donnees.txt');

  // Tentative d'ouverture
  {$I-}  // Désactive la gestion automatique des erreurs
  Reset(MonFichier);
  {$I+}  // Réactive la gestion automatique des erreurs

  // Vérification
  if IOResult <> 0 then
  begin
    WriteLn('ERREUR : Le fichier n''existe pas ou est inaccessible !');
    Exit;
  end;

  // Lecture normale
  while not EOF(MonFichier) do
  begin
    ReadLn(MonFichier, Ligne);
    WriteLn(Ligne);
  end;

  Close(MonFichier);
end.
```

**Explications :**
- `{$I-}` désactive les erreurs automatiques
- `IOResult` retourne 0 si tout s'est bien passé, un code d'erreur sinon
- `{$I+}` réactive les erreurs automatiques

---

## Tableau récapitulatif des procédures

| Procédure | Fonction | Usage |
|-----------|----------|-------|
| `Assign(f, nom)` | Associe une variable fichier à un nom physique | Toujours en premier |
| `Reset(f)` | Ouvre en lecture | Lire un fichier existant |
| `Rewrite(f)` | Ouvre en écriture (efface) | Créer un nouveau fichier |
| `Append(f)` | Ouvre en ajout | Ajouter à la fin d'un fichier |
| `ReadLn(f, var)` | Lit une ligne complète | Lecture ligne par ligne |
| `Read(f, var)` | Lit un élément | Lecture mot par mot |
| `WriteLn(f, data)` | Écrit et passe à la ligne | Écriture avec retour ligne |
| `Write(f, data)` | Écrit sans passer à la ligne | Écriture sur une même ligne |
| `Close(f)` | Ferme le fichier | Toujours en dernier |
| `EOF(f)` | Teste la fin du fichier | Contrôle de boucle de lecture |
| `EOLn(f)` | Teste la fin de ligne | Lecture caractère par caractère |

---

## Bonnes pratiques

✅ **Toujours fermer les fichiers** avec `Close()` après utilisation

✅ **Vérifier l'existence** d'un fichier avant de le lire avec `Reset`

✅ **Attention avec Rewrite** : il efface le contenu existant !

✅ **Utiliser Append** pour ajouter sans détruire les données existantes

✅ **Vérifier EOF** avant chaque lecture pour éviter les erreurs

✅ **Gérer les erreurs** avec `{$I-}` et `IOResult` pour les opérations critiques

✅ **Utiliser des chemins complets** pour éviter les confusions sur l'emplacement du fichier

✅ **Commenter votre code** pour expliquer ce que fait chaque opération sur le fichier

---

## Résumé

Pour manipuler un fichier texte en Pascal :

1. **Déclarer** : `var F: TextFile;`
2. **Associer** : `Assign(F, 'fichier.txt');`
3. **Ouvrir** :
   - `Reset(F)` pour lire
   - `Rewrite(F)` pour créer/écraser
   - `Append(F)` pour ajouter
4. **Utiliser** :
   - `ReadLn(F, variable)` pour lire
   - `WriteLn(F, données)` pour écrire
5. **Fermer** : `Close(F);`

Les fichiers texte sont parfaits pour stocker des configurations, des logs, ou des données lisibles par l'humain. Dans la prochaine section, nous verrons les fichiers binaires et typés pour des besoins plus avancés.

---

> **Conseil :** Pratiquez en créant de petits programmes qui lisent et écrivent dans des fichiers. C'est en manipulant réellement des fichiers que vous comprendrez vraiment leur fonctionnement !

⏭️ [Fichiers binaires et accès direct](/08-gestion-fichiers-io/03-fichiers-binaires-acces-direct.md)
