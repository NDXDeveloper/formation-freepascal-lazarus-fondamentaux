🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.2 Gestion portable des chemins (PathDelim, DirectorySeparator)

## Introduction

Dans le chapitre précédent, nous avons vu que Windows et Linux utilisent des séparateurs différents pour les chemins de fichiers (`\` vs `/`). Maintenant, nous allons apprendre concrètement comment écrire du code qui fonctionne sur les deux systèmes sans modification.

La bonne nouvelle : FreePascal fournit tout ce qu'il faut pour gérer automatiquement ces différences ! Vous n'aurez jamais besoin de vous demander "suis-je sous Windows ou Linux ?" pour construire un chemin de fichier.

---

## 1. Les Constantes Essentielles

FreePascal définit plusieurs constantes dans l'unité `SysUtils` qui s'adaptent automatiquement à la plateforme :

### PathDelim

**C'est la constante la plus importante !**

```pascal
uses
  SysUtils;

begin
  WriteLn('Séparateur de chemins : ', PathDelim);

  // Sous Windows affiche : \
  // Sous Linux affiche : /
end.
```

**Type :** `Char` (un seul caractère)

**Utilisation :** Pour construire des chemins de fichiers et répertoires.

### DirectorySeparator

```pascal
uses
  SysUtils;

begin
  WriteLn('Séparateur de répertoires : ', DirectorySeparator);

  // Même valeur que PathDelim
  // C'est un alias pour plus de clarté dans le code
end.
```

**Type :** `Char`

**Différence avec PathDelim :** Aucune ! Ce sont deux noms pour la même chose. Utilisez celui que vous préférez. `PathDelim` est plus court, `DirectorySeparator` est plus explicite.

### PathSeparator (Attention : différent !)

```pascal
uses
  SysUtils;

begin
  WriteLn('Séparateur de PATH : ', PathSeparator);

  // Sous Windows affiche : ;
  // Sous Linux affiche : :
end.
```

**Attention :** `PathSeparator` n'est PAS pour les chemins de fichiers ! Il sert à séparer les chemins dans la variable d'environnement PATH.

Exemple de PATH :
- **Windows :** `C:\Program Files;C:\Windows;C:\Windows\System32`
- **Linux :** `/usr/bin:/usr/local/bin:/home/user/bin`

### LineEnding

```pascal
uses
  SysUtils;

begin
  WriteLn('Fin de ligne : ', LineEnding);

  // Sous Windows : #13#10 (CR+LF)
  // Sous Linux : #10 (LF)
end.
```

**Type :** `String`

**Utilisation :** Pour les retours à la ligne dans les fichiers texte.

---

## 2. Construction de Chemins Simples

### Méthode Basique : Concaténation avec PathDelim

```pascal
uses
  SysUtils;

var
  Chemin: string;
begin
  // Construction manuelle
  Chemin := 'Data' + PathDelim + 'Config' + PathDelim + 'settings.ini';

  WriteLn(Chemin);
  // Windows : Data\Config\settings.ini
  // Linux : Data/Config/settings.ini
end.
```

**Avantage :** Simple et explicite.

**Inconvénient :** Répétitif si vous avez beaucoup de parties à assembler.

### Exemple Pratique : Fichier de Configuration

```pascal
program ConfigApp;

uses
  SysUtils;

var
  RepApp, RepConfig, FichierConfig: string;
begin
  // Répertoire de l'application
  RepApp := ExtractFilePath(ParamStr(0));

  // Sous-répertoire config
  RepConfig := RepApp + 'config';

  // Fichier dans ce répertoire
  FichierConfig := RepConfig + PathDelim + 'app.ini';

  WriteLn('Fichier de config : ', FichierConfig);

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(RepConfig) then
  begin
    CreateDir(RepConfig);
    WriteLn('Répertoire config créé.');
  end;
end.
```

---

## 3. Fonctions Utiles pour Gérer les Chemins

FreePascal fournit des fonctions toutes faites pour manipuler les chemins. **Utilisez-les !** Elles sont plus sûres et plus lisibles.

### IncludeTrailingPathDelimiter

**Ajoute un séparateur à la fin du chemin s'il n'y en a pas déjà un.**

```pascal
uses
  SysUtils;

var
  Rep: string;
begin
  Rep := 'C:\Data';
  Rep := IncludeTrailingPathDelimiter(Rep);
  WriteLn(Rep);  // C:\Data\

  Rep := '/home/user/docs/';
  Rep := IncludeTrailingPathDelimiter(Rep);
  WriteLn(Rep);  // /home/user/docs/  (inchangé, déjà présent)
end.
```

**Pourquoi c'est utile ?**
```pascal
var
  RepBase, Fichier, CheminComplet: string;
begin
  RepBase := 'Data';
  Fichier := 'test.txt';

  // SANS IncludeTrailingPathDelimiter - RISQUE D'ERREUR :
  CheminComplet := RepBase + PathDelim + Fichier;  // OK
  CheminComplet := RepBase + Fichier;              // ERREUR : Datatest.txt

  // AVEC IncludeTrailingPathDelimiter - TOUJOURS CORRECT :
  CheminComplet := IncludeTrailingPathDelimiter(RepBase) + Fichier;  // Data\test.txt
end.
```

### ExcludeTrailingPathDelimiter

**Enlève le séparateur à la fin du chemin s'il est présent.**

```pascal
uses
  SysUtils;

var
  Rep: string;
begin
  Rep := 'C:\Data\';
  Rep := ExcludeTrailingPathDelimiter(Rep);
  WriteLn(Rep);  // C:\Data

  Rep := '/home/user/docs';
  Rep := ExcludeTrailingPathDelimiter(Rep);
  WriteLn(Rep);  // /home/user/docs  (inchangé, pas de séparateur)
end.
```

**Cas d'usage :** Affichage propre, comparaison de chemins.

### IncludeLeadingPathDelimiter / ExcludeLeadingPathDelimiter

```pascal
uses
  SysUtils;

var
  Chemin: string;
begin
  Chemin := 'data\config';
  Chemin := IncludeLeadingPathDelimiter(Chemin);
  WriteLn(Chemin);  // \data\config

  Chemin := ExcludeLeadingPathDelimiter(Chemin);
  WriteLn(Chemin);  // data\config
end.
```

**Moins utilisé** que les versions "Trailing", mais peut être utile dans certains cas.

---

## 4. Assemblage de Chemins Complexes

### Fonction ConcatPaths (FreePascal 3.2+)

```pascal
uses
  SysUtils;

var
  Chemin: string;
begin
  // Assemble plusieurs parties intelligemment
  Chemin := ConcatPaths(['Documents', 'Projets', 'MonApp', 'data.db']);
  WriteLn(Chemin);
  // Windows : Documents\Projets\MonApp\data.db
  // Linux : Documents/Projets/MonApp/data.db
end.
```

**Avantages :**
- Très lisible
- Gère automatiquement les séparateurs
- Supporte un nombre variable de parties

### Votre Propre Fonction si ConcatPaths N'est Pas Disponible

```pascal
uses
  SysUtils;

function ConstruireChemin(const Parties: array of string): string;  
var  
  i: Integer;
begin
  Result := '';
  for i := Low(Parties) to High(Parties) do
  begin
    if i = Low(Parties) then
      Result := Parties[i]
    else
      Result := IncludeTrailingPathDelimiter(Result) + Parties[i];
  end;
end;

var
  Chemin: string;
begin
  Chemin := ConstruireChemin(['home', 'user', 'documents', 'fichier.txt']);
  WriteLn(Chemin);
end.
```

---

## 5. Extraction de Parties de Chemins

### ExtractFilePath

**Extrait le chemin du répertoire (sans le nom du fichier).**

```pascal
uses
  SysUtils;

var
  CheminComplet, Repertoire: string;
begin
  CheminComplet := 'C:\Users\Pierre\Documents\rapport.pdf';
  Repertoire := ExtractFilePath(CheminComplet);
  WriteLn(Repertoire);  // C:\Users\Pierre\Documents\

  // Note : conserve le séparateur final
end.
```

### ExtractFileName

**Extrait uniquement le nom du fichier (avec son extension).**

```pascal
uses
  SysUtils;

var
  CheminComplet, NomFichier: string;
begin
  CheminComplet := '/home/user/documents/rapport.pdf';
  NomFichier := ExtractFileName(CheminComplet);
  WriteLn(NomFichier);  // rapport.pdf
end.
```

### ExtractFileExt

**Extrait l'extension du fichier.**

```pascal
uses
  SysUtils;

var
  CheminComplet, Extension: string;
begin
  CheminComplet := 'C:\Data\image.jpg';
  Extension := ExtractFileExt(CheminComplet);
  WriteLn(Extension);  // .jpg

  // Note : inclut le point
end.
```

### ExtractFileNameWithoutExt (FreePascal 3.0+)

**Extrait le nom du fichier sans l'extension.**

```pascal
uses
  SysUtils;

var
  CheminComplet, NomSansExt: string;
begin
  CheminComplet := 'document.backup.txt';
  NomSansExt := ExtractFileNameWithoutExt(CheminComplet);
  WriteLn(NomSansExt);  // document.backup

  // Note : enlève seulement la dernière extension
end.
```

### Exemple Complet : Analyser un Chemin

```pascal
program AnalyseChemin;

uses
  SysUtils;

var
  Chemin: string;
begin
  Chemin := 'C:\Users\Pierre\Documents\Projet\rapport_final.pdf';

  WriteLn('Chemin complet : ', Chemin);
  WriteLn('Répertoire     : ', ExtractFilePath(Chemin));
  WriteLn('Nom fichier    : ', ExtractFileName(Chemin));
  WriteLn('Extension      : ', ExtractFileExt(Chemin));
  WriteLn('Sans extension : ', ExtractFileNameWithoutExt(ExtractFileName(Chemin)));

  // Résultat :
  // Chemin complet : C:\Users\Pierre\Documents\Projet\rapport_final.pdf
  // Répertoire     : C:\Users\Pierre\Documents\Projet\
  // Nom fichier    : rapport_final.pdf
  // Extension      : .pdf
  // Sans extension : rapport_final
end.
```

---

## 6. Modification et Changement d'Extensions

### ChangeFileExt

**Remplace l'extension d'un fichier.**

```pascal
uses
  SysUtils;

var
  Fichier, NouveauNom: string;
begin
  Fichier := 'document.txt';
  NouveauNom := ChangeFileExt(Fichier, '.pdf');
  WriteLn(NouveauNom);  // document.pdf

  // Si pas d'extension d'origine :
  Fichier := 'rapport';
  NouveauNom := ChangeFileExt(Fichier, '.docx');
  WriteLn(NouveauNom);  // rapport.docx
end.
```

**Astuce :** N'oubliez pas le point dans la nouvelle extension !

---

## 7. Chemins Absolus vs Relatifs

### IsPathDelimiter

**Vérifie si un caractère à une position donnée est un séparateur.**

```pascal
uses
  SysUtils;

var
  Chemin: string;
begin
  Chemin := 'C:\Data\Config';

  if IsPathDelimiter(Chemin, 3) then  // Position du premier \
    WriteLn('Séparateur trouvé en position 3');
end.
```

**Utilité :** Rarement utilisé directement, mais utile dans des fonctions d'analyse de chemins.

### Détecter un Chemin Absolu

```pascal
uses
  SysUtils;

function EstCheminAbsolu(const Chemin: string): Boolean;  
begin  
  {$IFDEF WINDOWS}
  // Sous Windows : commence par lettre de lecteur (C:\) ou UNC (\\)
  Result := (Length(Chemin) >= 3) and
            (Chemin[2] = ':') and
            IsPathDelimiter(Chemin, 3);
  Result := Result or ((Length(Chemin) >= 2) and
                       (Chemin[1] = PathDelim) and
                       (Chemin[2] = PathDelim));
  {$ENDIF}

  {$IFDEF LINUX}
  // Sous Linux : commence par /
  Result := (Length(Chemin) > 0) and (Chemin[1] = PathDelim);
  {$ENDIF}
end;

var
  C1, C2: string;
begin
  C1 := 'C:\Windows\System32';
  C2 := 'data\config.ini';

  WriteLn(C1, ' est absolu : ', EstCheminAbsolu(C1));  // True
  WriteLn(C2, ' est absolu : ', EstCheminAbsolu(C2));  // False
end.
```

---

## 8. Conversion et Normalisation

### CreateAbsolutePath

**Convertit un chemin relatif en chemin absolu.**

```pascal
uses
  SysUtils;

var
  Relatif, Absolu: string;
begin
  Relatif := 'config' + PathDelim + 'app.ini';
  Absolu := ExpandFileName(Relatif);
  WriteLn('Relatif : ', Relatif);
  WriteLn('Absolu  : ', Absolu);

  // Exemple de sortie :
  // Relatif : config\app.ini
  // Absolu  : C:\MonProgramme\config\app.ini
end.
```

### ExpandFileName

**Complète un nom de fichier avec le répertoire courant si nécessaire.**

```pascal
uses
  SysUtils;

var
  Fichier: string;
begin
  // Si le fichier n'a pas de chemin, ajoute le répertoire courant
  Fichier := ExpandFileName('test.txt');
  WriteLn(Fichier);  // C:\Users\Pierre\MonProjet\test.txt (par exemple)
end.
```

### CleanAndExpandFilename

**Nettoie et normalise un chemin (gère les .. et .).**

```pascal
uses
  FileUtil;  // Attention : unité FileUtil, pas SysUtils

var
  CheminSale, CheminPropre: string;
begin
  CheminSale := 'C:\Users\Pierre\..\Public\.\Documents\fichier.txt';
  CheminPropre := CleanAndExpandFilename(CheminSale);
  WriteLn(CheminPropre);  // C:\Users\Public\Documents\fichier.txt
end.
```

**Gère :**
- `..` (répertoire parent)
- `.` (répertoire courant)
- Doubles séparateurs (`\\` ou `//`)

---

## 9. Cas Pratiques Complets

### Cas 1 : Charger un Fichier de Configuration

```pascal
program ChargementConfig;

uses
  SysUtils, Classes;

function ObtenirCheminConfig: string;  
var  
  RepApp: string;
begin
  // Répertoire de l'exécutable
  RepApp := ExtractFilePath(ParamStr(0));

  // Construire le chemin vers le fichier config
  Result := IncludeTrailingPathDelimiter(RepApp) +
            'config' + PathDelim + 'settings.ini';
end;

procedure ChargerConfiguration;  
var  
  CheminConfig: string;
  Config: TStringList;
begin
  CheminConfig := ObtenirCheminConfig;

  WriteLn('Chargement de : ', CheminConfig);

  if FileExists(CheminConfig) then
  begin
    Config := TStringList.Create;
    try
      Config.LoadFromFile(CheminConfig);
      WriteLn('Configuration chargée : ', Config.Count, ' lignes');

      // Traiter la configuration...

    finally
      Config.Free;
    end;
  end
  else
    WriteLn('Fichier de configuration introuvable !');
end;

begin
  ChargerConfiguration;
end.
```

### Cas 2 : Sauvegarder des Données Utilisateur

```pascal
program SauvegardeUtilisateur;

uses
  SysUtils, Classes;

function ObtenirRepertoireDonnees: string;  
begin  
  {$IFDEF WINDOWS}
  // Sous Windows : AppData\Roaming
  Result := GetEnvironmentVariable('APPDATA');
  {$ENDIF}

  {$IFDEF LINUX}
  // Sous Linux : ~/.local/share
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.local' + PathDelim + 'share';
  {$ENDIF}

  // Ajouter le nom de notre application
  Result := IncludeTrailingPathDelimiter(Result) + 'MonApp';

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    ForceDirectories(Result);

  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure SauvegarderDonnees(const Donnees: string);  
var  
  CheminFichier: string;
  Fichier: TStringList;
begin
  CheminFichier := ObtenirRepertoireDonnees + 'donnees.txt';

  WriteLn('Sauvegarde dans : ', CheminFichier);

  Fichier := TStringList.Create;
  try
    Fichier.Text := Donnees;
    Fichier.SaveToFile(CheminFichier);
    WriteLn('Données sauvegardées avec succès.');
  finally
    Fichier.Free;
  end;
end;

begin
  SauvegarderDonnees('Mes données importantes...');
end.
```

### Cas 3 : Parcourir un Arbre de Répertoires

```pascal
program ParcoursRepertoire;

uses
  SysUtils;

procedure ParcoururRepertoire(const Repertoire: string; Niveau: Integer = 0);  
var  
  Info: TSearchRec;
  Chemin: string;
  Indentation: string;
begin
  Indentation := StringOfChar(' ', Niveau * 2);
  Chemin := IncludeTrailingPathDelimiter(Repertoire);

  // Chercher tous les fichiers et répertoires
  if FindFirst(Chemin + '*', faAnyFile, Info) = 0 then
  begin
    repeat
      // Ignorer . et ..
      if (Info.Name <> '.') and (Info.Name <> '..') then
      begin
        if (Info.Attr and faDirectory) = faDirectory then
        begin
          // C'est un répertoire
          WriteLn(Indentation, '[DIR] ', Info.Name);

          // Appel récursif
          ParcoururRepertoire(Chemin + Info.Name, Niveau + 1);
        end
        else
        begin
          // C'est un fichier
          WriteLn(Indentation, Info.Name);
        end;
      end;
    until FindNext(Info) <> 0;

    FindClose(Info);
  end;
end;

begin
  WriteLn('Parcours du répertoire courant :');
  WriteLn('================================');
  ParcoururRepertoire(GetCurrentDir);
end.
```

---

## 10. Pièges à Éviter

### ❌ Piège 1 : Séparateur Codé en Dur

```pascal
// MAUVAIS - Ne fonctionnera pas sous Linux
Chemin := 'C:\Data\fichier.txt';

// BON
Chemin := 'C:' + PathDelim + 'Data' + PathDelim + 'fichier.txt';

// ENCORE MIEUX (portable)
Chemin := GetCurrentDir + PathDelim + 'Data' + PathDelim + 'fichier.txt';
```

### ❌ Piège 2 : Oublier le Séparateur Final

```pascal
// RISQUÉ
Repertoire := 'Data';  
Fichier := Repertoire + 'config.ini';  // Donne : Dataconfig.ini !!!  

// SÛR
Repertoire := IncludeTrailingPathDelimiter('Data');  
Fichier := Repertoire + 'config.ini';  // Donne : Data\config.ini  
```

### ❌ Piège 3 : Mélanger les Séparateurs

```pascal
// MAUVAIS - Mélange \ et /
Chemin := 'Data\Config/fichier.txt';

// BON - Utilise toujours PathDelim
Chemin := 'Data' + PathDelim + 'Config' + PathDelim + 'fichier.txt';
```

### ❌ Piège 4 : Supposer une Lettre de Lecteur

```pascal
// MAUVAIS - Spécifique Windows
Chemin := 'C:\Program Files\MonApp';

// BON - Utiliser le répertoire d'exécution
Chemin := ExtractFilePath(ParamStr(0));
```

---

## 11. Bonnes Pratiques Résumées

### ✅ Toujours Utiliser les Constantes

```pascal
// Utilisez PathDelim ou DirectorySeparator
Chemin := 'data' + PathDelim + 'config' + PathDelim + 'app.ini';
```

### ✅ Utiliser les Fonctions Fournies

```pascal
// Préférez les fonctions intégrées à votre propre code
Chemin := IncludeTrailingPathDelimiter(Rep) + Fichier;  
NomSeul := ExtractFileName(Chemin);  
```

### ✅ Tester sur les Deux Plateformes

```pascal
// Compilez et testez régulièrement sous Windows ET Linux
// N'attendez pas la fin du projet !
```

### ✅ Éviter les Chemins Absolus Codés en Dur

```pascal
// MAUVAIS
Chemin := 'C:\MonApp\data.db';

// BON - Relatif au programme
RepApp := ExtractFilePath(ParamStr(0));  
Chemin := RepApp + 'data.db';  
```

### ✅ Gérer les Répertoires Manquants

```pascal
// Toujours vérifier/créer les répertoires nécessaires
if not DirectoryExists(MonRep) then
  ForceDirectories(MonRep);  // Crée toute la hiérarchie
```

---

## 12. Fonctions de Référence Rapide

| Fonction | Description | Exemple |
|----------|-------------|---------|
| `PathDelim` | Séparateur de chemins (`\` ou `/`) | `'data' + PathDelim + 'file.txt'` |
| `LineEnding` | Fin de ligne système | `'Ligne1' + LineEnding + 'Ligne2'` |
| `IncludeTrailingPathDelimiter()` | Ajoute `/` ou `\` à la fin | `IncludeTrailingPathDelimiter('data')` → `'data\'` |
| `ExcludeTrailingPathDelimiter()` | Enlève `/` ou `\` à la fin | `ExcludeTrailingPathDelimiter('data\')` → `'data'` |
| `ExtractFilePath()` | Extrait le chemin du répertoire | `ExtractFilePath('C:\data\file.txt')` → `'C:\data\'` |
| `ExtractFileName()` | Extrait le nom du fichier | `ExtractFileName('C:\data\file.txt')` → `'file.txt'` |
| `ExtractFileExt()` | Extrait l'extension | `ExtractFileExt('file.txt')` → `'.txt'` |
| `ChangeFileExt()` | Change l'extension | `ChangeFileExt('file.txt', '.pdf')` → `'file.pdf'` |
| `ExpandFileName()` | Convertit en chemin absolu | `ExpandFileName('file.txt')` → chemin complet |

---

## Conclusion

La gestion portable des chemins est **essentielle** pour créer des applications multi-plateformes. Heureusement, FreePascal rend cela très simple :

1. **Utilisez `PathDelim`** pour tous vos chemins
2. **Utilisez les fonctions** `IncludeTrailingPathDelimiter()`, `ExtractFilePath()`, etc.
3. **Testez régulièrement** sur Windows et Linux
4. **Évitez les chemins absolus** codés en dur

En suivant ces règles simples, votre code sera naturellement portable et fonctionnera sans modification sur toutes les plateformes supportées par FreePascal.

Dans le prochain chapitre (19.3), nous verrons les directives de compilation conditionnelle qui permettent de gérer les cas où vous avez vraiment besoin de code spécifique à une plateforme.

⏭️ [Directives de compilation conditionnelle {$IFDEF}](/19-developpement-multi-plateforme-pratique/03-directives-compilation-conditionnelle.md)
