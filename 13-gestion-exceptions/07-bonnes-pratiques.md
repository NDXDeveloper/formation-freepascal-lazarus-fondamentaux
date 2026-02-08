🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.7 Bonnes pratiques

## Introduction

Maintenant que nous maîtrisons les mécanismes des exceptions, voyons comment les utiliser efficacement. Les bonnes pratiques présentées ici sont le fruit de décennies d'expérience collective de la communauté de développeurs. Elles vous aideront à créer des applications robustes, maintenables et professionnelles.

## Principe 1 : Lever des exceptions pour les situations exceptionnelles

### Ce qui est exceptionnel

Une exception doit représenter une situation **anormale** et **inattendue** qui empêche le programme de continuer normalement.

#### ✓ Situations exceptionnelles (lever une exception)

```pascal
// Fichier obligatoire manquant
if not FileExists('config.ini') then
  raise Exception.Create('Fichier de configuration manquant');

// Données corrompues
if not ValiderIntegrite(donnees) then
  raise Exception.Create('Données corrompues détectées');

// Violation d'une règle métier critique
if montantRetrait > soldeMensuelAutorise then
  raise ELimiteDepassee.Create('Limite mensuelle dépassée');

// Ressource système indisponible
if not ConnecterServeur(adresse) then
  raise Exception.Create('Serveur inaccessible');
```

#### ✗ Situations normales (NE PAS lever d'exception)

```pascal
// ✗ MAUVAIS : Recherche qui ne trouve rien (normal)
function TrouverUtilisateur(id: Integer): TUtilisateur;
begin
  Result := BaseDonnees.Chercher(id);
  if Result = nil then
    raise Exception.Create('Utilisateur non trouvé');  // ✗ Non !
end;

// ✓ BON : Retourner un indicateur
function TrouverUtilisateur(id: Integer; out utilisateur: TUtilisateur): Boolean;
begin
  utilisateur := BaseDonnees.Chercher(id);
  Result := Assigned(utilisateur);
end;

// ✗ MAUVAIS : Validation d'entrée utilisateur
procedure TraiterFormulaire;
begin
  if EditNom.Text = '' then
    raise Exception.Create('Nom requis');  // ✗ L'utilisateur peut oublier
end;

// ✓ BON : Message d'erreur simple
procedure TraiterFormulaire;
begin
  if EditNom.Text = '' then
  begin
    ShowMessage('Veuillez remplir le nom');
    EditNom.SetFocus;
    Exit;
  end;
end;
```

### Règle d'or

**Si vous pouvez vérifier une condition avant d'appeler une fonction, faites-le plutôt que de compter sur une exception.**

```pascal
// ✗ Utilisation d'exception pour le flux normal
try
  resultat := Diviser(a, b);
except
  on E: EDivByZero do
    resultat := 0;
end;

// ✓ Vérification préalable
if b <> 0 then
  resultat := a div b
else
  resultat := 0;
```

## Principe 2 : Messages d'erreur clairs et utiles

### Anatomie d'un bon message d'erreur

Un bon message d'erreur répond à trois questions :

1. **Quoi ?** Qu'est-ce qui s'est passé ?
2. **Pourquoi ?** Pourquoi cela a-t-il échoué ?
3. **Comment ?** Que peut faire l'utilisateur ?

#### ✗ Messages vagues

```pascal
raise Exception.Create('Erreur');
raise Exception.Create('Échec');
raise Exception.Create('Impossible');
raise Exception.Create('Erreur dans le traitement');
```

#### ✓ Messages informatifs

```pascal
raise Exception.Create(
  'Impossible d''ouvrir le fichier "config.xml" : ' +
  'le fichier n''existe pas. Vérifiez que l''installation est complète.'
);

raise Exception.CreateFmt(
  'Le compte %s ne peut pas être débité de %.2f€ : ' +
  'solde actuel %.2f€. Veuillez effectuer un dépôt.',
  [numeroCompte, montant, solde]
);

raise Exception.Create(
  'La connexion au serveur a échoué (timeout après 30 secondes). ' +
  'Vérifiez votre connexion internet et réessayez.'
);
```

### Adapter le message au public

#### Pour les développeurs (logs, débogage)

```pascal
raise Exception.CreateFmt(
  'SQLException in ExecuteQuery: table "users" does not exist [%s:%d]',
  [UnitName, LineNumber]
);
```

#### Pour les utilisateurs finaux

```pascal
raise Exception.Create(
  'Les données n''ont pas pu être enregistrées. ' +
  'Veuillez réessayer ou contacter le support technique.'
);
```

### Messages multilingues

Pour une application internationale :

```pascal
// Définir les constantes
resourcestring
  RS_FileNotFound = 'Le fichier "%s" est introuvable';
  RS_InvalidEmail = 'L''adresse email n''est pas valide';
  RS_ConnectionFailed = 'Connexion au serveur impossible';

// Utilisation
if not FileExists(fichier) then
  raise Exception.CreateFmt(RS_FileNotFound, [fichier]);
```

## Principe 3 : Capturer au bon niveau

### Ne capturez que ce que vous pouvez gérer

Ne capturez pas une exception si vous ne savez pas quoi en faire.

#### ✗ Capture inutile

```pascal
procedure NiveauBas;
begin
  try
    TraiterDonnees;
  except
    raise;  // ✗ Pourquoi capturer si on re-lève immédiatement ?
  end;
end;
```

#### ✓ Capture avec valeur ajoutée

```pascal
procedure NiveauBas;
begin
  try
    TraiterDonnees;
  except
    on E: Exception do
    begin
      LoggerErreur('NiveauBas', E.Message);  // ✓ Logging utile
      raise;  // Puis propagation
    end;
  end;
end;
```

### Capturer tôt vs capturer tard

**Capturer tard** (au niveau de l'interface utilisateur) :

```pascal
// Niveau bas : laisse propager
function LireFichierConfig: String;
begin
  Result := LireContenuFichier('config.ini');  // Peut lever exception
end;

// Niveau moyen : laisse propager
procedure InitialiserApplication;
begin
  Configuration := LireFichierConfig;  // Peut lever exception
end;

// Niveau haut : capture et informe l'utilisateur
procedure FormCreate(Sender: TObject);
begin
  try
    InitialiserApplication;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur d''initialisation : ' + E.Message);
      Application.Terminate;
    end;
  end;
end;
```

**Avantage :** Le code métier reste propre, la gestion utilisateur est centralisée.

## Principe 4 : Ordre de capture spécifique → général

Nous l'avons déjà vu, mais c'est crucial : toujours capturer du plus spécifique au plus général.

```pascal
// ✓ CORRECT
try
  TraiterDonnees;
except
  on E: EFOpenError do      // Plus spécifique
    GererFichierManquant;
  on E: EInOutError do                  // Moins spécifique
    GererErreurIO;
  on E: Exception do                    // Plus général
    GererErreurGenerique;
end;

// ✗ INCORRECT
try
  TraiterDonnees;
except
  on E: Exception do                    // ✗ Capture tout !
    GererErreurGenerique;
  on E: EFOpenError do      // ✗ Jamais atteint
    GererFichierManquant;
end;
```

## Principe 5 : Ne jamais avaler les exceptions silencieusement

L'un des pires anti-patterns est de capturer une exception sans rien faire.

### ✗ L'exception invisible

```pascal
// ✗ TRÈS MAUVAIS : masque tous les problèmes
try
  OperationCritique;
except
  // Rien ! L'erreur disparaît sans trace
end;

// ✗ MAUVAIS : commentaire ne suffit pas
try
  OperationCritique;
except
  // TODO : gérer les erreurs
end;
```

### ✓ Au minimum, logger

```pascal
// ✓ ACCEPTABLE : au moins on sait qu'il y a eu un problème
try
  OperationCritique;
except
  on E: Exception do
    LoggerErreur('OperationCritique a échoué : ' + E.Message);
end;

// ✓ MIEUX : logger et informer
try
  OperationCritique;
except
  on E: Exception do
  begin
    LoggerErreur('OperationCritique a échoué : ' + E.Message);
    ShowMessage('Une erreur s''est produite. Détails enregistrés.');
  end;
end;
```

### Exceptions intentionnellement ignorées

Si vous devez vraiment ignorer une exception (rare), documentez pourquoi :

```pascal
try
  SupprimerFichierTemporaire(fichierTemp);
except
  on E: EInOutError do
    // Ignoré intentionnellement : le fichier temp sera nettoyé
    // au prochain redémarrage par le système d'exploitation
    ;
end;
```

## Principe 6 : Libérer les ressources correctement

Nous l'avons vu en détail, mais rappelons l'essentiel :

```pascal
// Pattern fondamental
Ressource := AcquerirRessource;
try
  UtiliserRessource(Ressource);
finally
  LibererRessource(Ressource);
end;
```

**Toujours** utiliser `try-finally` pour les ressources, même si vous avez aussi un `try-except`.

## Principe 7 : Documenter les exceptions levées

Documentez les exceptions que vos fonctions peuvent lever :

```pascal
/// Lit et retourne le contenu d'un fichier
/// @param nomFichier Le chemin du fichier à lire
/// @returns Le contenu du fichier
/// @raises EFOpenError si le fichier n'existe pas
/// @raises EInOutError si le fichier ne peut pas être lu
function LireFichier(const nomFichier: String): String;
```

Pour les fonctions publiques d'une bibliothèque, cette documentation est essentielle.

## Principe 8 : Exceptions et performance

### Les exceptions sont coûteuses

Lever et capturer une exception est environ **100 à 1000 fois plus lent** qu'un simple `if`.

#### ✗ Utilisation abusive pour le flux de contrôle

```pascal
// ✗ TRÈS MAUVAIS : exception dans une boucle
for i := 0 to 999999 do
begin
  try
    Traiter(tableau[i]);
  except
    Continue;  // Ignorer et continuer
  end;
end;
```

#### ✓ Vérification préalable

```pascal
// ✓ BON : vérification simple
for i := 0 to 999999 do
begin
  if EstValide(tableau[i]) then
    Traiter(tableau[i]);
end;
```

### Mais ne pas optimiser prématurément

Ne sacrifiez pas la robustesse pour la performance sans mesurer :

```pascal
// Si cette fonction est appelée 10 fois par seconde (pas 10000),
// la robustesse est plus importante que les microsecondes gagnées
function ChargerConfiguration: TConfiguration;
begin
  if not FileExists('config.xml') then
    raise EFOpenError.Create('Configuration manquante');

  Result := TConfiguration.Create;
  try
    Result.LoadFromFile('config.xml');
  except
    Result.Free;
    raise;
  end;
end;
```

## Principe 9 : Exceptions dans les constructeurs

Les constructeurs ont un comportement particulier avec les exceptions.

### Si un constructeur lève une exception

**Important :** En FreePascal/Delphi, si un constructeur lève une exception, le destructeur `Destroy` **EST automatiquement appelé** pour nettoyer l'objet partiellement créé. C'est pourquoi le destructeur doit être capable de gérer un objet incomplètement initialisé.

```pascal
constructor TMonObjet.Create;
begin
  inherited Create;
  FListe := TStringList.Create;
  FListe.LoadFromFile('obligatoire.txt');  // Si exception → Destroy est appelé
end;

destructor TMonObjet.Destroy;
begin
  FListe.Free;  // Free vérifie nil, donc sûr même si FListe n'a pas été créé
  inherited Destroy;
end;
```

**Attention au piège de la double libération :** Ne libérez pas un champ dans le constructeur ET dans le destructeur, sinon vous aurez un double-free.

```pascal
// ✗ MAUVAIS : double-free si LoadFromFile échoue
constructor TMonObjet.Create;
begin
  inherited Create;
  FListe := TStringList.Create;
  try
    FListe.LoadFromFile('obligatoire.txt');
  except
    FListe.Free;  // Libère ici...
    raise;        // Destroy sera appelé → FListe.Free à nouveau = crash !
  end;
end;
```

### Utilisation

```pascal
var
  obj: TMonObjet;
begin
  obj := nil;  // ✓ Initialiser à nil avant le try
  try
    obj := TMonObjet.Create;  // Peut lever exception
    obj.Travailler;
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;

  // Si Create a échoué, obj est resté nil (Destroy a été appelé en interne)
  // Si Create a réussi et Travailler a échoué, obj existe et doit être libéré
  obj.Free;  // Free vérifie nil, donc sûr dans les deux cas
end;
```

## Principe 10 : Exceptions dans les destructeurs

**Règle absolue : NE JAMAIS lever d'exception dans un destructeur.**

### ✗ Très dangereux

```pascal
destructor TMonObjet.Destroy;
begin
  FListe.SaveToFile('sauvegarde.txt');  // ✗ Peut lever exception !
  inherited Destroy;
end;
```

**Pourquoi ?** Le destructeur peut être appelé pendant la gestion d'une autre exception, causant des comportements imprévisibles.

### ✓ Gestion sécurisée

```pascal
destructor TMonObjet.Destroy;
begin
  try
    if Assigned(FListe) then
      FListe.SaveToFile('sauvegarde.txt');
  except
    // Logger mais ne pas propager
    on E: Exception do
      LoggerErreur('Erreur dans destructeur : ' + E.Message);
  end;

  FreeAndNil(FListe);
  inherited Destroy;
end;
```

## Principe 11 : Exceptions dans les callbacks et événements

Les événements (handlers) ne devraient généralement pas laisser les exceptions se propager.

### ✗ Laisser propager

```pascal
procedure TForm1.ButtonClick(Sender: TObject);
begin
  TraiterDonnees;  // ✗ Si exception, elle remonte dans la LCL
end;
```

### ✓ Capturer et gérer

```pascal
procedure TForm1.ButtonClick(Sender: TObject);
begin
  try
    TraiterDonnees;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur : ' + E.Message);
      LoggerErreur(E);
    end;
  end;
end;
```

**Pourquoi ?** Les frameworks UI ne savent généralement pas comment gérer vos exceptions métier. Capturez-les dans vos handlers.

## Principe 12 : Exceptions et transactions

Pour les opérations de base de données, combinez exceptions et transactions :

```pascal
procedure TransfererArgent(deCompte, versCompte: String; montant: Double);
begin
  Connexion.StartTransaction;
  try
    DebiterCompte(deCompte, montant);
    CrediterCompte(versCompte, montant);
    Connexion.Commit;  // Valider si tout OK
  except
    on E: Exception do
    begin
      Connexion.Rollback;  // Annuler en cas d'erreur
      LoggerErreur('Transfert échoué : ' + E.Message);
      raise;  // Propager l'exception
    end;
  end;
end;
```

**Pattern :** Commit si succès, Rollback si exception.

## Principe 13 : Créer une hiérarchie d'exceptions cohérente

Pour une application complexe, organisez vos exceptions :

```pascal
type
  // Base de l'application
  EMonApplication = class(Exception);

  // Par domaine
  EValidation = class(EMonApplication);
  EBusiness = class(EMonApplication);
  ETechnique = class(EMonApplication);

  // Spécifiques
  EChampRequis = class(EValidation);
  EFormatInvalide = class(EValidation);

  ESoldeInsuffisant = class(EBusiness);
  ECompteBloque = class(EBusiness);

  EConnexionDB = class(ETechnique);
  EFichierManquant = class(ETechnique);
```

**Avantage :** Capture flexible par catégorie ou spécifique.

## Principe 14 : Exceptions et tests

Testez que vos fonctions lèvent bien les exceptions attendues :

```pascal
procedure TesterValidation;
var
  exceptionLevee: Boolean;
begin
  exceptionLevee := False;
  try
    ValiderEmail('pas-un-email');  // Devrait lever exception
  except
    on E: EFormatInvalide do
      exceptionLevee := True;
  end;

  if not exceptionLevee then
    raise Exception.Create('Test échoué : exception non levée');

  WriteLn('Test réussi : exception correctement levée');
end;
```

## Principe 15 : Logging structuré

Loggez les exceptions de manière structurée pour faciliter le diagnostic :

```pascal
procedure LoggerException(const contexte: String; E: Exception);
begin
  LogMessage(Format(
    '[ERREUR] %s - Type: %s - Message: %s - Date: %s',
    [contexte, E.ClassName, E.Message, DateTimeToStr(Now)]
  ));
end;

// Utilisation
try
  TraiterFichier(nom);
except
  on E: Exception do
  begin
    LoggerException('TraiterFichier(' + nom + ')', E);
    raise;
  end;
end;
```

## Checklist des bonnes pratiques

Avant de valider votre code, vérifiez :

### Lever des exceptions
- [ ] Je lève des exceptions uniquement pour des situations exceptionnelles
- [ ] Mes messages d'erreur sont clairs et utiles
- [ ] J'utilise des classes d'exceptions appropriées
- [ ] Je documente les exceptions que mes fonctions publiques peuvent lever

### Capturer des exceptions
- [ ] Je capture au bon niveau (là où je peux gérer)
- [ ] Je capture du plus spécifique au plus général
- [ ] Je ne capture pas d'exceptions silencieusement
- [ ] Je libère toujours les ressources avec `try-finally`

### Constructeurs et destructeurs
- [ ] Mes constructeurs nettoient si exception
- [ ] Mes destructeurs ne lèvent JAMAIS d'exceptions

### Événements et callbacks
- [ ] Mes handlers d'événements capturent leurs exceptions
- [ ] Je n'utilise pas les exceptions pour le flux de contrôle normal

### Organisation
- [ ] Mes exceptions sont organisées en hiérarchie cohérente
- [ ] Je logue les exceptions importantes
- [ ] J'ai des tests pour vérifier les exceptions

## Anti-patterns à éviter

### 1. Le Pokemon Handler (catch 'em all)

```pascal
// ✗ Capture tout sans discrimination
try
  ToutFaire;
except
  // On verra plus tard...
end;
```

### 2. L'exception bavarde

```pascal
// ✗ Exceptions dans une boucle
for i := 0 to 1000000 do
begin
  try
    Traiter(i);
  except
    Continue;
  end;
end;
```

### 3. La poupée russe

```pascal
// ✗ Try-catch imbriqués à l'excès
try
  try
    try
      try
        Operation;
      except
      end;
    except
    end;
  except
  end;
except
end;
```

### 4. Le menteur

```pascal
// ✗ Message d'erreur inexact
try
  SauvegarderFichier(nom);
except
  ShowMessage('Fichier sauvegardé avec succès !');  // ✗ Mensonge !
end;
```

### 5. L'amnésique

```pascal
// ✗ Perte d'information
try
  OperationComplexe;
except
  raise Exception.Create('Erreur');  // ✗ Message d'origine perdu
end;
```

## Exemple complet de bonnes pratiques

```pascal
unit BonnesPratiquesDemo;

interface

type
  // Hiérarchie d'exceptions claire
  EApplicationException = class(Exception);
  EValidationException = class(EApplicationException);
  EBusinessException = class(EApplicationException);

  TProcessor = class
  private
    FConnexion: TSQLConnection;
    FLog: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    /// Traite un fichier de données
    /// @raises EValidationException si le fichier est invalide
    /// @raises EBusinessException si les règles métier sont violées
    /// @raises EInOutError si le fichier ne peut pas être lu
    procedure TraiterFichier(const nomFichier: String);
  end;

implementation

constructor TProcessor.Create;
begin
  inherited Create;

  FLog := TStringList.Create;
  FConnexion := TSQLConnection.Create(nil);

  try
    FConnexion.DatabaseName := 'mabase';
    FConnexion.Open;
  except
    on E: Exception do
    begin
      // Nettoyer ce qui a été créé
      FreeAndNil(FLog);
      FreeAndNil(FConnexion);
      raise;  // Propager
    end;
  end;
end;

destructor TProcessor.Destroy;
begin
  try
    // Fermer proprement la connexion
    if Assigned(FConnexion) and FConnexion.Connected then
      FConnexion.Close;
  except
    // Ne jamais lever d'exception dans un destructeur
    on E: Exception do
      // Logger seulement
      if Assigned(FLog) then
        FLog.Add('Erreur fermeture : ' + E.Message);
  end;

  FreeAndNil(FConnexion);
  FreeAndNil(FLog);

  inherited Destroy;
end;

procedure TProcessor.TraiterFichier(const nomFichier: String);
var
  fichier: TextFile;
  ligne: String;
begin
  // Validation préalable
  if not FileExists(nomFichier) then
    raise EValidationException.CreateFmt(
      'Le fichier "%s" n''existe pas',
      [nomFichier]
    );

  AssignFile(fichier, nomFichier);
  Reset(fichier);

  try
    FConnexion.StartTransaction;
    try
      while not EOF(fichier) do
      begin
        ReadLn(fichier, ligne);

        // Validation
        if Trim(ligne) = '' then
          raise EValidationException.Create('Ligne vide détectée');

        // Traitement
        TraiterLigne(ligne);
      end;

      FConnexion.Commit;
      FLog.Add('Fichier traité avec succès : ' + nomFichier);

    except
      on E: Exception do
      begin
        // Rollback en cas d'erreur
        FConnexion.Rollback;

        // Logger avec contexte
        FLog.Add(Format(
          'Erreur traitement fichier %s : [%s] %s',
          [nomFichier, E.ClassName, E.Message]
        ));

        // Propager
        raise;
      end;
    end;
  finally
    CloseFile(fichier);  // Toujours fermer
  end;
end;

end.
```

## Conclusion

Les bonnes pratiques de gestion des exceptions se résument en quelques principes simples :

1. **Lever** pour l'exceptionnel, pas pour le normal
2. **Messages** clairs et actionnables
3. **Capturer** au bon niveau, du spécifique au général
4. **Libérer** les ressources avec `try-finally`
5. **Ne jamais** ignorer silencieusement
6. **Documenter** ce qui peut être levé
7. **Tester** que les exceptions sont bien levées

En suivant ces pratiques, vous créerez des applications robustes, maintenables et professionnelles. Les exceptions ne sont pas vos ennemies : bien utilisées, elles sont vos alliées pour gérer l'imprévu avec élégance.

---

**Points clés à retenir :**

- Les exceptions sont pour les situations exceptionnelles, pas le flux normal
- Messages d'erreur : répondre à "Quoi ?", "Pourquoi ?", "Comment ?"
- Capturer uniquement ce qu'on peut gérer, au bon niveau
- Toujours capturer du plus spécifique au plus général
- Ne JAMAIS avaler les exceptions silencieusement
- Toujours libérer les ressources avec `try-finally`
- Documenter les exceptions dans les API publiques
- Ne pas lever d'exceptions dans les destructeurs
- Logger les exceptions pour le diagnostic
- Créer une hiérarchie d'exceptions cohérente
- Tester que les exceptions appropriées sont levées

⏭️ [Débogage avec exceptions](/13-gestion-exceptions/08-debogage-avec-exceptions.md)
