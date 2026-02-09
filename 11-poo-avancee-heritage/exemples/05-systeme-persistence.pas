{ ============================================================================
  Section 11.5 : Methodes abstraites et classes abstraites
  Description : Systeme de persistence abstrait (fichier et memoire)
  Fichier source : 05-methodes-abstraites-classes-abstraites.md
  ============================================================================ }
program SystemePersistence;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  { Classe abstraite : Système de persistence }
  TPersistence = class
  public
    // Méthodes abstraites : toute implémentation DOIT les fournir
    function Sauvegarder(const Cle, Valeur: string): Boolean; virtual; abstract;
    function Charger(const Cle: string): string; virtual; abstract;
    function Supprimer(const Cle: string): Boolean; virtual; abstract;
    function Existe(const Cle: string): Boolean; virtual; abstract;

    // Méthode concrète : implémentation par défaut
    procedure AfficherStatut; virtual;
  end;

  { Persistence en fichier texte }
  TPersistenceFichier = class(TPersistence)
  private
    FNomFichier: string;
    FListe: TStringList;
  public
    constructor Create(ANomFichier: string);
    destructor Destroy; override;

    function Sauvegarder(const Cle, Valeur: string): Boolean; override;
    function Charger(const Cle: string): string; override;
    function Supprimer(const Cle: string): Boolean; override;
    function Existe(const Cle: string): Boolean; override;
    procedure AfficherStatut; override;
  end;

  { Persistence en mémoire }
  TPersistenceMemoire = class(TPersistence)
  private
    FDonnees: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function Sauvegarder(const Cle, Valeur: string): Boolean; override;
    function Charger(const Cle: string): string; override;
    function Supprimer(const Cle: string): Boolean; override;
    function Existe(const Cle: string): Boolean; override;
    procedure AfficherStatut; override;
  end;

{ === TPersistence === }

procedure TPersistence.AfficherStatut;
begin
  WriteLn('Système de persistence générique');
end;

{ === TPersistenceFichier === }

constructor TPersistenceFichier.Create(ANomFichier: string);
begin
  inherited Create;
  FNomFichier := ANomFichier;
  FListe := TStringList.Create;

  if FileExists(FNomFichier) then
  begin
    FListe.LoadFromFile(FNomFichier);
    WriteLn('✓ Fichier chargé : ', FNomFichier, ' (', FListe.Count, ' entrées)');
  end
  else
    WriteLn('→ Nouveau fichier : ', FNomFichier);
end;

destructor TPersistenceFichier.Destroy;
begin
  FListe.SaveToFile(FNomFichier);
  FListe.Free;
  inherited Destroy;
end;

function TPersistenceFichier.Sauvegarder(const Cle, Valeur: string): Boolean;
var
  Index: Integer;
begin
  Index := FListe.IndexOfName(Cle);

  if Index >= 0 then
    FListe.Values[Cle] := Valeur  // Mise à jour
  else
    FListe.Add(Cle + '=' + Valeur);  // Ajout

  FListe.SaveToFile(FNomFichier);
  Result := True;
end;

function TPersistenceFichier.Charger(const Cle: string): string;
begin
  Result := FListe.Values[Cle];
end;

function TPersistenceFichier.Supprimer(const Cle: string): Boolean;
var
  Index: Integer;
begin
  Index := FListe.IndexOfName(Cle);

  if Index >= 0 then
  begin
    FListe.Delete(Index);
    FListe.SaveToFile(FNomFichier);
    Result := True;
  end
  else
    Result := False;
end;

function TPersistenceFichier.Existe(const Cle: string): Boolean;
begin
  Result := FListe.IndexOfName(Cle) >= 0;
end;

procedure TPersistenceFichier.AfficherStatut;
begin
  WriteLn('📁 Persistence FICHIER');
  WriteLn('   Fichier : ', FNomFichier);
  WriteLn('   Entrées : ', FListe.Count);
end;

{ === TPersistenceMemoire === }

constructor TPersistenceMemoire.Create;
begin
  inherited Create;
  FDonnees := TStringList.Create;
  WriteLn('→ Persistence en mémoire créée');
end;

destructor TPersistenceMemoire.Destroy;
begin
  FDonnees.Free;
  inherited Destroy;
end;

function TPersistenceMemoire.Sauvegarder(const Cle, Valeur: string): Boolean;
var
  Index: Integer;
begin
  Index := FDonnees.IndexOfName(Cle);

  if Index >= 0 then
    FDonnees.Values[Cle] := Valeur
  else
    FDonnees.Add(Cle + '=' + Valeur);

  Result := True;
end;

function TPersistenceMemoire.Charger(const Cle: string): string;
begin
  Result := FDonnees.Values[Cle];
end;

function TPersistenceMemoire.Supprimer(const Cle: string): Boolean;
var
  Index: Integer;
begin
  Index := FDonnees.IndexOfName(Cle);

  if Index >= 0 then
  begin
    FDonnees.Delete(Index);
    Result := True;
  end
  else
    Result := False;
end;

function TPersistenceMemoire.Existe(const Cle: string): Boolean;
begin
  Result := FDonnees.IndexOfName(Cle) >= 0;
end;

procedure TPersistenceMemoire.AfficherStatut;
begin
  WriteLn('💾 Persistence MEMOIRE');
  WriteLn('   Entrées : ', FDonnees.Count);
end;

{ === Fonction polymorphe === }

procedure TesterPersistence(P: TPersistence; const Nom: string);
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   TEST : ', Nom);
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  P.AfficherStatut;
  WriteLn;

  // Sauvegarde
  WriteLn('→ Sauvegarde de données...');
  P.Sauvegarder('nom', 'Jean Dupont');
  P.Sauvegarder('email', 'jean@example.com');
  P.Sauvegarder('age', '35');
  WriteLn('✓ 3 entrées sauvegardées');
  WriteLn;

  // Lecture
  WriteLn('→ Lecture des données...');
  WriteLn('Nom : ', P.Charger('nom'));
  WriteLn('Email : ', P.Charger('email'));
  WriteLn('Age : ', P.Charger('age'));
  WriteLn;

  // Vérification
  WriteLn('→ Vérification d''existence...');
  WriteLn('Clé "nom" existe ? ', P.Existe('nom'));
  WriteLn('Clé "inexistante" existe ? ', P.Existe('inexistante'));
  WriteLn;

  // Suppression
  WriteLn('→ Suppression de "age"...');
  if P.Supprimer('age') then
    WriteLn('✓ Supprimé')
  else
    WriteLn('✗ Échec');
  WriteLn;

  // Statut final
  WriteLn('→ Statut final :');
  P.AfficherStatut;
  WriteLn;
end;

{ === Programme principal === }
var
  PersistenceFichier: TPersistenceFichier;
  PersistenceMemoire: TPersistenceMemoire;
begin
  WriteLn('═══════════════════════════════════════════════');
  WriteLn('   SYSTEME DE PERSISTENCE ABSTRAIT');
  WriteLn('═══════════════════════════════════════════════');
  WriteLn;

  // Test avec fichier
  PersistenceFichier := TPersistenceFichier.Create('donnees.txt');
  TesterPersistence(PersistenceFichier, 'FICHIER');
  PersistenceFichier.Free;

  // Test avec mémoire
  PersistenceMemoire := TPersistenceMemoire.Create;
  TesterPersistence(PersistenceMemoire, 'MEMOIRE');
  PersistenceMemoire.Free;

  WriteLn('═══════════════════════════════════════════════');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
