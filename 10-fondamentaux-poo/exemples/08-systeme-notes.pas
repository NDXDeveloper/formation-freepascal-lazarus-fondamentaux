{ ============================================================================
  Section 10.8 : Visibilite : private, protected, public, published
  Description : Systeme de notes d'etudiants avec heritage et niveaux de visibilite
  Fichier source : 08-visibilite-private-protected-public-published.md
  ============================================================================ }
program SystemeNotes;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils;

type
  // Classe de base
  TPersonne = class
  protected
    // Protected : les classes dérivées en auront besoin
    FNom: string;
    FPrenom: string;
    FDateNaissance: TDateTime;
  public
    constructor Create(const Nom, Prenom: string; DateNaissance: TDateTime);
    function ObtenirNomComplet: string;
    function CalculerAge: Integer;
    procedure Afficher; virtual;  // virtual : cette méthode peut être redéfinie par les classes dérivées
  end;

  // Classe dérivée
  TEtudiant = class(TPersonne)
  private
    // Private : détails internes de TEtudiant
    FNumeroEtudiant: string;
    FNotes: array of Real;

    // Méthodes privées
    function NotesValidees: Boolean;
    procedure TrierNotes;

  protected
    // Protected : au cas où on dérive encore (TDoctorant, TMaster, etc.)
    FFormation: string;

  public
    constructor Create(const Nom, Prenom: string; DateNaissance: TDateTime; const NumeroEtudiant: string);
    destructor Destroy; override;

    // Interface publique
    procedure AjouterNote(Note: Real);
    function CalculerMoyenne: Real;
    function ObtenirMeilleureNote: Real;
    function ObtenirPireNote: Real;
    function ObtenirNombreNotes: Integer;
    procedure AfficherNotes;
    procedure Afficher; override;  // override : redéfinit la méthode virtual du parent
  end;

// === IMPLÉMENTATION TPersonne ===

constructor TPersonne.Create(const Nom, Prenom: string; DateNaissance: TDateTime);  
begin  
  inherited Create;
  FNom := Nom;
  FPrenom := Prenom;
  FDateNaissance := DateNaissance;
end;

function TPersonne.ObtenirNomComplet: string;  
begin  
  Result := FPrenom + ' ' + FNom;
end;

function TPersonne.CalculerAge: Integer;  
begin  
  Result := YearsBetween(Now, FDateNaissance);
end;

procedure TPersonne.Afficher;  
begin  
  WriteLn('Nom : ', ObtenirNomComplet);
  WriteLn('Age : ', CalculerAge, ' ans');
end;

// === IMPLÉMENTATION TEtudiant ===

constructor TEtudiant.Create(const Nom, Prenom: string; DateNaissance: TDateTime; const NumeroEtudiant: string);  
begin  
  inherited Create(Nom, Prenom, DateNaissance);
  FNumeroEtudiant := NumeroEtudiant;
  SetLength(FNotes, 0);
  FFormation := 'Non définie';
end;

destructor TEtudiant.Destroy;  
begin  
  SetLength(FNotes, 0);
  inherited Destroy;
end;

function TEtudiant.NotesValidees: Boolean;  
var  
  I: Integer;
begin
  Result := True;
  for I := 0 to High(FNotes) do
    if (FNotes[I] < 0) or (FNotes[I] > 20) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TEtudiant.TrierNotes;  
var  
  I, J: Integer;
  Temp: Real;
begin
  for I := 0 to High(FNotes) - 1 do
    for J := I + 1 to High(FNotes) do
      if FNotes[I] > FNotes[J] then
      begin
        Temp := FNotes[I];
        FNotes[I] := FNotes[J];
        FNotes[J] := Temp;
      end;
end;

procedure TEtudiant.AjouterNote(Note: Real);  
var  
  Index: Integer;
begin
  if (Note >= 0) and (Note <= 20) then
  begin
    Index := Length(FNotes);
    SetLength(FNotes, Index + 1);
    FNotes[Index] := Note;
    WriteLn('Note ajoutée : ', Note:0:2);
  end
  else
    WriteLn('Erreur : note invalide (doit être entre 0 et 20)');
end;

function TEtudiant.CalculerMoyenne: Real;  
var  
  I: Integer;
  Somme: Real;
begin
  if Length(FNotes) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Somme := 0;
  for I := 0 to High(FNotes) do
    Somme := Somme + FNotes[I];

  Result := Somme / Length(FNotes);
end;

function TEtudiant.ObtenirMeilleureNote: Real;  
var  
  I: Integer;
begin
  if Length(FNotes) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := FNotes[0];
  for I := 1 to High(FNotes) do
    if FNotes[I] > Result then
      Result := FNotes[I];
end;

function TEtudiant.ObtenirPireNote: Real;  
var  
  I: Integer;
begin
  if Length(FNotes) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := FNotes[0];
  for I := 1 to High(FNotes) do
    if FNotes[I] < Result then
      Result := FNotes[I];
end;

function TEtudiant.ObtenirNombreNotes: Integer;  
begin  
  Result := Length(FNotes);
end;

procedure TEtudiant.AfficherNotes;  
var  
  I: Integer;
begin
  WriteLn('=== Notes de ', ObtenirNomComplet, ' ===');

  if Length(FNotes) = 0 then
  begin
    WriteLn('Aucune note enregistrée');
    Exit;
  end;

  for I := 0 to High(FNotes) do
    WriteLn('Note ', I + 1, ' : ', FNotes[I]:0:2);

  WriteLn('---');
  WriteLn('Nombre de notes : ', ObtenirNombreNotes);
  WriteLn('Moyenne : ', CalculerMoyenne:0:2);
  WriteLn('Meilleure note : ', ObtenirMeilleureNote:0:2);
  WriteLn('Pire note : ', ObtenirPireNote:0:2);
  WriteLn('==================');
end;

procedure TEtudiant.Afficher;  
begin  
  WriteLn('=== Étudiant ===');
  inherited Afficher;  // Affiche nom et âge
  WriteLn('Numéro étudiant : ', FNumeroEtudiant);
  WriteLn('Formation : ', FFormation);
  WriteLn('Nombre de notes : ', ObtenirNombreNotes);
  if ObtenirNombreNotes > 0 then
    WriteLn('Moyenne générale : ', CalculerMoyenne:0:2);
  WriteLn('================');
end;

// === PROGRAMME PRINCIPAL ===

var
  Etudiant: TEtudiant;
begin
  Etudiant := TEtudiant.Create('Dupont', 'Marie', EncodeDate(2003, 5, 15), 'E2024001');
  try
    Etudiant.FFormation := 'Informatique';  // Protected, accessible ici car même unité

    Etudiant.Afficher;
    WriteLn;

    WriteLn('--- Ajout de notes ---');
    Etudiant.AjouterNote(15.5);
    Etudiant.AjouterNote(18);
    Etudiant.AjouterNote(12.5);
    Etudiant.AjouterNote(16);
    WriteLn;

    Etudiant.AfficherNotes;
    WriteLn;

    Etudiant.Afficher;

    // Tentatives d'accès incorrect (décommentez pour voir les erreurs)
    // WriteLn(Etudiant.FNumeroEtudiant);  // ✗ ERREUR : private
    // WriteLn(Etudiant.FNotes[0]);         // ✗ ERREUR : private

  finally
    Etudiant.Free;
  end;
end.
