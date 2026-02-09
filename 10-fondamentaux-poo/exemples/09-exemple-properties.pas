{ ============================================================================
  Section 10.9 : Proprietes (properties) simples
  Description : Classe TCompteBancaire avec proprietes, getters et setters
  Fichier source : 09-proprietes-properties-simples.md
  ============================================================================ }
program ExempleProperties;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TCompteBancaire = class
  private
    FNumeroCompte: string;
    FSolde: Real;
    FTitulaire: string;
    FTauxInteret: Real;
    FHistorique: array of string;

    // Getters
    function GetSoldeFormate: string;
    function GetInteretsAnnuels: Real;
    function GetNombreOperations: Integer;

    // Setters
    procedure SetTitulaire(const Valeur: string);
    procedure SetTauxInteret(Valeur: Real);

    // Méthode privée
    procedure AjouterHistorique(const Operation: string);

  public
    constructor Create(const NumeroCompte, Titulaire: string; SoldeInitial: Real);
    destructor Destroy; override;

    procedure Crediter(Montant: Real);
    procedure Debiter(Montant: Real);
    procedure AfficherHistorique;

    // Propriétés
    property NumeroCompte: string read FNumeroCompte;  // Lecture seule
    property Solde: Real read FSolde;                  // Lecture seule
    property SoldeFormate: string read GetSoldeFormate; // Calculee
    property Titulaire: string read FTitulaire write SetTitulaire;
    property TauxInteret: Real read FTauxInteret write SetTauxInteret;
    property InteretsAnnuels: Real read GetInteretsAnnuels;  // Calculee
    property NombreOperations: Integer read GetNombreOperations;  // Calculee
  end;

// === IMPLÉMENTATION ===

constructor TCompteBancaire.Create(const NumeroCompte, Titulaire: string; SoldeInitial: Real);
begin
  inherited Create;
  FNumeroCompte := NumeroCompte;
  FTitulaire := Titulaire;
  FSolde := SoldeInitial;
  FTauxInteret := 0.02;  // 2% par défaut
  SetLength(FHistorique, 0);
  AjouterHistorique('Ouverture du compte avec solde : ' + FloatToStr(SoldeInitial) + ' €');
end;

destructor TCompteBancaire.Destroy;
begin
  SetLength(FHistorique, 0);
  inherited Destroy;
end;

procedure TCompteBancaire.AjouterHistorique(const Operation: string);
var
  Index: Integer;
begin
  Index := Length(FHistorique);
  SetLength(FHistorique, Index + 1);
  FHistorique[Index] := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Operation;
end;

function TCompteBancaire.GetSoldeFormate: string;
begin
  Result := FormatFloat('#,##0.00', FSolde) + ' €';
end;

function TCompteBancaire.GetInteretsAnnuels: Real;
begin
  Result := FSolde * FTauxInteret;
end;

function TCompteBancaire.GetNombreOperations: Integer;
begin
  Result := Length(FHistorique);
end;

procedure TCompteBancaire.SetTitulaire(const Valeur: string);
begin
  if Length(Valeur) > 0 then
  begin
    FTitulaire := Valeur;
    AjouterHistorique('Changement de titulaire : ' + Valeur);
  end
  else
    WriteLn('Erreur : nom de titulaire invalide');
end;

procedure TCompteBancaire.SetTauxInteret(Valeur: Real);
begin
  if (Valeur >= 0) and (Valeur <= 0.10) then  // Max 10%
  begin
    FTauxInteret := Valeur;
    AjouterHistorique('Nouveau taux d''intérêt : ' + FloatToStr(Valeur * 100) + '%');
  end
  else
    WriteLn('Erreur : taux d''intérêt invalide (doit être entre 0 et 10%)');
end;

procedure TCompteBancaire.Crediter(Montant: Real);
begin
  if Montant > 0 then
  begin
    FSolde := FSolde + Montant;
    AjouterHistorique('Crédit de ' + FloatToStr(Montant) + ' €');
    WriteLn('Crédit effectué : ', Montant:0:2, ' €');
  end
  else
    WriteLn('Erreur : montant invalide');
end;

procedure TCompteBancaire.Debiter(Montant: Real);
begin
  if Montant > 0 then
  begin
    if FSolde >= Montant then
    begin
      FSolde := FSolde - Montant;
      AjouterHistorique('Débit de ' + FloatToStr(Montant) + ' €');
      WriteLn('Débit effectué : ', Montant:0:2, ' €');
    end
    else
      WriteLn('Erreur : solde insuffisant');
  end
  else
    WriteLn('Erreur : montant invalide');
end;

procedure TCompteBancaire.AfficherHistorique;
var
  I: Integer;
begin
  WriteLn('=== Historique du compte ', FNumeroCompte, ' ===');
  for I := 0 to High(FHistorique) do
    WriteLn(FHistorique[I]);
  WriteLn('==========================================');
end;

// === PROGRAMME PRINCIPAL ===

var
  Compte: TCompteBancaire;
begin
  WriteLn('=== Création du compte ===');
  Compte := TCompteBancaire.Create('FR123456789', 'Jean Dupont', 1000);
  WriteLn;

  WriteLn('=== Affichage des propriétés ===');
  WriteLn('Numéro : ', Compte.NumeroCompte);           // Lecture seule
  WriteLn('Titulaire : ', Compte.Titulaire);
  WriteLn('Solde : ', Compte.Solde:0:2, ' €');
  WriteLn('Solde formaté : ', Compte.SoldeFormate);    // Propriété calculée
  WriteLn('Taux d''intérêt : ', (Compte.TauxInteret * 100):0:2, '%');
  WriteLn('Intérêts annuels : ', Compte.InteretsAnnuels:0:2, ' €');  // Calculee
  WriteLn('Nombre d''opérations : ', Compte.NombreOperations);
  WriteLn;

  WriteLn('=== Opérations ===');
  Compte.Crediter(500);
  Compte.Debiter(200);
  WriteLn('Nouveau solde : ', Compte.SoldeFormate);
  WriteLn;

  WriteLn('=== Modification des propriétés ===');
  Compte.Titulaire := 'Marie Dupont';  // Utilise SetTitulaire avec validation
  Compte.TauxInteret := 0.03;          // 3%, utilise SetTauxInteret
  WriteLn('Nouveau titulaire : ', Compte.Titulaire);
  WriteLn('Nouveau taux : ', (Compte.TauxInteret * 100):0:2, '%');
  WriteLn('Nouveaux intérêts annuels : ', Compte.InteretsAnnuels:0:2, ' €');
  WriteLn;

  WriteLn('=== Tentative de modification invalide ===');
  Compte.TauxInteret := 0.15;  // Trop élevé, sera rejeté
  WriteLn;

  // Tentative d'écriture sur propriété en lecture seule (décommentez pour voir l'erreur)
  // Compte.Solde := 5000;  // ✗ ERREUR de compilation
  // Compte.NumeroCompte := 'AUTRE';  // ✗ ERREUR de compilation

  Compte.AfficherHistorique;
  WriteLn;

  Compte.Free;
  WriteLn('Programme terminé');
end.
