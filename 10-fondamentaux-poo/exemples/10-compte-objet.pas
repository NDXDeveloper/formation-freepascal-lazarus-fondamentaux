{ ============================================================================
  Section 10.10 : Comparaison procedural vs objet
  Description : Gestion de comptes bancaires - approche orientee objet
  Fichier source : 10-comparaison-procedural-vs-objet.md
  ============================================================================ }
program CompteObjet;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TCompte = class
  private
    FNumeroCompte: string;
    FTitulaire: string;
    FSolde: Real;
  public
    constructor Create(const Numero, Titulaire: string; SoldeInitial: Real);
    procedure Crediter(Montant: Real);
    procedure Debiter(Montant: Real);
    procedure Afficher;
    property NumeroCompte: string read FNumeroCompte;
    property Titulaire: string read FTitulaire;
    property Solde: Real read FSolde;
  end;

  TBanque = class
  private
    FComptes: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterCompte(Compte: TCompte);
    function TrouverCompte(const Numero: string): TCompte;
  end;

// Implémentation TCompte

constructor TCompte.Create(const Numero, Titulaire: string; SoldeInitial: Real);  
begin  
  inherited Create;
  FNumeroCompte := Numero;
  FTitulaire := Titulaire;
  FSolde := SoldeInitial;
end;

procedure TCompte.Crediter(Montant: Real);  
begin  
  if Montant > 0 then
  begin
    FSolde := FSolde + Montant;
    WriteLn('Crédit de ', Montant:0:2, ' € effectué');
  end
  else
    WriteLn('Erreur: montant invalide');
end;

procedure TCompte.Debiter(Montant: Real);  
begin  
  if Montant > 0 then
  begin
    if FSolde >= Montant then
    begin
      FSolde := FSolde - Montant;
      WriteLn('Débit de ', Montant:0:2, ' € effectué');
    end
    else
      WriteLn('Erreur: solde insuffisant');
  end
  else
    WriteLn('Erreur: montant invalide');
end;

procedure TCompte.Afficher;  
begin  
  WriteLn('Compte: ', FNumeroCompte);
  WriteLn('Titulaire: ', FTitulaire);
  WriteLn('Solde: ', FSolde:0:2, ' €');
end;

// Implémentation TBanque

constructor TBanque.Create;  
begin  
  inherited Create;
  FComptes := TList.Create;
end;

destructor TBanque.Destroy;  
var  
  I: Integer;
begin
  for I := 0 to FComptes.Count - 1 do
    TCompte(FComptes[I]).Free;  // TList stocke des Pointer : cast nécessaire pour accéder aux méthodes
  FComptes.Free;
  inherited Destroy;
end;

procedure TBanque.AjouterCompte(Compte: TCompte);  
begin  
  FComptes.Add(Compte);
end;

function TBanque.TrouverCompte(const Numero: string): TCompte;  
var  
  I: Integer;
begin
  Result := nil;
  for I := 0 to FComptes.Count - 1 do
    if TCompte(FComptes[I]).NumeroCompte = Numero then
    begin
      Result := TCompte(FComptes[I]);
      Break;
    end;
end;

// Programme principal

var
  Banque: TBanque;
  Compte: TCompte;
begin
  Banque := TBanque.Create;
  try
    // Création de comptes
    Banque.AjouterCompte(TCompte.Create('FR001', 'Alice Martin', 1000));
    Banque.AjouterCompte(TCompte.Create('FR002', 'Bob Durand', 500));

    // Opérations
    Compte := Banque.TrouverCompte('FR001');
    if Compte <> nil then
    begin
      Compte.Crediter(500);
      Compte.Debiter(200);
      Compte.Afficher;
    end;
  finally
    Banque.Free;
  end;
end.
