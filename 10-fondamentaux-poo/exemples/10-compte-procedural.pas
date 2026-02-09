{ ============================================================================
  Section 10.10 : Comparaison procedural vs objet
  Description : Gestion de comptes bancaires - approche procedurale
  Fichier source : 10-comparaison-procedural-vs-objet.md
  ============================================================================ }
program CompteProcedural;

{$mode objfpc}{$H+}

type
  TCompte = record
    NumeroCompte: string;
    Titulaire: string;
    Solde: Real;
  end;

  TArrayComptes = array of TCompte;

var
  Comptes: TArrayComptes;

procedure AjouterCompte(var Comptes: TArrayComptes; const Numero, Titulaire: string; SoldeInitial: Real);
var
  Index: Integer;
begin
  Index := Length(Comptes);
  SetLength(Comptes, Index + 1);
  Comptes[Index].NumeroCompte := Numero;
  Comptes[Index].Titulaire := Titulaire;
  Comptes[Index].Solde := SoldeInitial;
end;

function TrouverCompte(const Comptes: TArrayComptes; const Numero: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Comptes) do
    if Comptes[I].NumeroCompte = Numero then
    begin
      Result := I;
      Break;
    end;
end;

procedure Crediter(var Compte: TCompte; Montant: Real);
begin
  if Montant > 0 then
  begin
    Compte.Solde := Compte.Solde + Montant;
    WriteLn('Crédit de ', Montant:0:2, ' € effectué');
  end
  else
    WriteLn('Erreur: montant invalide');
end;

procedure Debiter(var Compte: TCompte; Montant: Real);
begin
  if Montant > 0 then
  begin
    if Compte.Solde >= Montant then
    begin
      Compte.Solde := Compte.Solde - Montant;
      WriteLn('Débit de ', Montant:0:2, ' € effectué');
    end
    else
      WriteLn('Erreur: solde insuffisant');
  end
  else
    WriteLn('Erreur: montant invalide');
end;

procedure AfficherCompte(const Compte: TCompte);
begin
  WriteLn('Compte: ', Compte.NumeroCompte);
  WriteLn('Titulaire: ', Compte.Titulaire);
  WriteLn('Solde: ', Compte.Solde:0:2, ' €');
end;

var
  Index: Integer;
begin
  // Création de comptes
  AjouterCompte(Comptes, 'FR001', 'Alice Martin', 1000);
  AjouterCompte(Comptes, 'FR002', 'Bob Durand', 500);

  // Opérations sur le premier compte
  Index := TrouverCompte(Comptes, 'FR001');
  if Index >= 0 then
  begin
    Crediter(Comptes[Index], 500);
    Debiter(Comptes[Index], 200);
    AfficherCompte(Comptes[Index]);
  end;

  SetLength(Comptes, 0);
end.
