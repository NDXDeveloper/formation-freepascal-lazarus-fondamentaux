{ ============================================================================
  Section 7.5 : Variables et procedures publiques/privees
  Description : Unite demonstrant l'encapsulation (compte en banque)
  Fichier source : 05-variables-procedures-publiques-privees.md
  ============================================================================ }
{$mode objfpc}{$H+}
unit CompteEnBanque;

interface

procedure Deposer(montant: Real);  
procedure Retirer(montant: Real);  
function ObtenirSolde: Real;  

implementation

var
  Solde: Real = 0;  // Variable PRIVÉE

procedure Deposer(montant: Real);  
begin  
  if montant > 0 then
    Solde := Solde + montant
  else
    WriteLn('Erreur : montant invalide');
end;

procedure Retirer(montant: Real);  
begin  
  if (montant > 0) and (montant <= Solde) then
    Solde := Solde - montant
  else
    WriteLn('Erreur : retrait impossible');
end;

function ObtenirSolde: Real;  
begin  
  Result := Solde;
end;

end.
