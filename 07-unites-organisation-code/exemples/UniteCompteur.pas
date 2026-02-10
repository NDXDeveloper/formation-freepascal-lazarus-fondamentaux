{ ============================================================================
  Section 7.5 : Variables et procedures publiques/privees
  Description : Unite avec variable privee et accesseurs publics
  Fichier source : 05-variables-procedures-publiques-privees.md
  ============================================================================ }
{$mode objfpc}{$H+}
unit UniteCompteur;

interface

procedure Incrementer;  
procedure Reinitialiser;  
function ObtenirValeur: Integer;  

implementation

var
  compteur: Integer = 0;  // Variable PRIVÉE - dans implementation

procedure Incrementer;  
begin  
  compteur := compteur + 1;
end;

procedure Reinitialiser;  
begin  
  compteur := 0;
end;

function ObtenirValeur: Integer;  
begin  
  Result := compteur;
end;

end.
