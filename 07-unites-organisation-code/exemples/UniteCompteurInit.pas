{ ============================================================================
  Section 7.6 : Sections initialization et finalization
  Description : Unite compteur avec initialization et finalization
  Fichier source : 06-sections-initialization-finalization.md
  Note : Renomme UniteCompteurInit pour eviter conflit avec UniteCompteur (section 05)
  ============================================================================ }
{$mode objfpc}{$H+}
unit UniteCompteurInit;

interface

function ObtenirCompteur: Integer;  
procedure Incrementer;  

implementation

var
  Compteur: Integer;

function ObtenirCompteur: Integer;  
begin  
  Result := Compteur;
end;

procedure Incrementer;  
begin  
  Inc(Compteur);
end;

initialization
  // Initialiser le compteur à 0 au démarrage
  Compteur := 0;
  WriteLn('UniteCompteur : Compteur initialisé à 0');

finalization
  // Afficher la valeur finale
  WriteLn('UniteCompteur : Valeur finale du compteur = ', Compteur);
  Flush(Output);  // Nécessaire en finalization : sans Flush, la sortie peut être perdue

end.
