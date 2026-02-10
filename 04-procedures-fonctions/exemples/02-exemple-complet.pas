{ ============================================================================
  Section 4.2 : Declaration et appel
  Description : Exemple complet combinant procedures et fonctions
  Fichier source : 02-declaration-appel.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ExempleComplet;

// Déclaration d'une procédure
procedure AfficherSeparateur;  
begin  
  WriteLn('====================');
end;

// Déclaration d'une fonction
function ObtenirAnneeActuelle: Integer;  
begin  
  Result := 2025;
end;

// Déclaration d'une fonction avec calcul
function CalculerDoubleAnnee: Integer;  
var  
  annee: Integer;  // Variable locale
begin
  annee := ObtenirAnneeActuelle;
  Result := annee * 2;
end;

// Programme principal
var
  resultat: Integer;
begin
  AfficherSeparateur;
  WriteLn('Année actuelle : ', ObtenirAnneeActuelle);

  resultat := CalculerDoubleAnnee;
  WriteLn('Le double : ', resultat);

  AfficherSeparateur;
end.
