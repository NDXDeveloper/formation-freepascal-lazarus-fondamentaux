{ ============================================================================
  Section 7.5 : Variables et procedures publiques/privees
  Description : Programme utilisant UniteCompteur (variable privee protegee)
  Fichier source : 05-variables-procedures-publiques-privees.md
  ============================================================================ }
program TestCompteur;

uses
  UniteCompteur;

begin
  Incrementer;
  Incrementer;
  WriteLn('Compteur : ', ObtenirValeur);  // Affiche 2
end.
