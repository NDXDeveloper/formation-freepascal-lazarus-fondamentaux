{ ============================================================================
  Section 7.5 : Variables et procedures publiques/privees
  Description : Programme demonstrant l'encapsulation avec CompteEnBanque
  Fichier source : 05-variables-procedures-publiques-privees.md
  ============================================================================ }
program ClientBanque;

uses
  CompteEnBanque;

begin
  Deposer(1000);
  Retirer(200);
  WriteLn('Solde : ', ObtenirSolde:0:2);  // Affiche 800.00

  Retirer(1000);       // Affiche "Erreur : retrait impossible"
end.
