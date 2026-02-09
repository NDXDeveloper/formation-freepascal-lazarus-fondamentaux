{ ============================================================================
  Section 7.6 : Sections initialization et finalization
  Description : Programme demonstrant initialization/finalization avec compteur
  Fichier source : 06-sections-initialization-finalization.md
  Note : Utilise UniteCompteurInit au lieu de UniteCompteur (conflit section 05)
  ============================================================================ }
program TestCompteurInit;

uses
  UniteCompteurInit;

begin
  WriteLn('Début du programme principal');

  Incrementer;
  Incrementer;
  Incrementer;
  WriteLn('Compteur actuel : ', ObtenirCompteur);

  WriteLn('Fin du programme principal');
end.
