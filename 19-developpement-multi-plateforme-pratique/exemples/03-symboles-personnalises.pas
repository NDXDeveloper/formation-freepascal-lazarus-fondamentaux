{ ============================================================================
  Section 19.3 : Directives de compilation conditionnelle
  Description : Definition et utilisation de symboles personnalises
  Fichier source : 03-directives-compilation-conditionnelle.md
  ============================================================================ }
program SymbolesPersonnalises;

{$mode objfpc}{$H+}

// Definir un symbole
{$DEFINE VERSION_DEMO}
{$DEFINE AFFICHAGE_COULEUR}

begin
  WriteLn('Mon Application');

  {$IFDEF VERSION_DEMO}
  WriteLn('VERSION DEMO - Fonctionnalites limitees');
  WriteLn('Achetez la version complete sur notre site !');
  {$ELSE}
  WriteLn('Version complete enregistree');
  {$ENDIF}

  {$IFDEF AFFICHAGE_COULEUR}
  WriteLn('Mode couleur active');
  {$ENDIF}
end.
