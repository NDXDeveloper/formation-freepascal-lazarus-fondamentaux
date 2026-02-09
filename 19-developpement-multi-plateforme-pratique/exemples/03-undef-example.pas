{ ============================================================================
  Section 19.3 : Directives de compilation conditionnelle
  Description : Annulation d'un symbole avec UNDEF
  Fichier source : 03-directives-compilation-conditionnelle.md
  ============================================================================ }
program UndefExample;

{$mode objfpc}{$H+}

{$DEFINE MA_FONCTIONNALITE}

begin
  {$IFDEF MA_FONCTIONNALITE}
  WriteLn('Fonctionnalite activee');
  {$ENDIF}

  // Desactiver le symbole
  {$UNDEF MA_FONCTIONNALITE}

  {$IFDEF MA_FONCTIONNALITE}
  WriteLn('Cette ligne ne sera jamais affichee');
  {$ENDIF}
end.
