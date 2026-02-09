{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Unite avec implementation separee par plateforme (.inc)
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
unit SystemInfo;

{$mode objfpc}{$H+}

interface

function ObtenirVersionOS: string;
function ObtenirNomMachine: string;

implementation

{$IFDEF WINDOWS}
  {$I SystemInfo.Windows.inc}
{$ENDIF}

{$IFDEF UNIX}
  {$I SystemInfo.Unix.inc}
{$ENDIF}

end.
