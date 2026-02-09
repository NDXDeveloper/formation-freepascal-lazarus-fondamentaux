{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Clause uses conditionnelle selon la plateforme
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program UtilisationConditionnelle;

{$mode objfpc}{$H+}

uses
  SysUtils,
  {$IFDEF WINDOWS}
  Windows,
  Registry,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  Unix,
  {$ENDIF}
  Classes;

begin
  WriteLn('Programme multi-plateforme');
end.
