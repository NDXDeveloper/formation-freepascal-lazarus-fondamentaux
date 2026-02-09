{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Utilisation de l'unite PlatformUtils (wrapper)
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program UtilisationWrapper;

{$mode objfpc}{$H+}

uses
  SysUtils, PlatformUtils;

begin
  WriteLn('Nom d''utilisateur : ', ObtenirNomUtilisateur);
  WriteLn('ID du processus : ', ObtenirProcessID);
end.
