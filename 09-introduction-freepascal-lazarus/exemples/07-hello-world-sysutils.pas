{ ============================================================================
  Section 9.7 : Structure d'un projet Lazarus
  Description : Programme console avec SysUtils - exemple de fichier .lpr
  Fichier source : 07-structure-projet-lazarus.md
  ============================================================================ }
program HelloWorld;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('Hello World !');
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
