{ ============================================================================
  Section 9.6 : Premier projet avec Lazarus IDE
  Description : Application graphique améliorée avec compteur de clics
  Fichier source : 06-premier-projet-lazarus-ide.md
  ============================================================================ }
program AppliCompteur;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms, Unit1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
