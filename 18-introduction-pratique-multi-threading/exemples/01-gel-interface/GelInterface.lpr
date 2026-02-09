{ ============================================================================
  Section 18.1 : Probleme du gel des interfaces graphiques
  Description : Demontre le gel GUI lors d'un calcul long vs l'utilisation
                d'un thread pour garder l'interface reactive
  Fichier source : 01-probleme-gel-interfaces-graphiques.md
  ============================================================================ }
program GelInterface;

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
