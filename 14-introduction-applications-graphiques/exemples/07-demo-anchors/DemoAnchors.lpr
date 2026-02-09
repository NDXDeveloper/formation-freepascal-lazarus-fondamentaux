{
  Section 14.7 - Layouts et anchors
  Description : Interface de recherche avec Anchors
                Les composants s'adaptent au redimensionnement
  Fichier source : 07-layouts-anchors.md
}
program DemoAnchors;

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
