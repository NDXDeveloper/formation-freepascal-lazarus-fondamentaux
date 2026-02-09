{
  Section 19.6 - Cross-compilation : théorie et pratique
  Description : Programme principal du projet CrossGUI
                Application graphique cross-compilable
  Fichier source : 06-cross-compilation-theorie-pratique.md
}
program CrossGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, MainForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
