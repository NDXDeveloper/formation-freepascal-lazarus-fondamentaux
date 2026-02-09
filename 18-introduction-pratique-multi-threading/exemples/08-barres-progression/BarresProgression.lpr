{ ============================================================================
  Section 18.8 : Barres de progression et feedback utilisateur
  Description : Progression multi-niveaux (globale + etape), estimation
                du temps restant, Queue pour mises a jour frequentes
  Fichier source : 08-barres-progression-feedback-utilisateur.md
  ============================================================================ }
program BarresProgression;

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
