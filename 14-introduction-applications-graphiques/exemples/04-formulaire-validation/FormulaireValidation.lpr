{
  Section 14.4 - Composants de base (TButton, TEdit, TLabel)
  Description : Formulaire de validation avec Labels, Edits et Buttons
                Validation de nom, email et âge avec messages d'erreur
  Fichier source : 04-composants-base-tbutton-tedit-tlabel.md
}
program FormulaireValidation;

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
