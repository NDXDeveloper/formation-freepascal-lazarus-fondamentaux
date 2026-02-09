{
  Section 14.5 - Événements et handlers
  Description : Démonstration des handlers partagés, du Tag,
                et de l'assignation dynamique de handlers
  Fichier source : 05-evenements-handlers.md
}
program DemoEvenements;

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
