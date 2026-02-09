{
  Section 14.3 - Formulaires (TForm)
  Description : Démonstration des événements de formulaire
                OnCreate, OnCloseQuery, OnResize, OnKeyDown
  Fichier source : 03-formulaires-tform.md
}
program DemoFormEvents;

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
