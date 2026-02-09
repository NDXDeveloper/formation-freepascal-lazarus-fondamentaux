{
  Section 14.2 - Première application fenêtrée
  Description : Squelette minimal d'une application Lazarus
                Montre la structure .lpr + unit + .lfm
  Fichier source : 02-premiere-application-fenetree.md
}
program PremiereFenetre;

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
