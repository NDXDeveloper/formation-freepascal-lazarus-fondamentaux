{
  Section 16.6 — Composants data-aware
  Description : Programme principal Lazarus - demo TDBGrid, TDBNavigator,
                TDBEdit, TDataSource, TDBMemo, validation BeforePost
  Fichier source : 06-composants-data-aware.md
}
program DataAwareDemo;

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
