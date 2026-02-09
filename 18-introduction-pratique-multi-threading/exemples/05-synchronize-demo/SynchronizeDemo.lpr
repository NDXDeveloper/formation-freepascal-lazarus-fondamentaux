{ ============================================================================
  Section 18.5 : TThread.Synchronize - communication thread-UI
  Description : Demonstration de Synchronize pour mettre a jour
                ProgressBar, Label et Memo depuis un thread de travail
  Fichier source : 05-tthread-synchronize-communication.md
  ============================================================================ }
program SynchronizeDemo;

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
