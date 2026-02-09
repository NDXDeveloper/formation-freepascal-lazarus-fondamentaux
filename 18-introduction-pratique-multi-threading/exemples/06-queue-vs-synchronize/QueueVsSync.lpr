{ ============================================================================
  Section 18.6 : TThread.Queue vs Synchronize
  Description : Comparaison visuelle du comportement de Queue et Synchronize
                avec deux Memos cote a cote
  Fichier source : 06-tthread-queue-vs-synchronize.md
  ============================================================================ }
program QueueVsSync;

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
