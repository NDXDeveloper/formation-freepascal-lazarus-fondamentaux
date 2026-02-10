{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Utilisation de CThreads pour les threads sous Unix
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program AppliAvecThreads;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  CThreads,  // DOIT etre en PREMIER sous Unix/Linux !
  {$ENDIF}
  Classes, SysUtils;

type
  TMonThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TMonThread.Execute;  
begin  
  WriteLn('Thread en cours d''execution...');
  Sleep(1000);
end;

var
  MonThread: TMonThread;
begin
  MonThread := TMonThread.Create(False);
  MonThread.WaitFor;
  MonThread.Free;
  WriteLn('Thread termine');
end.
