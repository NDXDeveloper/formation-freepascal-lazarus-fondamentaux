{ ============================================================================
  Section 18.4 : Cycle de vie d'un thread
  Description : Etats d'un thread (cree, suspendu, running, termine, libere),
                Terminated, Finished, strategies de liberation, try-finally
  Fichier source : 04-cycle-vie-thread.md
  ============================================================================ }
program CycleVieThread;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils;

{ --------------------------------------------------------------------------
  Thread d'exemple avec nettoyage de ressource dans try-finally
  -------------------------------------------------------------------------- }
type
  TThreadExemple = class(TThread)
  private
    FNom: string;
    FDuree: Integer;  // nombre d'iterations
  protected
    procedure Execute; override;
  public
    constructor Create(const ANom: string; ADuree: Integer);
  end;

constructor TThreadExemple.Create(const ANom: string; ADuree: Integer);  
begin  
  inherited Create(True);  // Cree suspendu
  FNom := ANom;
  FDuree := ADuree;
end;

procedure TThreadExemple.Execute;  
var  
  i: Integer;
  Ressource: TStringList;
begin
  // Simuler l'acquisition d'une ressource
  Ressource := TStringList.Create;
  try
    WriteLn('  [', FNom, '] Execute demarre, ressource allouee');
    for i := 1 to FDuree do
    begin
      if Terminated then
      begin
        WriteLn('  [', FNom, '] Terminated detecte a l''iteration ', i);
        Exit;  // Le finally liberera la ressource
      end;
      Ressource.Add(Format('Donnee %d', [i]));
      Sleep(200);
    end;
    WriteLn('  [', FNom, '] Boucle terminee normalement (',
            Ressource.Count, ' elements)');
  finally
    Ressource.Free;
    WriteLn('  [', FNom, '] Ressource liberee dans finally');
  end;
end;

{ ========================================================================== }
procedure DemoEtatsThread;  
var  
  T: TThreadExemple;
begin
  WriteLn('--- Suivi des etats ---');

  // Etat 1 : Non cree
  WriteLn('1. Thread non cree (variable non initialisee)');

  // Etat 2 : Cree et suspendu
  T := TThreadExemple.Create('Etats', 5);
  T.FreeOnTerminate := False;
  WriteLn('2. Thread cree (suspendu)');
  WriteLn('   Terminated = ', T.Terminated);
  WriteLn('   Finished   = ', T.Finished);

  // Etat 3 : En execution
  T.Start;
  WriteLn('3. Thread demarre (en execution)');
  Sleep(300);  // Laisser tourner un peu
  WriteLn('   Terminated = ', T.Terminated);
  WriteLn('   Finished   = ', T.Finished);

  // Etat 4 : Attendre la fin
  WriteLn('4. Attente de la fin (WaitFor)...');
  T.WaitFor;
  WriteLn('5. Thread termine');
  WriteLn('   Terminated = ', T.Terminated);
  WriteLn('   Finished   = ', T.Finished);

  // Etat 5 : Libere
  T.Free;
  WriteLn('6. Thread libere (n''existe plus)');
end;

{ -------------------------------------------------------------------------- }
procedure DemoStrategieAutoFree;  
var  
  T: TThreadExemple;
begin
  WriteLn('--- Strategie 1 : FreeOnTerminate = True (fire-and-forget) ---');
  T := TThreadExemple.Create('Auto', 3);
  T.FreeOnTerminate := True;
  T.Start;
  WriteLn('[Principal] Thread lance, on ne garde pas de reference.');
  WriteLn('[Principal] Attente 2s pour laisser le thread finir...');
  Sleep(2000);
  WriteLn('[Principal] Le thread s''est libere automatiquement.');
end;

{ -------------------------------------------------------------------------- }
procedure DemoStrategieManuelle;  
var  
  T: TThreadExemple;
begin
  WriteLn('--- Strategie 2 : FreeOnTerminate = False (controle total) ---');
  T := TThreadExemple.Create('Manuel', 10);
  T.FreeOnTerminate := False;
  try
    T.Start;
    WriteLn('[Principal] Thread lance, on attend 600ms puis Terminate...');
    Sleep(600);
    WriteLn('[Principal] Appel Terminate...');
    T.Terminate;
    WriteLn('[Principal] Appel WaitFor...');
    T.WaitFor;
    WriteLn('[Principal] Thread termine, on peut lire Finished = ', T.Finished);
  finally
    T.Free;
    WriteLn('[Principal] Thread libere proprement.');
  end;
end;

{ ========================================================================== }
begin
  WriteLn('=== Demo 1 : Suivi des etats d''un thread ===');
  WriteLn;
  DemoEtatsThread;
  WriteLn;

  WriteLn('=== Demo 2 : Strategie FreeOnTerminate = True ===');
  WriteLn;
  DemoStrategieAutoFree;
  WriteLn;

  WriteLn('=== Demo 3 : Strategie FreeOnTerminate = False + arret propre ===');
  WriteLn;
  DemoStrategieManuelle;
  WriteLn;

  WriteLn('=== Toutes les demos terminees ===');
end.
