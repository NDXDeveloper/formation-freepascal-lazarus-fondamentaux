{ ============================================================================
  Section 18.3 : La classe TThread - creation et utilisation
  Description : TThread basique, creation, Execute, FreeOnTerminate, WaitFor,
                constructeur personnalise, verification Terminated
  Fichier source : 03-classe-tthread-creation-utilisation.md
  ============================================================================ }
program TThreadCreation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils;

{ --------------------------------------------------------------------------
  Demo 1 : Thread minimal - Fire and Forget
  -------------------------------------------------------------------------- }
type
  TThreadCompteur = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadCompteur.Execute;
var
  i: Integer;
begin
  for i := 1 to 5 do
  begin
    if Terminated then
      Exit;
    Sleep(200);
    WriteLn('  [Thread compteur] Iteration ', i);
  end;
end;

{ --------------------------------------------------------------------------
  Demo 2 : Thread avec constructeur personnalise et resultat
  -------------------------------------------------------------------------- }
type
  TThreadCalcul = class(TThread)
  private
    FMax: Integer;
    FResultat: Double;
  protected
    procedure Execute; override;
  public
    constructor Create(AMax: Integer);
    property Resultat: Double read FResultat;
  end;

constructor TThreadCalcul.Create(AMax: Integer);
begin
  inherited Create(True);  // Toujours appeler inherited Create(True)
  FMax := AMax;
  FResultat := 0;
end;

procedure TThreadCalcul.Execute;
var
  i: Integer;
begin
  WriteLn('  [Thread calcul] Debut du calcul (somme de 1 a ', FMax, ')');
  FResultat := 0;
  for i := 1 to FMax do
  begin
    if Terminated then
    begin
      WriteLn('  [Thread calcul] Interrompu !');
      Exit;
    end;
    FResultat := FResultat + Sqrt(i);
    if i mod 2500 = 0 then
      Sleep(1);  // Ceder un peu de temps
  end;
  WriteLn('  [Thread calcul] Calcul termine');
end;

{ --------------------------------------------------------------------------
  Demo 3 : Thread avec boucle continue et arret propre
  -------------------------------------------------------------------------- }
type
  TThreadTravail = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadTravail.Execute;
var
  Compteur: Integer;
begin
  Compteur := 0;
  while not Terminated do
  begin
    Inc(Compteur);
    WriteLn('  [Thread travail] Cycle ', Compteur);
    Sleep(300);
  end;
  WriteLn('  [Thread travail] Arret propre apres ', Compteur, ' cycles');
end;

{ ========================================================================== }
var
  Thread: TThreadCompteur;
  ThreadCalc: TThreadCalcul;
  ThreadW: TThreadTravail;
begin
  WriteLn('=== Demo 1 : Fire and Forget (FreeOnTerminate = True) ===');
  WriteLn('Le thread compte de 1 a 5 et se libere automatiquement.');
  WriteLn;

  Thread := TThreadCompteur.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;

  // Le thread principal continue pendant que le thread travaille
  WriteLn('[Principal] Thread demarre, on attend 2 secondes...');
  Sleep(2000);
  WriteLn('[Principal] Fin de l''attente (le thread a du terminer)');
  WriteLn;

  // -----------------------------------------------------------------------
  WriteLn('=== Demo 2 : Wait For Result (FreeOnTerminate = False) ===');
  WriteLn('Le thread calcule et on recupere le resultat.');
  WriteLn;

  ThreadCalc := TThreadCalcul.Create(10000);
  ThreadCalc.FreeOnTerminate := False;
  try
    ThreadCalc.Start;
    WriteLn('[Principal] Thread de calcul demarre, WaitFor...');
    ThreadCalc.WaitFor;
    WriteLn('[Principal] Resultat = ', ThreadCalc.Resultat:0:4);
  finally
    ThreadCalc.Free;
  end;
  WriteLn;

  // -----------------------------------------------------------------------
  WriteLn('=== Demo 3 : Arret propre avec Terminate + WaitFor ===');
  WriteLn('Le thread tourne en boucle, on l''arrete apres 1 seconde.');
  WriteLn;

  ThreadW := TThreadTravail.Create(True);
  ThreadW.FreeOnTerminate := False;
  try
    ThreadW.Start;
    WriteLn('[Principal] Thread de travail demarre...');
    Sleep(1000);
    WriteLn('[Principal] Appel de Terminate...');
    ThreadW.Terminate;
    ThreadW.WaitFor;
    WriteLn('[Principal] Thread arrete et libere.');
  finally
    ThreadW.Free;
  end;

  WriteLn;
  WriteLn('=== Toutes les demos terminees ===');
end.
