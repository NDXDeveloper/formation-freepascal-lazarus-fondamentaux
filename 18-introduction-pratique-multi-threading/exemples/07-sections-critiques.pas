{ ============================================================================
  Section 18.7 : Variables partagees et section critique
  Description : Race condition, TCriticalSection, InterlockedIncrement,
                liste partagee thread-safe, producteur/consommateur
  Fichier source : 07-variables-partagees-section-critique.md
  ============================================================================ }
program SectionsCritiques;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, SyncObjs;

const
  NB_ITERATIONS = 100000;
  NB_THREADS    = 4;

{ ==========================================================================
  Demo 1 : Race condition (compteur global sans protection)
  ========================================================================== }
var
  CompteurSansProtection: Integer;

type
  TThreadSansProtection = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadSansProtection.Execute;
var
  i: Integer;
begin
  for i := 1 to NB_ITERATIONS do
    CompteurSansProtection := CompteurSansProtection + 1;
end;

procedure DemoRaceCondition;
var
  Threads: array[1..NB_THREADS] of TThreadSansProtection;
  i: Integer;
begin
  CompteurSansProtection := 0;

  for i := 1 to NB_THREADS do
  begin
    Threads[i] := TThreadSansProtection.Create(True);
    Threads[i].FreeOnTerminate := False;
  end;

  for i := 1 to NB_THREADS do
    Threads[i].Start;

  for i := 1 to NB_THREADS do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;

  WriteLn('  Attendu  : ', NB_THREADS * NB_ITERATIONS);
  WriteLn('  Obtenu   : ', CompteurSansProtection);
  if CompteurSansProtection = NB_THREADS * NB_ITERATIONS then
    WriteLn('  Resultat : Correct (par chance !)')
  else
    WriteLn('  Resultat : INCORRECT (race condition !)');
end;

{ ==========================================================================
  Demo 2 : Correction avec TCriticalSection
  ========================================================================== }
var
  CompteurProtege: Integer;
  CS: TCriticalSection;

type
  TThreadProtege = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadProtege.Execute;
var
  i: Integer;
begin
  for i := 1 to NB_ITERATIONS do
  begin
    CS.Enter;
    try
      CompteurProtege := CompteurProtege + 1;
    finally
      CS.Leave;
    end;
  end;
end;

procedure DemoCriticalSection;
var
  Threads: array[1..NB_THREADS] of TThreadProtege;
  i: Integer;
begin
  CompteurProtege := 0;
  CS := TCriticalSection.Create;
  try
    for i := 1 to NB_THREADS do
    begin
      Threads[i] := TThreadProtege.Create(True);
      Threads[i].FreeOnTerminate := False;
    end;

    for i := 1 to NB_THREADS do
      Threads[i].Start;

    for i := 1 to NB_THREADS do
    begin
      Threads[i].WaitFor;
      Threads[i].Free;
    end;
  finally
    CS.Free;
  end;

  WriteLn('  Attendu  : ', NB_THREADS * NB_ITERATIONS);
  WriteLn('  Obtenu   : ', CompteurProtege);
  if CompteurProtege = NB_THREADS * NB_ITERATIONS then
    WriteLn('  Resultat : Correct (protege par TCriticalSection)')
  else
    WriteLn('  Resultat : INCORRECT (ne devrait pas arriver)');
end;

{ ==========================================================================
  Demo 3 : InterlockedIncrement (operation atomique)
  ========================================================================== }
var
  CompteurAtomique: Integer;

type
  TThreadAtomique = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadAtomique.Execute;
var
  i: Integer;
begin
  for i := 1 to NB_ITERATIONS do
    InterlockedIncrement(CompteurAtomique);
end;

procedure DemoInterlockedIncrement;
var
  Threads: array[1..NB_THREADS] of TThreadAtomique;
  i: Integer;
begin
  CompteurAtomique := 0;

  for i := 1 to NB_THREADS do
  begin
    Threads[i] := TThreadAtomique.Create(True);
    Threads[i].FreeOnTerminate := False;
  end;

  for i := 1 to NB_THREADS do
    Threads[i].Start;

  for i := 1 to NB_THREADS do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;

  WriteLn('  Attendu  : ', NB_THREADS * NB_ITERATIONS);
  WriteLn('  Obtenu   : ', CompteurAtomique);
  if CompteurAtomique = NB_THREADS * NB_ITERATIONS then
    WriteLn('  Resultat : Correct (InterlockedIncrement)')
  else
    WriteLn('  Resultat : INCORRECT (ne devrait pas arriver)');
end;

{ ==========================================================================
  Demo 4 : Liste partagee thread-safe
  ========================================================================== }
type
  TListePartagee = class
  private
    FListe: TStringList;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ajouter(const S: string);
    function Retirer: string;
    function Nombre: Integer;
  end;

constructor TListePartagee.Create;
begin
  inherited;
  FListe := TStringList.Create;
  FCS := TCriticalSection.Create;
end;

destructor TListePartagee.Destroy;
begin
  FCS.Free;
  FListe.Free;
  inherited;
end;

procedure TListePartagee.Ajouter(const S: string);
begin
  FCS.Enter;
  try
    FListe.Add(S);
  finally
    FCS.Leave;
  end;
end;

function TListePartagee.Retirer: string;
begin
  Result := '';
  FCS.Enter;
  try
    if FListe.Count > 0 then
    begin
      Result := FListe[0];
      FListe.Delete(0);
    end;
  finally
    FCS.Leave;
  end;
end;

function TListePartagee.Nombre: Integer;
begin
  FCS.Enter;
  try
    Result := FListe.Count;
  finally
    FCS.Leave;
  end;
end;

procedure DemoListePartagee;
var
  Liste: TListePartagee;
  i: Integer;
begin
  Liste := TListePartagee.Create;
  try
    // Ajouter des elements depuis le thread principal
    for i := 1 to 10 do
      Liste.Ajouter(Format('Element %d', [i]));

    WriteLn('  Elements ajoutes : ', Liste.Nombre);

    // Retirer quelques elements
    WriteLn('  Retire : ', Liste.Retirer);
    WriteLn('  Retire : ', Liste.Retirer);
    WriteLn('  Retire : ', Liste.Retirer);
    WriteLn('  Elements restants : ', Liste.Nombre);
  finally
    Liste.Free;
  end;
end;

{ ==========================================================================
  Demo 5 : Pattern producteur / consommateur
  ========================================================================== }
var
  FileTaches: TListePartagee;
  TachesTraitees: Integer;
  CSTraitees: TCriticalSection;

type
  TThreadProducteur = class(TThread)
  private
    FNom: string;
    FNbTaches: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ANom: string; ANb: Integer);
  end;

  TThreadConsommateur = class(TThread)
  private
    FNom: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const ANom: string);
  end;

constructor TThreadProducteur.Create(const ANom: string; ANb: Integer);
begin
  inherited Create(True);
  FNom := ANom;
  FNbTaches := ANb;
end;

procedure TThreadProducteur.Execute;
var
  i: Integer;
begin
  for i := 1 to FNbTaches do
  begin
    if Terminated then Exit;
    FileTaches.Ajouter(Format('Tache %s-%d', [FNom, i]));
    Sleep(20);
  end;
end;

constructor TThreadConsommateur.Create(const ANom: string);
begin
  inherited Create(True);
  FNom := ANom;
end;

procedure TThreadConsommateur.Execute;
var
  Tache: string;
begin
  while not Terminated do
  begin
    Tache := FileTaches.Retirer;
    if Tache <> '' then
    begin
      // Simuler le traitement
      Sleep(10);
      CSTraitees.Enter;
      try
        Inc(TachesTraitees);
      finally
        CSTraitees.Leave;
      end;
    end
    else
      Sleep(50);  // Pas de tache, attendre
  end;
end;

procedure DemoProducteurConsommateur;
var
  Prod1, Prod2: TThreadProducteur;
  Cons1, Cons2: TThreadConsommateur;
begin
  FileTaches := TListePartagee.Create;
  CSTraitees := TCriticalSection.Create;
  TachesTraitees := 0;
  try
    // Creer 2 producteurs (10 taches chacun) et 2 consommateurs
    Prod1 := TThreadProducteur.Create('P1', 10);
    Prod1.FreeOnTerminate := False;
    Prod2 := TThreadProducteur.Create('P2', 10);
    Prod2.FreeOnTerminate := False;
    Cons1 := TThreadConsommateur.Create('C1');
    Cons1.FreeOnTerminate := False;
    Cons2 := TThreadConsommateur.Create('C2');
    Cons2.FreeOnTerminate := False;

    // Demarrer tous les threads
    Prod1.Start;
    Prod2.Start;
    Cons1.Start;
    Cons2.Start;

    // Attendre que les producteurs terminent
    Prod1.WaitFor;
    Prod2.WaitFor;

    // Laisser les consommateurs finir le traitement
    Sleep(500);

    // Arreter les consommateurs
    Cons1.Terminate;
    Cons2.Terminate;
    Cons1.WaitFor;
    Cons2.WaitFor;

    WriteLn('  Taches produites  : 20 (2 producteurs x 10)');
    WriteLn('  Taches traitees   : ', TachesTraitees);
    WriteLn('  Taches restantes  : ', FileTaches.Nombre);

    Prod1.Free;
    Prod2.Free;
    Cons1.Free;
    Cons2.Free;
  finally
    CSTraitees.Free;
    FileTaches.Free;
  end;
end;

{ ========================================================================== }
begin
  WriteLn('=== Demo 1 : Race condition (sans protection) ===');
  WriteLn;
  DemoRaceCondition;
  WriteLn;

  WriteLn('=== Demo 2 : TCriticalSection (compteur protege) ===');
  WriteLn;
  DemoCriticalSection;
  WriteLn;

  WriteLn('=== Demo 3 : InterlockedIncrement (operation atomique) ===');
  WriteLn;
  DemoInterlockedIncrement;
  WriteLn;

  WriteLn('=== Demo 4 : Liste partagee thread-safe ===');
  WriteLn;
  DemoListePartagee;
  WriteLn;

  WriteLn('=== Demo 5 : Producteur / Consommateur ===');
  WriteLn;
  DemoProducteurConsommateur;
  WriteLn;

  WriteLn('=== Toutes les demos terminees ===');
end.
