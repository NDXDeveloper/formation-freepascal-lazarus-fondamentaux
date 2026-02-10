{
  Section 20.6 - Gestion efficace de la mémoire
  Description : Démonstration des patterns de gestion mémoire correcte
                New/Dispose, Create/Free, try-finally, FreeAndNil
  Fichier source : 06-gestion-efficace-memoire.md
}
program GestionMemoirePatterns;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  PEntier = ^Integer;

  TMonObjet = class
  private
    FNom: String;
    FValeur: Integer;
  public
    constructor Create(const ANom: String; AValeur: Integer);
    destructor Destroy; override;
    procedure Afficher;
  end;

constructor TMonObjet.Create(const ANom: String; AValeur: Integer);  
begin  
  inherited Create;
  FNom := ANom;
  FValeur := AValeur;
  WriteLn('    [Create] ', FNom, ' (valeur=', FValeur, ')');
end;

destructor TMonObjet.Destroy;  
begin  
  WriteLn('    [Destroy] ', FNom);
  inherited Destroy;
end;

procedure TMonObjet.Afficher;  
begin  
  WriteLn('    ', FNom, ' = ', FValeur);
end;

{ Pattern 1 : New/Dispose avec pointeurs }
procedure DemoNewDispose;  
var  
  p: PEntier;
begin
  WriteLn('--- Pattern 1 : New/Dispose ---');
  New(p);
  try
    p^ := 42;
    WriteLn('  Valeur du pointeur : ', p^);
  finally
    Dispose(p);
    WriteLn('  Pointeur libéré');
  end;
end;

{ Pattern 2 : Create/Free avec try-finally }
procedure DemoCreateFree;  
var  
  liste: TStringList;
begin
  WriteLn;
  WriteLn('--- Pattern 2 : Create/Free (try-finally) ---');
  liste := TStringList.Create;
  try
    liste.Add('Premier');
    liste.Add('Deuxième');
    liste.Add('Troisième');
    WriteLn('  Liste contient ', liste.Count, ' éléments');
    WriteLn('  [1] = "', liste[1], '"');
  finally
    liste.Free;
    WriteLn('  Liste libérée');
  end;
end;

{ Pattern 3 : FreeAndNil pour sécurité }
procedure DemoFreeAndNil;  
var  
  obj: TMonObjet;
begin
  WriteLn;
  WriteLn('--- Pattern 3 : FreeAndNil ---');
  obj := TMonObjet.Create('Objet-FreeAndNil', 99);
  try
    obj.Afficher;
  finally
    FreeAndNil(obj);
  end;
  WriteLn('  obj est nil ? ', obj = nil);
  { obj.Free ici ne planterait pas car obj = nil }
end;

{ Pattern 4 : Plusieurs objets avec try-finally imbriqués }
procedure DemoMultiplesObjets;  
var  
  obj1, obj2: TMonObjet;
begin
  WriteLn;
  WriteLn('--- Pattern 4 : Objets multiples ---');
  obj1 := TMonObjet.Create('Premier-Obj', 10);
  try
    obj2 := TMonObjet.Create('Second-Obj', 20);
    try
      obj1.Afficher;
      obj2.Afficher;
    finally
      obj2.Free;
    end;
  finally
    obj1.Free;
  end;
end;

{ Pattern 5 : Réutiliser un objet au lieu de recréer }
procedure DemoReutilisation;  
var  
  liste: TStringList;
  i, iteration: Integer;
begin
  WriteLn;
  WriteLn('--- Pattern 5 : Réutilisation d''objet ---');
  liste := TStringList.Create;
  try
    for iteration := 1 to 3 do
    begin
      liste.Clear;  { Réutilise au lieu de recréer }
      for i := 1 to 5 do
        liste.Add(Format('Iter%d-Elem%d', [iteration, i]));
      WriteLn('  Iteration ', iteration, ' : ', liste.Count, ' éléments');
    end;
  finally
    liste.Free;
    WriteLn('  Liste libérée (1 seule allocation pour 3 itérations)');
  end;
end;

{ Pattern 6 : GetMem/FreeMem pour buffers }
procedure DemoGetMemFreeMem;  
var  
  buffer: PByte;
  taille: Integer;
  i: Integer;
begin
  WriteLn;
  WriteLn('--- Pattern 6 : GetMem/FreeMem ---');
  taille := 256;
  GetMem(buffer, taille);
  try
    for i := 0 to taille - 1 do
      buffer[i] := Byte(i mod 256);
    WriteLn('  Buffer de ', taille, ' octets alloué et rempli');
    WriteLn('  Premier octet : ', buffer[0]);
    WriteLn('  Dernier octet : ', buffer[taille - 1]);
  finally
    FreeMem(buffer);
    WriteLn('  Buffer libéré');
  end;
end;

begin
  WriteLn('=== Patterns de Gestion Mémoire ===');
  WriteLn('Chaque pattern montre la bonne façon de gérer la mémoire.');
  WriteLn;

  DemoNewDispose;
  DemoCreateFree;
  DemoFreeAndNil;
  DemoMultiplesObjets;
  DemoReutilisation;
  DemoGetMemFreeMem;

  WriteLn;
  WriteLn('Tous les patterns exécutés sans fuite mémoire.');
end.
