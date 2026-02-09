{ ============================================================================
  Section 6.3 : Allocation Dynamique (New, Dispose)
  Description : Fonctions de creation et liberation d'objets dynamiques
  Fichier source : 03-allocation-dynamique-new-dispose.md
  ============================================================================ }
{$mode objfpc}{$H+}
program CreerLibererPersonne;
type
  PPerson = ^TPerson;
  TPerson = record
    nom: String;
    age: Integer;
  end;

function CreerPersonne(n: String; a: Integer): PPerson;
begin
  New(Result);           // Allocation
  Result^.nom := n;      // Initialisation
  Result^.age := a;
end;

procedure LibererPersonne(var p: PPerson);  // var nécessaire pour que p := nil modifie le pointeur de l'appelant
begin
  if p <> nil then
  begin
    Dispose(p);          // Libération
    p := nil;
  end;
end;

var
  pers: PPerson;
begin
  pers := CreerPersonne('Sophie', 28);
  WriteLn(pers^.nom, ' a ', pers^.age, ' ans');

  LibererPersonne(pers);  // Ne pas oublier !
end.
