{ ============================================================================
  Section 6.8 : Fuites Memoire et Bonnes Pratiques
  Description : Encapsulation creation/destruction avec try-finally
  Fichier source : 08-fuites-memoire-bonnes-pratiques.md
  ============================================================================ }
{$mode objfpc}{$H+}
program EncapsulationMemoire;
type
  PPerson = ^TPerson;
  TPerson = record
    nom: String;
    age: Integer;
  end;

function CreerPersonne(n: String; a: Integer): PPerson;
begin
  New(Result);
  Result^.nom := n;
  Result^.age := a;
end;

procedure DetruirePersonne(var p: PPerson);
begin
  if p <> nil then
  begin
    Dispose(p);
    p := nil;
  end;
end;

// Utilisation claire
var
  pers: PPerson;
begin
  pers := CreerPersonne('Alice', 30);
  try
    WriteLn('Nom : ', pers^.nom);
    WriteLn('Age : ', pers^.age);
  finally
    DetruirePersonne(pers);
  end;

  if pers = nil then
    WriteLn('Pointeur remis à nil après destruction');
end.
