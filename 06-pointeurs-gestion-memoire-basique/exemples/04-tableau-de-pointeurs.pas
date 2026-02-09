{ ============================================================================
  Section 6.4 : Pointeurs et Tableaux
  Description : Tableau ou chaque element est un pointeur vers un record
  Fichier source : 04-pointeurs-tableaux.md
  ============================================================================ }
program TableauDePointeurs;
uses SysUtils;

type
  PPerson = ^TPerson;
  TPerson = record
    nom: String;
    age: Integer;
  end;

var
  personnes: array[1..3] of PPerson;
  i: Integer;
begin
  // Créer 3 personnes
  for i := 1 to 3 do
  begin
    New(personnes[i]);
    personnes[i]^.nom := 'Personne' + IntToStr(i);
    personnes[i]^.age := 20 + i;
  end;

  // Afficher
  for i := 1 to 3 do
    WriteLn(personnes[i]^.nom, ' : ', personnes[i]^.age, ' ans');

  // Libérer
  for i := 1 to 3 do
  begin
    Dispose(personnes[i]);
    personnes[i] := nil;
  end;
end.
