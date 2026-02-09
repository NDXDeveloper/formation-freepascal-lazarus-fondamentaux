{ ============================================================================
  Section 6.2 : Declaration et Utilisation de Pointeurs
  Description : Echange de valeurs entre deux variables via pointeurs
  Fichier source : 02-declaration-utilisation-pointeurs.md
  ============================================================================ }
program EchangeValeurs;
type
  PInteger = ^Integer;

procedure Echanger(a, b: PInteger);
var
  temp: Integer;
begin
  if (a <> nil) and (b <> nil) then
  begin
    temp := a^;
    a^ := b^;
    b^ := temp;
  end;
end;

var
  x, y: Integer;
begin
  x := 5;
  y := 10;

  WriteLn('Avant : x=', x, ' y=', y);
  Echanger(@x, @y);
  WriteLn('Après : x=', x, ' y=', y);
end.
