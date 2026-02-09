{ ============================================================================
  Section 2.5 : Operateurs arithmetiques et logiques
  Description : Calculatrice simple avec les 4 operations de base
  Fichier source : 05-operateurs-arithmetiques-logiques.md
  ============================================================================ }
program CalculatriceSimple;
var
  a, b: real;
  choix: integer;
  resultat: real;
begin
  a := 15.0;
  b := 4.0;
  choix := 1;  // 1=addition, 2=soustraction, 3=multiplication, 4=division

  // :0:2 formate un real : 0 = largeur minimale, 2 = decimales
  writeln('Nombre 1 : ', a:0:2);
  writeln('Nombre 2 : ', b:0:2);
  writeln;

  // Addition
  if choix = 1 then
  begin
    resultat := a + b;
    writeln('Résultat : ', a:0:2, ' + ', b:0:2, ' = ', resultat:0:2);
  end;

  // Soustraction
  if choix = 2 then
  begin
    resultat := a - b;
    writeln('Résultat : ', a:0:2, ' - ', b:0:2, ' = ', resultat:0:2);
  end;

  // Multiplication
  if choix = 3 then
  begin
    resultat := a * b;
    writeln('Résultat : ', a:0:2, ' * ', b:0:2, ' = ', resultat:0:2);
  end;

  // Division
  if choix = 4 then
  begin
    if b <> 0 then
    begin
      resultat := a / b;
      writeln('Résultat : ', a:0:2, ' / ', b:0:2, ' = ', resultat:0:2);
    end
    else
      writeln('Erreur : division par zéro !');
  end;
end.
