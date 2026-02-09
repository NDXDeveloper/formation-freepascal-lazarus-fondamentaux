{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Calculatrice simple utilisant case-of sur un caractere
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program CalculatriceCaseOf;
var
  nombre1, nombre2: Real;
  operation: Char;
  resultat: Real;
begin
  WriteLn('=== CALCULATRICE SIMPLE ===');
  Write('Premier nombre : ');
  ReadLn(nombre1);
  Write('Operation (+, -, *, /) : ');
  ReadLn(operation);
  Write('Deuxieme nombre : ');
  ReadLn(nombre2);
  { case..of fonctionne avec Char car c'est un type ordinal (comme Integer et Boolean) }
  case operation of
    '+':
      begin
        resultat := nombre1 + nombre2;
        WriteLn('Resultat : ', resultat:0:2);
      end;
    '-':
      begin
        resultat := nombre1 - nombre2;
        WriteLn('Resultat : ', resultat:0:2);
      end;
    '*':
      begin
        resultat := nombre1 * nombre2;
        WriteLn('Resultat : ', resultat:0:2);
      end;
    '/':
      begin
        if nombre2 <> 0 then
        begin
          resultat := nombre1 / nombre2;
          WriteLn('Resultat : ', resultat:0:2);
        end
        else
          WriteLn('Erreur : division par zero !');
      end;
  else
    WriteLn('Operation non reconnue !');
  end;
end.
