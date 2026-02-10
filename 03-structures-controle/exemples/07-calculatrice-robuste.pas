{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Calculatrice robuste avec gestion des erreurs de saisie
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program CalculatriceRobuste;  
var  
  a, b, resultat: Real;
  operation: Char;
  erreur: Boolean;
begin
  WriteLn('═══════════════════════════');
  WriteLn('   CALCULATRICE ROBUSTE');
  WriteLn('═══════════════════════════');
  WriteLn;

  erreur := False;

  Write('Premier nombre : ');
  ReadLn(a);
  Write('Opération (+, -, *, /) : ');
  ReadLn(operation);
  Write('Deuxième nombre : ');
  ReadLn(b);
  WriteLn;

  case operation of
    '+': resultat := a + b;
    '-': resultat := a - b;
    '*': resultat := a * b;
    '/':
      begin
        if b = 0 then
        begin
          WriteLn('ERREUR : Division par zéro impossible !');
          erreur := True;
        end
        else
          resultat := a / b;
      end;
  else
    begin
      WriteLn('ERREUR : Opération inconnue !');
      WriteLn('Opérations valides : +, -, *, /');
      erreur := True;
    end;
  end;

  if not erreur then
  begin
    WriteLn('═══════════════════════════');
    WriteLn('Calcul : ', a:0:2, ' ', operation, ' ', b:0:2);
    WriteLn('Résultat : ', resultat:0:2);
    WriteLn('═══════════════════════════');
  end;
end.
