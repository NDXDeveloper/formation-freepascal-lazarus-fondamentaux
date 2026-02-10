{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Calculatrice interactive avec saisie de deux nombres et
                d'un operateur (+, -, *, /) - utilise case..of
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program CalculatriceInteractive;  
var  
  nombre1, nombre2: real;
  operateur: char;
  resultat: real;
begin
  WriteLn('=== CALCULATRICE ===');
  WriteLn;

  Write('Premier nombre : ');
  ReadLn(nombre1);

  Write('Opérateur (+, -, *, /) : ');
  ReadLn(operateur);

  Write('Deuxième nombre : ');
  ReadLn(nombre2);

  WriteLn;

  case operateur of
    '+': resultat := nombre1 + nombre2;
    '-': resultat := nombre1 - nombre2;
    '*': resultat := nombre1 * nombre2;
    '/':
      if nombre2 <> 0 then    // <> signifie "different de" en Pascal
        resultat := nombre1 / nombre2
      else
      begin
        WriteLn('Erreur : division par zéro !');
        ReadLn;
        exit;
      end;
  else
    WriteLn('Opérateur invalide !');
    ReadLn;
    exit;
  end;

  WriteLn('Résultat : ', nombre1:0:2, ' ', operateur, ' ', nombre2:0:2, ' = ', resultat:0:2);

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
