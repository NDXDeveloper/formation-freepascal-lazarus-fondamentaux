{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Utilisation de break avec une boucle while infinie
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program BreakAvecWhile;
var
  nombre, tentatives: Integer;
begin
  tentatives := 0;

  WriteLn('Entrez des nombres positifs (nombre négatif pour arrêter) :');

  while True do  // Boucle infinie intentionnelle
  begin
    tentatives := tentatives + 1;
    Write('Nombre ', tentatives, ' : ');
    ReadLn(nombre);

    if nombre < 0 then
    begin
      WriteLn('Nombre négatif détecté. Arrêt.');
      break;  // Sort de la boucle infinie
    end;

    WriteLn('Vous avez entré : ', nombre);
  end;

  WriteLn('Total de tentatives : ', tentatives);
end.
