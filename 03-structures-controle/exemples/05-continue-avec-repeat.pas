{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Utilisation de continue avec une boucle repeat-until
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program ContinueAvecRepeat;  
var  
  nombre: Integer;
  compteur: Integer;
begin
  compteur := 0;

  WriteLn('Entrez 5 nombres positifs :');

  repeat
    compteur := compteur + 1;
    Write('Nombre ', compteur, ' : ');
    ReadLn(nombre);

    if nombre <= 0 then
    begin
      WriteLn('Nombre invalide, ignoré');
      compteur := compteur - 1;  // Ne compte pas cette tentative
      // Continue saute directement au test 'until' (pas au début du repeat)
      continue;
    end;

    WriteLn('Nombre ', nombre, ' accepté');
  until compteur >= 5;

  WriteLn('Merci !');
end.
