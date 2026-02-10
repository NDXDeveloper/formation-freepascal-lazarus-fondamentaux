{ ============================================================================
  Section 4.9 : Recursivite
  Description : Tours de Hanoi - exemple classique de recursivite
  Fichier source : 09-recursivite.md
  ============================================================================ }
program ToursHanoi;

procedure DeplacerDisques(n: Integer; source, destination, auxiliaire: Char);  
begin  
  if n = 1 then
  begin
    // Cas de base : déplacer un seul disque
    WriteLn('Déplacer disque de ', source, ' vers ', destination);
  end
  else
  begin
    // Cas récursif
    // 1. Déplacer n-1 disques de source vers auxiliaire
    DeplacerDisques(n - 1, source, auxiliaire, destination);

    // 2. Déplacer le disque restant de source vers destination
    WriteLn('Déplacer disque de ', source, ' vers ', destination);

    // 3. Déplacer n-1 disques d'auxiliaire vers destination
    DeplacerDisques(n - 1, auxiliaire, destination, source);
  end;
end;

begin
  WriteLn('Solution pour 3 disques :');
  DeplacerDisques(3, 'A', 'C', 'B');
end.
