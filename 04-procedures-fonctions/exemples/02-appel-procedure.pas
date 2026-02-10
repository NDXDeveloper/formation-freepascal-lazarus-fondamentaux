{ ============================================================================
  Section 4.2 : Declaration et appel
  Description : Appel multiple de procedures
  Fichier source : 02-declaration-appel.md
  ============================================================================ }
program AppelProcedure;

procedure Message1;  
begin  
  WriteLn('Premier message');
end;

procedure Message2;  
begin  
  WriteLn('Deuxième message');
end;

begin
  Message1;      // Premier appel
  Message2;      // Deuxième appel
  Message1;      // On peut rappeler la même procédure
end.
