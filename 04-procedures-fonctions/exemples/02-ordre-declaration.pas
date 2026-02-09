{ ============================================================================
  Section 4.2 : Declaration et appel
  Description : Ordre correct de declaration des procedures
  Fichier source : 02-declaration-appel.md
  ============================================================================ }
program Correct;

procedure Procedure1;
begin
  WriteLn('Procédure 1');
end;

procedure Procedure2;
begin
  Procedure1;  // OK : Procedure1 est déjà déclarée au-dessus
end;

begin
  Procedure2;
end.
