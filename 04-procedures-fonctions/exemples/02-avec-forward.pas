{ ============================================================================
  Section 4.2 : Declaration et appel
  Description : Declaration anticipee (forward) de procedure
  Fichier source : 02-declaration-appel.md
  ============================================================================ }
program AvecForward;

procedure Procedure1; forward;  // Déclaration anticipée

procedure Procedure2;  
begin  
  Procedure1;  // OK maintenant
end;

procedure Procedure1;  // Définition complète  
begin  
  WriteLn('Procédure 1');
end;

begin
  Procedure2;
end.
