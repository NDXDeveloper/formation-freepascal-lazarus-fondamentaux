{ ============================================================================
  Section 4.2 : Declaration et appel
  Description : Procedure simple avec declaration et appel
  Fichier source : 02-declaration-appel.md
  ============================================================================ }
program ExempleDeclaration;

procedure DireBonjour;  
begin  
  WriteLn('Bonjour tout le monde !');
  WriteLn('Comment allez-vous ?');
end;

begin
  DireBonjour;  // Appel de la procédure
end.
