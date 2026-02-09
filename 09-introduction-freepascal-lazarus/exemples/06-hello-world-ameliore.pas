{ ============================================================================
  Section 9.6 : Premier projet avec Lazarus IDE
  Description : Programme console amélioré avec mise en forme et apostrophes
  Fichier source : 06-premier-projet-lazarus-ide.md
  ============================================================================ }
program HelloWorld;

begin
  WriteLn('==================================');
  WriteLn('  Bienvenue dans mon programme !');
  WriteLn('==================================');
  WriteLn;  // Ligne vide
  WriteLn('Aujourd''hui, j''apprends Pascal !');
  WriteLn('C''est facile et amusant !');
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
