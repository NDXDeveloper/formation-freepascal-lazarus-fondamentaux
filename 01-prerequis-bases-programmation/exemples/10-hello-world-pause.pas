{ ============================================================================
  Section 1.10 : Premier programme - Hello World en Pascal
  Description : Hello World avec pause a la fin (ReadLn) pour empecher
                la console de se fermer immediatement sous Windows
  Fichier source : 10-premier-programme-hello-world-pascal.md
  ============================================================================ }
program HelloWorldPause;  
begin  
  WriteLn('Hello, World!');
  WriteLn('Appuyez sur Entrée pour continuer...');
  ReadLn;
end.
