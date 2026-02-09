{ ============================================================================
  Section 9.8 : Compilation et exécution
  Description : Affichage des paramètres passés en ligne de commande
  Fichier source : 08-compilation-execution.md
  ============================================================================ }
program ParametresProgramme;

var
  i: Integer;
begin
  WriteLn('Nombre de paramètres : ', ParamCount);
  for i := 1 to ParamCount do
    WriteLn('Paramètre ', i, ' : ', ParamStr(i));
end.
