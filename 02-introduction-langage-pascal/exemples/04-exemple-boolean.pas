{ ============================================================================
  Section 2.4 : Types de donnees primitifs
  Description : Declaration et utilisation de variables booleennes
                (true/false)
  Fichier source : 04-types-donnees-primitifs.md
  ============================================================================ }
program ExempleBoolean;  
var  
  estMajeur: boolean;
  aReussi: boolean;
  estConnecte: boolean;
begin
  estMajeur := true;
  aReussi := false;
  estConnecte := true;

  writeln('Majeur : ', estMajeur);
  writeln('A réussi : ', aReussi);
  writeln('Connecté : ', estConnecte);
end.
