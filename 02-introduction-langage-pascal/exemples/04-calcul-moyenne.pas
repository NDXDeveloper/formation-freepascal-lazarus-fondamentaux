{ ============================================================================
  Section 2.4 : Types de donnees primitifs
  Description : Calcul de la moyenne de trois notes et verification
                de reussite avec un booleen
  Fichier source : 04-types-donnees-primitifs.md
  ============================================================================ }
program CalculMoyenne;  
var  
  note1, note2, note3: real;
  moyenne: real;
  aReussi: boolean;
begin
  note1 := 15.5;
  note2 := 12.0;
  note3 := 14.5;

  moyenne := (note1 + note2 + note3) / 3;
  aReussi := moyenne >= 10.0;  // La comparaison >= produit true ou false

  writeln('Note 1 : ', note1:0:1);
  writeln('Note 2 : ', note2:0:1);
  writeln('Note 3 : ', note3:0:1);
  writeln('Moyenne : ', moyenne:0:2);
  writeln('A réussi : ', aReussi);
end.
