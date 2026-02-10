{ ============================================================================
  Section 4.3 : Parametres par valeur
  Description : Procedure simple avec un parametre String
  Fichier source : 03-parametres-par-valeur.md
  ============================================================================ }
program ExempleParametre;

procedure DireBonjour(prenom: String);  
begin  
  WriteLn('Bonjour ', prenom, ' !');
end;

begin
  DireBonjour('Marie');   // Affiche : Bonjour Marie !
  DireBonjour('Pierre');  // Affiche : Bonjour Pierre !
  DireBonjour('Sophie');  // Affiche : Bonjour Sophie !
end.
