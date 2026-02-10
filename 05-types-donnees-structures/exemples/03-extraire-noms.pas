{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Extraire le prenom et le nom d'une chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ExtraireNoms;  
var  
  nomComplet, prenom, nom: String;
  posEspace: Integer;
begin
  Write('Entrez prénom et nom : ');
  ReadLn(nomComplet);

  // Trouver la position de l'espace
  posEspace := Pos(' ', nomComplet);

  if posEspace > 0 then
  begin
    // Extraire le prénom (avant l'espace)
    prenom := Copy(nomComplet, 1, posEspace - 1);

    // Extraire le nom (après l'espace)
    nom := Copy(nomComplet, posEspace + 1, Length(nomComplet));

    WriteLn('Prénom : ', prenom);
    WriteLn('Nom : ', nom);
  end
  else
    WriteLn('Format incorrect : espace manquant');
end.
