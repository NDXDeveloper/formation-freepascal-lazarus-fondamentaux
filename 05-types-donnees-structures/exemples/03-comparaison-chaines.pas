{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Operateurs de comparaison de chaines (egalite, ordre, difference)
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ComparaisonChaines;  
var  
  nom1, nom2: String;
begin
  nom1 := 'Alice';
  nom2 := 'Bob';

  // Égalité
  if nom1 = nom2 then
    WriteLn('Les noms sont identiques')
  else
    WriteLn('Les noms sont différents');

  // Ordre alphabétique
  if nom1 < nom2 then
    WriteLn(nom1, ' vient avant ', nom2);  // Alice vient avant Bob

  // Différence
  if nom1 <> nom2 then
    WriteLn('Les noms ne sont pas les mêmes');
end.
