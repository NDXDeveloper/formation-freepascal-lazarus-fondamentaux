{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Utilisation de caracteres speciaux dans les chaines
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program CaracteresSpeciaux;  
begin  
  WriteLn('Guillemet simple : ''');      // Pour afficher '
  WriteLn('Tabulation :'#9'texte');      // #9 = tabulation
  WriteLn('Sonnerie'#7);                 // #7 = bip sonore
  WriteLn('Symbole Euro : '#8364);       // Code Unicode
end.
