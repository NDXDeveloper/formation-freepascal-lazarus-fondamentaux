{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Jeu de devinette d'un nombre entre 1 et 100 avec Random
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program JeuDevinette;  
var  
  nombreSecret, proposition, tentatives: Integer;
begin
  Randomize;   { Initialise le generateur aleatoire (a appeler une seule fois) }
  nombreSecret := Random(100) + 1;  { Random(100) donne 0..99, donc +1 donne 1..100 }
  tentatives := 0;
  WriteLn('=== JEU DE DEVINETTE ===');
  WriteLn('J''ai choisi un nombre entre 1 et 100.');
  WriteLn('Essayez de le deviner !');
  WriteLn;
  repeat
    Write('Votre proposition : ');
    ReadLn(proposition);
    tentatives := tentatives + 1;
    if proposition < nombreSecret then
      WriteLn('C''est plus !')
    else if proposition > nombreSecret then
      WriteLn('C''est moins !')
    else
      WriteLn('Bravo ! Vous avez trouvé en ', tentatives, ' tentative(s) !');
    WriteLn;
  until proposition = nombreSecret;
end.
