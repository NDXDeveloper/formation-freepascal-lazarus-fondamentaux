{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Menu formate avec caracteres Unicode (bordures)
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program MenuFormate;  
begin  
  WriteLn;
  WriteLn('+================================+');
  WriteLn('|      MENU PRINCIPAL            |');
  WriteLn('+================================+');
  WriteLn('|                                |');
  WriteLn('|  1. Nouvelle partie            |');
  WriteLn('|  2. Charger une partie         |');
  WriteLn('|  3. Options                    |');
  WriteLn('|  4. Crédits                    |');
  WriteLn('|  0. Quitter                    |');
  WriteLn('|                                |');
  WriteLn('+================================+');
  WriteLn;
  Write('Votre choix : ');
end.
