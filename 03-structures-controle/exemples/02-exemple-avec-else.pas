{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Case-of avec clause else pour gerer les valeurs non prevues
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program ExempleAvecElse;  
var  
  jour: Integer;
begin
  Write('Entrez un numero de jour (1-7) : ');
  ReadLn(jour);
  case jour of
    1: WriteLn('Lundi');
    2: WriteLn('Mardi');
    3: WriteLn('Mercredi');
    4: WriteLn('Jeudi');
    5: WriteLn('Vendredi');
    6: WriteLn('Samedi');
    7: WriteLn('Dimanche');
  else  { Dans case..of, le point-virgule avant else est autorise (contrairement a if..then..else) }
    WriteLn('Numero de jour invalide !');
  end;
end.
