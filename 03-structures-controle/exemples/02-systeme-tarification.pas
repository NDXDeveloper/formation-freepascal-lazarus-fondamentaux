{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Systeme de tarification par tranche d'age avec intervalles
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program SystemeTarification;
var
  age: Integer;
  tarif: Real;
begin
  Write('Entrez votre age : ');
  ReadLn(age);
  case age of
    0..4:
      begin
        tarif := 0;
        WriteLn('Categorie : Gratuit (moins de 5 ans)');
      end;
    5..12:
      begin
        tarif := 5.50;
        WriteLn('Categorie : Enfant');
      end;
    13..17:
      begin
        tarif := 8.00;
        WriteLn('Categorie : Adolescent');
      end;
    18..64:
      begin
        tarif := 12.50;
        WriteLn('Categorie : Adulte');
      end;
    65..120:
      begin
        tarif := 9.00;
        WriteLn('Categorie : Senior');
      end;
  else
    begin
      tarif := 0;
      WriteLn('Age invalide !');
    end;
  end;
  if (age >= 0) and (age <= 120) then
    WriteLn('Tarif : ', tarif:0:2, ' euros');
end.
