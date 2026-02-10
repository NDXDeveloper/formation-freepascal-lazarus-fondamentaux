{ ============================================================================
  Section 5.9 : Types intervalle
  Description : Intervalles de caracteres pour la validation d'entree
  Fichier source : 09-types-intervalle.md
  ============================================================================ }
{$mode objfpc}{$H+}
{$R+}  // Active la vérification des intervalles à l'exécution
program IntervalleCaracteres;  
type  
  TChiffre = '0'..'9';
  TLettreMajuscule = 'A'..'Z';
  TLettreMinuscule = 'a'..'z';

function EstChiffre(c: Char): Boolean;  
var  
  chiffre: TChiffre;
begin
  try
    chiffre := c;
    EstChiffre := True;
  except
    EstChiffre := False;
  end;
end;

function EstLettre(c: Char): Boolean;  
begin  
  EstLettre := ((c >= 'A') and (c <= 'Z')) or
               ((c >= 'a') and (c <= 'z'));
end;

var
  caractere: Char;
begin
  Write('Entrez un caractère : ');
  ReadLn(caractere);

  if EstChiffre(caractere) then
    WriteLn('C''est un chiffre')
  else if EstLettre(caractere) then
    WriteLn('C''est une lettre')
  else
    WriteLn('C''est un caractère spécial');
end.
