{ ============================================================================
  Section 4.3 : Parametres par valeur
  Description : Exemple complet de gestion de notes avec fonctions et procedures
  Fichier source : 03-parametres-par-valeur.md
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionNotes;

// Fonction avec un paramètre
function EstReussi(note: Real): Boolean;  
begin  
  Result := note >= 10;
end;

// Fonction avec plusieurs paramètres
function CalculerMoyenne(n1, n2, n3: Real): Real;  
begin  
  Result := (n1 + n2 + n3) / 3;
end;

// Procédure avec plusieurs paramètres
procedure AfficherResultat(nom: String; moyenne: Real);  
begin  
  WriteLn('Étudiant : ', nom);
  WriteLn('Moyenne : ', moyenne:0:2);
  if EstReussi(moyenne) then
    WriteLn('Résultat : ADMIS')
  else
    WriteLn('Résultat : REFUSÉ');
  WriteLn('---');
end;

var
  moy: Real;
begin
  moy := CalculerMoyenne(12.5, 15.0, 13.5);
  AfficherResultat('Dupont Jean', moy);

  moy := CalculerMoyenne(8.0, 9.5, 7.5);
  AfficherResultat('Martin Sophie', moy);
end.
