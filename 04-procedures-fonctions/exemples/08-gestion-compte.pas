{ ============================================================================
  Section 4.8 : Variables locales vs globales
  Description : Gestion d'un compte bancaire avec variables globales et locales
  Fichier source : 08-variables-locales-vs-globales.md
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionCompte;

// Variables globales (état du compte)
var
  solde: Real;
  nomTitulaire: String;
  numeroCompte: String;

// Initialiser le compte
procedure InitialiserCompte(nom, numero: String);  
begin  
  nomTitulaire := nom;
  numeroCompte := numero;
  solde := 0.0;
end;

// Déposer de l'argent
procedure Deposer(montant: Real);  
var  
  frais: Real;  // Variable locale
begin
  frais := 0.0;  // Pas de frais pour un dépôt
  solde := solde + montant - frais;
  WriteLn('Dépôt de ', montant:0:2, ' €. Nouveau solde : ', solde:0:2, ' €');
end;

// Retirer de l'argent
function Retirer(montant: Real): Boolean;  
var  
  frais: Real;  // Variable locale
begin
  frais := 1.50;  // Frais de retrait

  if (montant + frais) <= solde then
  begin
    solde := solde - montant - frais;
    WriteLn('Retrait de ', montant:0:2, ' €. Nouveau solde : ', solde:0:2, ' €');
    Result := True;
  end
  else
  begin
    WriteLn('Solde insuffisant !');
    Result := False;
  end;
end;

// Afficher les informations du compte
procedure AfficherInfos;  
begin  
  WriteLn('=== Informations du compte ===');
  WriteLn('Titulaire : ', nomTitulaire);
  WriteLn('Numéro : ', numeroCompte);
  WriteLn('Solde : ', solde:0:2, ' €');
  WriteLn('==============================');
end;

// Programme principal
begin
  InitialiserCompte('Dupont Jean', 'FR76 1234 5678 9012');
  AfficherInfos;

  Deposer(1000.00);
  Deposer(500.00);

  Retirer(200.00);
  Retirer(2000.00);  // Échouera

  AfficherInfos;
end.
