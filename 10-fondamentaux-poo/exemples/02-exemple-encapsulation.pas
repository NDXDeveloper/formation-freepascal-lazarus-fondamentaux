{ ============================================================================
  Section 10.2 : Encapsulation et visibilite
  Description : Classe TCompteBancaire illustrant l'encapsulation et la visibilite
  Fichier source : 02-encapsulation-visibilite.md
  ============================================================================ }
program ExempleEncapsulation;

{$mode objfpc}{$H+}

type
  TCompteBancaire = class
  private
    // Attributs privés : détails d'implémentation
    FNumeroCompte: string;
    FSolde: Real;
    FTitulaire: string;

    // Méthode privée : utilisée seulement en interne
    function VerifierMontantValide(Montant: Real): Boolean;

  public
    // Méthodes publiques : interface de la classe
    procedure Crediter(Montant: Real);
    procedure Debiter(Montant: Real);
    function ObtenirSolde: Real;
    procedure AfficherInfos;
    procedure DefinirTitulaire(const Nom: string);
  end;

// Implémentation de la méthode privée
function TCompteBancaire.VerifierMontantValide(Montant: Real): Boolean;  
begin  
  Result := Montant > 0;
end;

// Implémentation des méthodes publiques
procedure TCompteBancaire.Crediter(Montant: Real);  
begin  
  if VerifierMontantValide(Montant) then
  begin
    FSolde := FSolde + Montant;
    WriteLn('Crédit de ', Montant:0:2, ' € effectué.');
  end
  else
    WriteLn('Erreur : montant invalide.');
end;

procedure TCompteBancaire.Debiter(Montant: Real);  
begin  
  if not VerifierMontantValide(Montant) then
  begin
    WriteLn('Erreur : montant invalide.');
    Exit;
  end;

  if FSolde - Montant >= 0 then
  begin
    FSolde := FSolde - Montant;
    WriteLn('Débit de ', Montant:0:2, ' € effectué.');
  end
  else
    WriteLn('Erreur : solde insuffisant.');
end;

function TCompteBancaire.ObtenirSolde: Real;  
begin  
  Result := FSolde;
end;

procedure TCompteBancaire.AfficherInfos;  
begin  
  WriteLn('=== Informations du compte ===');
  WriteLn('Titulaire : ', FTitulaire);
  WriteLn('Numéro : ', FNumeroCompte);
  WriteLn('Solde : ', FSolde:0:2, ' €');
  WriteLn('==============================');
end;

procedure TCompteBancaire.DefinirTitulaire(const Nom: string);  
begin  
  if Length(Nom) > 0 then
    FTitulaire := Nom
  else
    WriteLn('Erreur : nom invalide.');
end;

// Programme principal
var
  MonCompte: TCompteBancaire;
begin
  MonCompte := TCompteBancaire.Create;

  // Utilisation de l'interface publique
  MonCompte.DefinirTitulaire('Jean Dupont');
  MonCompte.Crediter(1000);
  MonCompte.Debiter(250);
  MonCompte.AfficherInfos;

  // IMPOSSIBLE : les attributs privés ne sont pas accessibles
  // MonCompte.FSolde := 5000;  // Erreur de compilation !

  // On doit passer par les méthodes publiques
  WriteLn('Solde actuel : ', MonCompte.ObtenirSolde:0:2, ' €');

  MonCompte.Free;
end.
