{
  Section 20.2 - Points d'arrêt conditionnels
  Description : Programme de démonstration pour pratiquer les points d'arrêt
                conditionnels. Traite 1000 commandes dont certaines sont
                problématiques (prix négatif, montant excessif, etc.)
  Fichier source : 02-points-arret-conditionnels.md
}
program DemoPointsArret;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TCommande = record
    ID: Integer;
    Client: String;
    Montant: Double;
    Pays: String;
    Valide: Boolean;
  end;

var
  Commandes: array[1..1000] of TCommande;
  i: Integer;
  TotalValides, TotalInvalides: Integer;
  MontantTotal: Double;

procedure InitialiserCommandes;  
var  
  j: Integer;
begin
  for j := 1 to 1000 do
  begin
    Commandes[j].ID := j;
    Commandes[j].Client := 'Client_' + IntToStr(j);
    Commandes[j].Montant := (Random(10000) + 1) / 100.0;
    Commandes[j].Valide := True;

    case j mod 7 of
      0: Commandes[j].Pays := 'France';
      1: Commandes[j].Pays := 'Belgique';
      2: Commandes[j].Pays := 'Suisse';
      3: Commandes[j].Pays := 'Canada';
    else
      Commandes[j].Pays := 'Autre';
    end;

    { Injecter des anomalies pour le débogage }
    if j = 42 then
      Commandes[j].Montant := -15.50;  { Prix négatif ! }

    if j = 256 then
      Commandes[j].Montant := 99999.99;  { Montant excessif }

    if j = 500 then
    begin
      Commandes[j].Valide := False;
      Commandes[j].Client := '';  { Client vide }
    end;

    if j = 777 then
      Commandes[j].Montant := 0;  { Montant zéro }
  end;
end;

procedure TraiterCommande(const cmd: TCommande);  
begin  
  { Points d'arrêt conditionnels suggérés :
    - cmd.Montant < 0         (détecte les prix négatifs)
    - cmd.Montant > 50000     (détecte les montants excessifs)
    - not cmd.Valide          (détecte les commandes invalides)
    - cmd.Client = ''         (détecte les clients vides)
    - cmd.ID = 777            (arrêt sur une commande précise)
    - (cmd.Pays = 'France') and (cmd.Montant > 50) }

  if not cmd.Valide then
  begin
    Inc(TotalInvalides);
    Exit;
  end;

  if cmd.Montant < 0 then
  begin
    Inc(TotalInvalides);
    Exit;
  end;

  Inc(TotalValides);
  MontantTotal := MontantTotal + cmd.Montant;
end;

begin
  Randomize;
  WriteLn('=== Demo Points d''Arrêt Conditionnels ===');
  WriteLn;
  WriteLn('Ce programme traite 1000 commandes dont certaines sont anomales.');
  WriteLn('Ouvrez dans Lazarus et placez des points d''arrêt conditionnels');
  WriteLn('dans TraiterCommande pour détecter les anomalies.');
  WriteLn;

  InitialiserCommandes;

  TotalValides := 0;
  TotalInvalides := 0;
  MontantTotal := 0;

  for i := 1 to 1000 do
    TraiterCommande(Commandes[i]);

  WriteLn('Résultats :');
  WriteLn('  Commandes valides   : ', TotalValides);
  WriteLn('  Commandes invalides : ', TotalInvalides);
  WriteLn('  Montant total       : ', MontantTotal:0:2, ' EUR');
  WriteLn;

  { Afficher les anomalies connues }
  WriteLn('Anomalies injectées (vérifiables avec breakpoints) :');
  WriteLn('  #42  : Montant négatif (', Commandes[42].Montant:0:2, ')');
  WriteLn('  #256 : Montant excessif (', Commandes[256].Montant:0:2, ')');
  WriteLn('  #500 : Commande invalide, client vide');
  WriteLn('  #777 : Montant zéro (', Commandes[777].Montant:0:2, ')');
end.
