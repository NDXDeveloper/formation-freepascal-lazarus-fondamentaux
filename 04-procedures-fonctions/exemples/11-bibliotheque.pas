{ ============================================================================
  Section 4.11 : Organisation modulaire du code
  Description : Systeme complet de gestion de bibliotheque modulaire
  Fichier source : 11-organisation-modulaire-code.md
  ============================================================================ }
{$mode objfpc}{$H+}
program Bibliotheque;

uses
  SysUtils;

// ============================================================================
// TYPES ET CONSTANTES
// ============================================================================

const
  MAX_LIVRES = 100;

type
  TLivre = record
    Titre: String;
    Auteur: String;
    Annee: Integer;
    Disponible: Boolean;
  end;

var
  livres: array[1..MAX_LIVRES] of TLivre;
  nbLivres: Integer;

// ============================================================================
// MODULE : GESTION DES LIVRES
// ============================================================================

function CreerLivre(const titre, auteur: String; annee: Integer): TLivre;  
begin  
  Result.Titre := titre;
  Result.Auteur := auteur;
  Result.Annee := annee;
  Result.Disponible := True;
end;

function AjouterLivre(const livre: TLivre): Boolean;  
begin  
  if nbLivres < MAX_LIVRES then
  begin
    Inc(nbLivres);  // Inc incrémente : équivalent de nbLivres := nbLivres + 1
    livres[nbLivres] := livre;
    Result := True;
  end
  else
    Result := False;
end;

function RechercherLivre(const titre: String): Integer;  
var  
  i: Integer;
begin
  Result := -1;
  for i := 1 to nbLivres do
    if LowerCase(livres[i].Titre) = LowerCase(titre) then
    begin
      Result := i;
      Exit;
    end;
end;

procedure EmprunterLivre(index: Integer);  
begin  
  if (index > 0) and (index <= nbLivres) then
    livres[index].Disponible := False;
end;

procedure RetournerLivre(index: Integer);  
begin  
  if (index > 0) and (index <= nbLivres) then
    livres[index].Disponible := True;
end;

// ============================================================================
// MODULE : AFFICHAGE
// ============================================================================

procedure AfficherSeparateur;  
begin  
  WriteLn('------------------------------------------------');
end;

procedure AfficherEntete;  
begin  
  AfficherSeparateur;
  WriteLn('SYSTÈME DE GESTION DE BIBLIOTHÈQUE');
  AfficherSeparateur;
end;

procedure AfficherLivre(const livre: TLivre);  
var  
  statut: String;
begin
  if livre.Disponible then
    statut := 'Disponible'
  else
    statut := 'Emprunté';

  WriteLn(Format('"%s" par %s (%d) - %s',
          [livre.Titre, livre.Auteur, livre.Annee, statut]));
end;

procedure AfficherTousLesLivres;  
var  
  i: Integer;
begin
  WriteLn(Format('Total : %d livre(s)', [nbLivres]));
  AfficherSeparateur;

  for i := 1 to nbLivres do
  begin
    Write(i:2, '. ');
    AfficherLivre(livres[i]);
  end;
end;

procedure AfficherMenu;  
begin  
  WriteLn;
  WriteLn('=== MENU ===');
  WriteLn('1. Ajouter un livre');
  WriteLn('2. Lister les livres');
  WriteLn('3. Emprunter un livre');
  WriteLn('4. Retourner un livre');
  WriteLn('0. Quitter');
  Write('Votre choix : ');
end;

// ============================================================================
// MODULE : SAISIE UTILISATEUR
// ============================================================================

function LireTexte(const prompt: String): String;  
begin  
  Write(prompt, ' : ');
  ReadLn(Result);
  Result := Trim(Result);
end;

function LireEntier(const prompt: String): Integer;  
var  
  texte: String;
begin
  Write(prompt, ' : ');
  ReadLn(texte);
  Result := StrToIntDef(texte, 0);
end;

function LireChoix: Integer;  
begin  
  Result := LireEntier('Choix');
end;

// ============================================================================
// MODULE : ACTIONS MÉTIER
// ============================================================================

procedure ActionAjouterLivre;  
var  
  titre, auteur: String;
  annee: Integer;
  livre: TLivre;
begin
  WriteLn('=== AJOUTER UN LIVRE ===');
  titre := LireTexte('Titre');
  auteur := LireTexte('Auteur');
  annee := LireEntier('Année');

  livre := CreerLivre(titre, auteur, annee);

  if AjouterLivre(livre) then
    WriteLn('Livre ajouté avec succès !')
  else
    WriteLn('Erreur : bibliothèque pleine');
end;

procedure ActionListerLivres;  
begin  
  WriteLn('=== LISTE DES LIVRES ===');
  if nbLivres = 0 then
    WriteLn('Aucun livre dans la bibliothèque')
  else
    AfficherTousLesLivres;
end;

procedure ActionEmprunter;  
var  
  titre: String;
  index: Integer;
begin
  WriteLn('=== EMPRUNTER UN LIVRE ===');
  titre := LireTexte('Titre du livre');
  index := RechercherLivre(titre);

  if index = -1 then
    WriteLn('Livre non trouvé')
  else if not livres[index].Disponible then
    WriteLn('Livre déjà emprunté')
  else
  begin
    EmprunterLivre(index);
    WriteLn('Emprunt enregistré');
  end;
end;

procedure ActionRetourner;  
var  
  titre: String;
  index: Integer;
begin
  WriteLn('=== RETOURNER UN LIVRE ===');
  titre := LireTexte('Titre du livre');
  index := RechercherLivre(titre);

  if index = -1 then
    WriteLn('Livre non trouvé')
  else if livres[index].Disponible then
    WriteLn('Le livre n''est pas emprunté')
  else
  begin
    RetournerLivre(index);
    WriteLn('Retour enregistré');
  end;
end;

// ============================================================================
// PROGRAMME PRINCIPAL
// ============================================================================

var
  choix: Integer;
begin
  nbLivres := 0;
  AfficherEntete;

  repeat
    AfficherMenu;
    choix := LireChoix;
    WriteLn;

    case choix of
      1: ActionAjouterLivre;
      2: ActionListerLivres;
      3: ActionEmprunter;
      4: ActionRetourner;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide');
    end;
  until choix = 0;
end.
