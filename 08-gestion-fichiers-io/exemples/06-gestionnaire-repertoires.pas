{ ============================================================================
  Section 8.6 : Manipulation de repertoires
  Description : Gestionnaire interactif de repertoires
  Fichier source : 06-manipulation-repertoires.md
  Note : Programme interactif (ReadLn)
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionnaireRepertoires;

uses
  SysUtils;

procedure AfficherRepertoireCourant;
begin
  WriteLn('Répertoire actuel : ', GetCurrentDir);
end;

procedure ListerContenu;
var
  Info: TSearchRec;
  NbFichiers, NbDossiers: Integer;
begin
  NbFichiers := 0;
  NbDossiers := 0;

  WriteLn;
  WriteLn('=== Contenu du répertoire ===');

  if FindFirst('*', faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Name <> '.') and (Info.Name <> '..') then
        begin
          if (Info.Attr and faDirectory) = faDirectory then
          begin
            WriteLn('[DIR]  ', Info.Name);
            Inc(NbDossiers);
          end
          else
          begin
            WriteLn('[FILE] ', Info.Name:30, '  ', Info.Size:12, ' octets');
            Inc(NbFichiers);
          end;
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;

  WriteLn;
  WriteLn(NbDossiers, ' répertoire(s), ', NbFichiers, ' fichier(s)');
  WriteLn('=============================');
end;

procedure CreerNouveauRepertoire;
var
  Nom: string;
begin
  Write('Nom du nouveau répertoire : ');
  ReadLn(Nom);

  if Nom = '' then
  begin
    WriteLn('Nom invalide !');
    Exit;
  end;

  if DirectoryExists(Nom) then
  begin
    WriteLn('Ce répertoire existe déjà !');
    Exit;
  end;

  if CreateDir(Nom) then
    WriteLn('Répertoire "', Nom, '" créé avec succès !')
  else
    WriteLn('Erreur lors de la création du répertoire');
end;

procedure SupprimerRepertoire;
var
  Nom: string;
begin
  Write('Nom du répertoire à supprimer : ');
  ReadLn(Nom);

  if not DirectoryExists(Nom) then
  begin
    WriteLn('Ce répertoire n''existe pas !');
    Exit;
  end;

  if RemoveDir(Nom) then
    WriteLn('Répertoire "', Nom, '" supprimé avec succès !')
  else
    WriteLn('Erreur : Le répertoire n''est probablement pas vide');
end;

procedure ChangerRepertoire;
var
  Nom: string;
begin
  Write('Nom du répertoire (.. pour parent) : ');
  ReadLn(Nom);

  if SetCurrentDir(Nom) then
  begin
    WriteLn('Changement réussi !');
    AfficherRepertoireCourant;
  end
  else
    WriteLn('Erreur : Répertoire invalide ou inaccessible');
end;

procedure RechercherFichiers;
var
  Motif: string;
  Info: TSearchRec;
  Compteur: Integer;
begin
  Write('Motif de recherche (ex: *.txt) : ');
  ReadLn(Motif);

  if Motif = '' then
    Motif := '*';

  WriteLn;
  WriteLn('Résultats de la recherche :');

  Compteur := 0;

  if FindFirst(Motif, faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Attr and faDirectory) = 0 then
        begin
          WriteLn('  ', Info.Name, ' (', Info.Size, ' octets)');
          Inc(Compteur);
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;

  WriteLn;
  WriteLn(Compteur, ' fichier(s) trouvé(s)');
end;

var
  Choix: Integer;

begin
  WriteLn('================================');
  WriteLn('  GESTIONNAIRE DE RÉPERTOIRES  ');
  WriteLn('================================');

  repeat
    WriteLn;
    AfficherRepertoireCourant;
    WriteLn;
    WriteLn('1. Lister le contenu');
    WriteLn('2. Créer un répertoire');
    WriteLn('3. Supprimer un répertoire');
    WriteLn('4. Changer de répertoire');
    WriteLn('5. Rechercher des fichiers');
    WriteLn('0. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(Choix);
    WriteLn;

    case Choix of
      1: ListerContenu;
      2: CreerNouveauRepertoire;
      3: SupprimerRepertoire;
      4: ChangerRepertoire;
      5: RechercherFichiers;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;
end.
