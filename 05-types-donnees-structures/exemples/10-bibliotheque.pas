{ ============================================================================
  Section 5.10 : Definition de types personnalises (Type)
  Description : Systeme de gestion de bibliotheque avec types combines
  Fichier source : 10-definition-types-personnalises.md
  ============================================================================ }
program Bibliotheque;  
type  
  // Types de base
  TCategorie = (Roman, SF, Policier, Histoire, Science);
  TEtat = (Disponible, Emprunte, Perdu, EnReparation);

  // Types intervalles
  TISBNCode = String[13];  // String[N] : ShortString limitée à N caractères max
  TAnnee = 1900..2100;

  // Structures
  TLivre = record
    isbn: TISBNCode;
    titre: String;
    auteur: String;
    annee: TAnnee;
    categorie: TCategorie;
    etat: TEtat;
  end;

  TEmprunteur = record
    nom: String;
    prenom: String;
    numeroMembre: String;
  end;

  TEmprunt = record
    livre: TLivre;
    emprunteur: TEmprunteur;
    dateEmprunt: String;
    dateRetourPrevue: String;
  end;

  // Collections
  TCatalogue = array[1..1000] of TLivre;
  TListeEmprunts = array[1..100] of TEmprunt;

var
  catalogue: TCatalogue;
  emprunts: TListeEmprunts;
  nbLivres, nbEmprunts: Integer;

procedure AfficherLivre(livre: TLivre);  
begin  
  WriteLn('Titre : ', livre.titre);
  WriteLn('Auteur : ', livre.auteur);
  WriteLn('ISBN : ', livre.isbn);
  Write('État : ');
  case livre.etat of
    Disponible: WriteLn('Disponible');
    Emprunte: WriteLn('Emprunté');
    Perdu: WriteLn('Perdu');
    EnReparation: WriteLn('En réparation');
  end;
end;

begin
  // Exemple d'utilisation
  nbLivres := 1;
  catalogue[1].titre := 'Le Seigneur des Anneaux';
  catalogue[1].auteur := 'J.R.R. Tolkien';
  catalogue[1].isbn := '9780544003415';
  catalogue[1].annee := 1954;
  catalogue[1].categorie := SF;
  catalogue[1].etat := Disponible;

  AfficherLivre(catalogue[1]);
end.
