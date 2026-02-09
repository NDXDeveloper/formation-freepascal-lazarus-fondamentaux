{ ============================================================================
  Section 8.4 : Fichiers types
  Description : Gestion d'employes avec fichier type (File of TEmploye)
  Fichier source : 04-fichiers-types.md
  ============================================================================ }
program GestionEmployes;

type
  TEmploye = record
    Numero: Integer;
    Nom: string[40];
    Poste: string[30];
    Salaire: Real;
  end;

var
  F: File of TEmploye;  // Fichier typé : chaque Read/Write lit/écrit un TEmploye complet
  Employe: TEmploye;

procedure AjouterEmploye(Num: Integer; N: string; P: string; S: Real);
begin
  Employe.Numero := Num;
  Employe.Nom := N;
  Employe.Poste := P;
  Employe.Salaire := S;

  Write(F, Employe);
  WriteLn('Employé ajouté : ', N);
end;

procedure AfficherTous;
begin
  Seek(F, 0);  // Retour au debut

  WriteLn('=== LISTE DES EMPLOYÉS ===');
  WriteLn;

  while not EOF(F) do
  begin
    Read(F, Employe);
    WriteLn('N° ', Employe.Numero:4, ' | ',
            Employe.Nom:20, ' | ',
            Employe.Poste:15, ' | ',
            Employe.Salaire:8:2, ' €');
  end;
end;

begin
  // Creation du fichier
  Assign(F, 'employes.dat');
  Rewrite(F);

  // Ajout de quelques employes
  AjouterEmploye(1, 'Dupont Jean', 'Développeur', 35000);
  AjouterEmploye(2, 'Martin Sophie', 'Chef de projet', 45000);
  AjouterEmploye(3, 'Durand Pierre', 'Technicien', 28000);
  AjouterEmploye(4, 'Bernard Marie', 'Analyste', 38000);

  WriteLn;

  // Affichage de tous les employes
  AfficherTous;

  Close(F);

  // Nettoyage du fichier de donnees
  Assign(F, 'employes.dat');
  Erase(F);
end.
