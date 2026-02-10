{ ============================================================================
  Section 12.2 : Déclaration et implémentation
  Description : Programme de test utilisant l'unité UNotifications
  Fichier source : 02-declaration-implementation.md
  ============================================================================ }
program TestNotifications;

{$mode objfpc}{$H+}

uses
  UNotifications;

// Accepte tout objet implémentant INotificateur (polymorphisme via interface)
procedure EnvoyerAlerte(Notif: INotificateur; const Alerte: string);  
begin  
  WriteLn('=== ', Notif.ObtenirNomService, ' ===');
  Notif.EnvoyerMessage(Alerte);
  WriteLn('');
end;

var
  Email: INotificateur;
  SMS: INotificateur;
begin
  // Création des notificateurs
  Email := TNotificateurEmail.Create('utilisateur@exemple.com');
  SMS := TNotificateurSMS.Create('+33 6 12 34 56 78');

  // Utilisation via l'interface
  EnvoyerAlerte(Email, 'Nouveau message dans votre boîte');
  EnvoyerAlerte(SMS, 'Code de vérification: 123456');

  // Pas besoin de Free : gestion automatique !
end.
