{ ============================================================================
  Section 5.9 : Types intervalle
  Description : Gestion d'horaires avec types intervalle heure/minute/seconde
  Fichier source : 09-types-intervalle.md
  ============================================================================ }
{$mode objfpc}{$H+}
program GestionHeures;
type
  THeure = 0..23;
  TMinute = 0..59;
  TSeconde = 0..59;

  THoraire = record
    heure: THeure;
    minute: TMinute;
    seconde: TSeconde;
  end;

procedure AfficherHoraire(h: THoraire);
begin
  // Afficher avec zéros de remplissage
  if h.heure < 10 then Write('0');
  Write(h.heure);
  Write(':');
  if h.minute < 10 then Write('0');
  Write(h.minute);
  Write(':');
  if h.seconde < 10 then Write('0');
  WriteLn(h.seconde);
end;

function AjouterSecondes(h: THoraire; sec: Integer): THoraire;
var
  totalSecondes: Integer;
begin
  // Convertir en secondes totales
  totalSecondes := h.heure * 3600 + h.minute * 60 + h.seconde + sec;

  // Gérer le dépassement de 24h
  totalSecondes := totalSecondes mod 86400;  // 86400 = 24 * 60 * 60

  // Reconvertir
  Result.heure := totalSecondes div 3600;
  totalSecondes := totalSecondes mod 3600;
  Result.minute := totalSecondes div 60;
  Result.seconde := totalSecondes mod 60;

  AjouterSecondes := Result;
end;

var
  maintenant: THoraire;
  plusTard: THoraire;
begin
  WriteLn('Entrez l''heure actuelle :');
  Write('  Heure (0-23) : ');
  ReadLn(maintenant.heure);
  Write('  Minute (0-59) : ');
  ReadLn(maintenant.minute);
  Write('  Seconde (0-59) : ');
  ReadLn(maintenant.seconde);

  Write('Heure actuelle : ');
  AfficherHoraire(maintenant);

  plusTard := AjouterSecondes(maintenant, 3600);  // +1 heure
  Write('Dans 1 heure : ');
  AfficherHoraire(plusTard);
end.
