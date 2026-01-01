/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmailTranslations} from '../types';

export const de: EmailTranslations = {
	passwordReset: {
		subject: 'Setze dein Fluxer-Passwort zurück',
		body: `Hallo {username},

du hast angefordert, das Passwort für dein Fluxer-Konto zurückzusetzen. Bitte folge dem Link unten, um ein neues Passwort festzulegen:

{resetUrl}

Wenn du diese Zurücksetzung nicht angefordert hast, kannst du diese E-Mail sicher ignorieren.

Dieser Link läuft in 1 Stunde ab.

- Dein Fluxer-Team`,
	},
	emailVerification: {
		subject: 'Bestätige deine Fluxer-E-Mail-Adresse',
		body: `Hallo {username},

bitte bestätige deine E-Mail-Adresse für dein Fluxer-Konto, indem du auf den folgenden Link klickst:

{verifyUrl}

Wenn du kein Fluxer-Konto erstellt hast, kannst du diese E-Mail sicher ignorieren.

Dieser Link läuft in 24 Stunden ab.

- Dein Fluxer-Team`,
	},
	ipAuthorization: {
		subject: 'Login von neuer IP-Adresse autorisieren',
		body: `Hallo {username},

wir haben einen Anmeldeversuch bei deinem Fluxer-Konto von einer neuen IP-Adresse festgestellt:

IP-Adresse: {ipAddress}
Ort: {location}

Wenn du das warst, autorisiere diese IP-Adresse bitte über den folgenden Link:

{authUrl}

Wenn du nicht versucht hast, dich anzumelden, ändere bitte umgehend dein Passwort.

Dieser Autorisierungslink läuft in 30 Minuten ab.

- Dein Fluxer-Team`,
	},
	accountDisabledSuspicious: {
		subject: 'Dein Fluxer-Konto wurde vorübergehend deaktiviert',
		body: `Hallo {username},

dein Fluxer-Konto wurde aufgrund verdächtiger Aktivitäten vorübergehend deaktiviert.

{reason, select,
	null {}
	other {Grund: {reason}

}}Um den Zugriff auf dein Konto wiederzuerlangen, musst du dein Passwort zurücksetzen:

{forgotUrl}

Nachdem du dein Passwort zurückgesetzt hast, kannst du dich wieder anmelden.

Wenn du glaubst, dass diese Maßnahme irrtümlich erfolgt ist, kontaktiere bitte unser Support-Team.

- Fluxer Safety Team`,
	},
	accountTempBanned: {
		subject: 'Dein Fluxer-Konto wurde vorübergehend gesperrt',
		body: `Hallo {username},

dein Fluxer-Konto wurde wegen Verstößen gegen unsere Nutzungsbedingungen oder Community-Richtlinien vorübergehend gesperrt.

Dauer: {durationHours, plural,
	=1 {1 Stunde}
	other {# Stunden}
}
Gesperrt bis: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Grund: {reason}}
}

In dieser Zeit hast du keinen Zugriff auf dein Konto.

Wir empfehlen dir, unsere folgenden Dokumente zu überprüfen:
- Nutzungsbedingungen: {termsUrl}
- Community-Richtlinien: {guidelinesUrl}

Wenn du glaubst, dass diese Entscheidung falsch oder ungerechtfertigt ist, kannst du eine Beschwerde an appeals@fluxer.app von dieser E-Mail-Adresse senden. Bitte erkläre klar und ausführlich, warum du glaubst, dass die Entscheidung falsch war. Wir werden deine Beschwerde prüfen und dir unsere Entscheidung mitteilen.

- Fluxer Safety Team`,
	},
	accountScheduledDeletion: {
		subject: 'Dein Fluxer-Konto ist zur Löschung vorgesehen',
		body: `Hallo {username},

dein Fluxer-Konto wurde aufgrund von Verstößen gegen unsere Nutzungsbedingungen oder Community-Richtlinien zur dauerhaften Löschung vorgesehen.

Geplantes Löschdatum: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Grund: {reason}}
}

Dies ist eine schwerwiegende Vollzugsmaßnahme. Deine Kontodaten werden am geplanten Datum dauerhaft gelöscht.

Wir empfehlen dir, unsere folgenden Dokumente zu überprüfen:
- Nutzungsbedingungen: {termsUrl}
- Community-Richtlinien: {guidelinesUrl}

EINSICHTS- UND BESCHWERDEVERFAHREN:
Wenn du glaubst, dass diese Entscheidung falsch oder ungerechtfertigt ist, hast du 30 Tage Zeit, eine Beschwerde an appeals@fluxer.app von dieser E-Mail-Adresse zu senden.

In deiner Beschwerde solltest du:
- Klar erläutern, warum du die Entscheidung für falsch oder ungerechtfertigt hältst
- Alle relevanten Belege oder Kontextinformationen anführen

Ein Mitglied des Fluxer Safety Teams wird deine Beschwerde prüfen und die geplante Löschung gegebenenfalls aussetzen, bis eine endgültige Entscheidung getroffen wurde.

- Fluxer Safety Team`,
	},
	selfDeletionScheduled: {
		subject: 'Die Löschung deines Fluxer-Kontos wurde geplant',
		body: `Hallo {username},

es tut uns leid, dass du gehst! Die Löschung deines Fluxer-Kontos wurde geplant.

Geplantes Löschdatum: {deletionDate, date, full} {deletionDate, time, short}

WICHTIG: Du kannst diese Löschung jederzeit vor {deletionDate, date, full} {deletionDate, time, short} widerrufen, indem du dich einfach wieder in dein Konto einloggst.

BEVOR DU GEHST:
Dein Datenschutz-Dashboard in den Benutzereinstellungen ermöglicht dir:
- Deine Nachrichten auf der Plattform zu löschen
- Wichtige Daten vor deinem Abschied zu exportieren

Bitte beachte: Sobald dein Konto gelöscht wurde, gibt es keine Möglichkeit mehr, deine Nachrichten zu löschen. Wenn du deine Nachrichten entfernen möchtest, tue dies bitte über das Datenschutz-Dashboard, bevor die Kontolöschung abgeschlossen ist.

Wenn du deine Meinung änderst, logge dich einfach wieder ein, um die Löschung zu stornieren.

- Dein Fluxer-Team`,
	},
	inactivityWarning: {
		subject: 'Dein Fluxer-Konto wird wegen Inaktivität gelöscht',
		body: `Hallo {username},

wir haben festgestellt, dass du dich seit über 2 Jahren nicht mehr in dein Fluxer-Konto eingeloggt hast.

Letzte Anmeldung: {lastActiveDate, date, full} {lastActiveDate, time, short}

Im Rahmen unserer Richtlinie zur Datenspeicherung werden inaktive Konten automatisch zur Löschung vorgemerkt. Dein Konto wird dauerhaft gelöscht am:

Geplantes Löschdatum: {deletionDate, date, full} {deletionDate, time, short}

SO BEHÄLTST DU DEIN KONTO:
Logge dich einfach vor dem Löschdatum unter {loginUrl} in dein Konto ein, um diese automatische Löschung zu verhindern. Weitere Schritte sind nicht erforderlich.

WAS PASSIERT, WENN DU DICH NICHT EINLOGGST:
- Dein Konto und alle zugehörigen Daten werden dauerhaft gelöscht
- Deine Nachrichten werden anonymisiert (als „Gelöschter Benutzer“ gekennzeichnet)
- Diese Aktion kann nicht rückgängig gemacht werden

MÖCHTEST DU DEINE NACHRICHTEN LÖSCHEN?
Wenn du deine Nachrichten löschen möchtest, bevor dein Konto gelöscht wird, logge dich bitte ein und nutze das Datenschutz-Dashboard in den Benutzereinstellungen.

Wir würden uns freuen, dich wieder bei Fluxer zu sehen!

- Dein Fluxer-Team`,
	},
	harvestCompleted: {
		subject: 'Dein Fluxer-Datenexport ist bereit',
		body: `Hallo {username},

dein Datenexport wurde abgeschlossen und steht jetzt zum Download bereit!

Exportübersicht:
- Gesamte Anzahl an Nachrichten: {totalMessages, number}
- Dateigröße: {fileSizeMB} MB
- Format: ZIP-Archiv mit JSON-Dateien

Lade deine Daten herunter: {downloadUrl}

WICHTIG: Dieser Download-Link läuft am {expiresAt, date, full} {expiresAt, time, short} ab.

Folgendes ist in deinem Export enthalten:
- Alle deine Nachrichten, nach Kanälen organisiert
- Kanal-Metadaten
- Dein Benutzerprofil und Kontoinformationen
- Guild-Mitgliedschaften und Einstellungen
- Anmeldesitzungen und Sicherheitsinformationen

Die Daten sind im JSON-Format organisiert, um die Verarbeitung und Analyse zu erleichtern.

Wenn du Fragen zu deinem Datenexport hast, kontaktiere bitte support@fluxer.app

- Dein Fluxer-Team`,
	},
	unbanNotification: {
		subject: 'Die Sperre deines Fluxer-Kontos wurde aufgehoben',
		body: `Hallo {username},

gute Nachrichten! Die Sperre deines Fluxer-Kontos wurde aufgehoben.

Grund: {reason}

Du kannst dich jetzt wieder in dein Konto einloggen und Fluxer weiter nutzen.

- Fluxer Safety Team`,
	},
	scheduledDeletionNotification: {
		subject: 'Dein Fluxer-Konto ist zur Löschung vorgemerkt',
		body: `Hallo {username},

dein Fluxer-Konto wurde zur dauerhaften Löschung vorgemerkt.

Geplantes Löschdatum: {deletionDate, date, full} {deletionDate, time, short}
Grund: {reason}

Dies ist eine schwerwiegende Vollzugsmaßnahme. Deine Kontodaten werden am geplanten Datum dauerhaft gelöscht.

Wenn du glaubst, dass diese Entscheidung falsch ist, kannst du eine Beschwerde an appeals@fluxer.app von dieser E-Mail-Adresse senden.

- Fluxer Safety Team`,
	},
	giftChargebackNotification: {
		subject: 'Dein Fluxer Premium-Geschenk wurde widerrufen',
		body: `Hallo {username},

wir möchten dich darüber informieren, dass das Fluxer Premium-Geschenk, das du eingelöst hast, aufgrund eines Zahlungsstreits (Chargeback) des ursprünglichen Käufers widerrufen wurde.

Deine Premium-Vorteile wurden von deinem Konto entfernt. Diese Maßnahme wurde ergriffen, weil die Zahlung für das Geschenk angefochten und rückgängig gemacht wurde.

Wenn du Fragen dazu hast, kontaktiere bitte support@fluxer.app.

- Dein Fluxer-Team`,
	},
	reportResolved: {
		subject: 'Deine Fluxer-Meldung wurde überprüft',
		body: `Hallo {username},

deine Meldung (ID: {reportId}) wurde von unserem Safety Team geprüft.

Antwort vom Safety Team:
{publicComment}

Vielen Dank, dass du dabei hilfst, Fluxer für alle sicher zu halten. Wir nehmen alle Meldungen ernst und schätzen deinen Beitrag zu unserer Community.

Wenn du Fragen oder Bedenken hinsichtlich dieser Entscheidung hast, kontaktiere bitte safety@fluxer.app.

- Fluxer Safety Team`,
	},
	dsaReportVerification: {
		subject: 'Bestätige deine E-Mail für eine DSA-Meldung',
		body: `Hallo,

Verwende den folgenden Bestätigungscode, um deine Meldung gemäß dem Digital Services Act auf Fluxer einzureichen:

{code}

Dieser Code läuft ab am {expiresAt, date, full} {expiresAt, time, short}.

Wenn du dies nicht angefordert hast, kannst du diese E-Mail ignorieren.

- Fluxer Safety Team`,
	},
	registrationApproved: {
		subject: 'Deine Fluxer-Registrierung wurde genehmigt',
		body: `Hallo {username},

gute Nachrichten! Deine Registrierung bei Fluxer wurde genehmigt.

Du kannst dich jetzt in der Fluxer-App anmelden unter:
{channelsUrl}

Willkommen in der Fluxer-Community!

- Dein Fluxer-Team`,
	},
	emailChangeRevert: {
		subject: 'Deine Fluxer-E-Mail wurde geändert',
		body: `Hallo {username},

Die E-Mail deines Fluxer-Kontos wurde auf {newEmail} geändert.

Wenn du diese Änderung vorgenommen hast, musst du nichts weiter tun. Falls nicht, kannst du sie über diesen Link rückgängig machen und dein Konto sichern:

{revertUrl}

Dadurch wird deine vorherige E-Mail wiederhergestellt, du wirst überall abgemeldet, verknüpfte Telefonnummern werden entfernt, MFA wird deaktiviert und ein neues Passwort ist erforderlich.

- Fluxer Sicherheitsteam`,
	},
};
