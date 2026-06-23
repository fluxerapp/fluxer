// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_DE_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Dein {product_name}-Konto wurde vorübergehend deaktiviert",
		"body": "Hallo {username},\n\nwir haben dein {product_name}-Konto vorübergehend deaktiviert, da wir verdächtige Aktivitäten festgestellt haben.\n\n{reason, select,\n  null {}\n  other {Grund: {reason}}\n}\n\nUm wieder Zugriff auf dein Konto zu erhalten, musst du dein Passwort zurücksetzen:\n\n{forgotUrl}\n\nNachdem du dein Passwort zurückgesetzt hast, kannst du dich wieder anmelden.\n\nWenn du glaubst, dass dies ein Fehler war, kontaktiere bitte unser Support-Team.\n\n– {product_name}-Sicherheitsteam"
	},
	"account_scheduled_deletion": {
		"subject": "Dein {product_name}-Konto wird dauerhaft gelöscht",
		"body": "Hallo {username},\n\ndein {product_name}-Konto wurde aufgrund von Verstößen gegen unsere Nutzungsbedingungen oder Community-Richtlinien zur dauerhaften Löschung vorgemerkt.\n\nGeplante Löschung: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Grund: {reason}}\n}\n\nDies ist eine ernsthafte Maßnahme. Deine Kontodaten werden am geplanten Datum dauerhaft gelöscht.\n\nBitte lies dir Folgendes durch:\n- Nutzungsbedingungen: {termsUrl}\n- Community-Richtlinien: {guidelinesUrl}\n\nEinspruchsverfahren:\nWenn du glaubst, dass diese Entscheidung falsch oder ungerechtfertigt war, hast du 60 Tage Zeit, Einspruch einzulegen. Sende eine E-Mail von dieser E-Mail-Adresse an {appeals_email}.\n\nIn deinem Einspruch:\n- Erkläre klar, warum du glaubst, dass die Entscheidung falsch oder ungerechtfertigt war\n- Füge alle relevanten Beweise oder Kontextinformationen bei\n\nEin Mitglied des {product_name}-Sicherheitsteams wird deinen Einspruch prüfen und die ausstehende Löschung möglicherweise pausieren, bis eine endgültige Entscheidung getroffen wurde.\n\n– {product_name}-Sicherheitsteam"
	},
	"account_temp_banned": {
		"subject": "Dein {product_name}-Konto wurde vorübergehend gesperrt",
		"body": "Hallo {username},\n\ndein {product_name}-Konto wurde vorübergehend gesperrt, weil du gegen unsere Nutzungsbedingungen oder Community-Richtlinien verstoßen hast.\n\nDauer: {durationHours, plural,\n  one {1 Stunde}\n  other {# Stunden}\n}\nGesperrt bis: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Grund: {reason}}\n}\n\nWährend dieser Zeit kannst du nicht auf dein Konto zugreifen.\n\nBitte lies dir Folgendes durch:\n- Nutzungsbedingungen: {termsUrl}\n- Community-Richtlinien: {guidelinesUrl}\n\nWenn du glaubst, dass diese Entscheidung falsch oder ungerechtfertigt war, kannst du Einspruch einlegen. Sende eine E-Mail von dieser E-Mail-Adresse an {appeals_email} und erkläre klar, warum du die Entscheidung für falsch hältst. Wir werden deinen Einspruch prüfen und dir unsere Entscheidung mitteilen.\n\n– {product_name}-Sicherheitsteam"
	},
	"donation_confirmation": {
		"subject": "Vielen Dank für deine {product_name}-Spende",
		"body": "Hallo,\n\nvielen Dank für deine Spende an {product_name}! Deine {interval, select,\n  month {wiederkehrende Spende}\n  year {wiederkehrende Spende}\n  other {einmalige Spende}\n} wurde erfolgreich {interval, select,\n  month {eingerichtet}\n  year {eingerichtet}\n  other {verarbeitet}\n}.\n\nDetails zur Spende:\nBetrag: {amount} {currency} {interval, select,\n  month {pro Monat}\n  year {pro Jahr}\n  other {}\n}\n\nStripe sendet dir in Kürze eine separate Quittung mit deiner Rechnungs-PDF per E-Mail. Diese enthält alle Zahlungsdetails und kann für Steuerzwecke verwendet werden.\n\nDu kannst deine Spendenhistorie einsehen, Rechnungen herunterladen {interval, select,\n  month {und dein Abonnement jederzeit über diesen Link verwalten oder kündigen}\n  year {und dein Abonnement jederzeit über diesen Link verwalten oder kündigen}\n  other {und zukünftige Spenden verwalten}\n}:\n\n{manageUrl}\n\nDeine Unterstützung hilft, {product_name} am Laufen zu halten. Vielen Dank!\n\n– {product_name} Team"
	},
	"donation_magic_link": {
		"subject": "Verwalte deine {product_name}-Spenden",
		"body": "Hallo,\n\nklicke auf den Link unten, um auf dein Spenderportal zuzugreifen:\n\n{manageUrl}\n\nIm Portal kannst du Abonnements verwalten, Rechnungen herunterladen und deine Spendenhistorie einsehen.\n\nDieser Link läuft am {expiresAt, date, full} um {expiresAt, time, short} ab.\n\nWenn du diesen Link nicht angefordert hast, kannst du diese E-Mail ignorieren.\n\n– {product_name} Team"
	},
	"dsa_report_verification": {
		"subject": "Bestätige deine E-Mail für einen DSA-Bericht",
		"body": "Hallo,\n\nverwende den untenstehenden Bestätigungscode, um deinen DSA-Bericht auf {product_name} einzureichen:\n\n{code}\n\nDieser Code läuft am {expiresAt, date, full} um {expiresAt, time, short} ab.\n\nWenn du dies nicht angefordert hast, kannst du diese E-Mail ignorieren.\n\n– {product_name}-Sicherheitsteam"
	},
	"email_change_new": {
		"subject": "Bestätige deine neue {product_name}-E-Mail",
		"body": "Hallo {username},\n\ngib diesen Code in der App ein, um deine neue {product_name}-E-Mail zu bestätigen:\n\n{code}\n\nDieser Code läuft am {expiresAt, date, full} um {expiresAt, time, short} ab.\n\nWenn du dies nicht angefordert hast, kannst du diese E-Mail ignorieren.\n\n– {product_name} Team"
	},
	"email_change_original": {
		"subject": "Bestätige deine {product_name}-E-Mail-Änderung",
		"body": "Hallo {username},\n\nwir haben eine Anfrage zur Änderung der E-Mail-Adresse deines {product_name}-Kontos erhalten.\n\nUm diese Änderung zu bestätigen, gib diesen Code in der App ein:\n\n{code}\n\nDieser Code läuft am {expiresAt, date, full} um {expiresAt, time, short} ab.\n\nWenn du dies nicht angefordert hast, sichere dein Konto bitte sofort.\n\n– {product_name} Team"
	},
	"email_change_revert": {
		"subject": "Deine {product_name}-E-Mail wurde geändert",
		"body": "Hallo {username},\n\ndie E-Mail-Adresse deines {product_name}-Kontos wurde zu {newEmail} geändert.\n\nWenn du diese Änderung vorgenommen hast, ist keine weitere Aktion erforderlich. Wenn nicht, kannst du die Änderung rückgängig machen und dein Konto über diesen Link sichern:\n\n{revertUrl}\n\nDadurch wird deine vorherige E-Mail wiederhergestellt, du wirst überall abgemeldet, verknüpfte Telefonnummern werden entfernt, MFA wird deaktiviert und du musst ein neues Passwort festlegen.\n\n– {product_name}-Sicherheitsteam"
	},
	"email_verification": {
		"subject": "Bestätige deine {product_name}-E-Mail-Adresse",
		"body": "Hallo {username},\n\nbitte bestätige die E-Mail-Adresse für dein {product_name}-Konto, indem du auf den untenstehenden Link klickst:\n\n{verifyUrl}\n\nWenn du kein {product_name}-Konto erstellt hast, kannst du diese E-Mail ignorieren.\n\nDieser Link ist 24 Stunden gültig.\n\n– {product_name} Team"
	},
	"gift_chargeback_notification": {
		"subject": "Vorteile deines eingelösten Geschenks wurden entfernt",
		"body": "Hallo {username},\n\nein von dir eingelöster Geschenkcode wurde ursprünglich von jemand anderem bezahlt. Diese Zahlung wurde inzwischen rückgängig gemacht (eine Rückbuchung).\n\nAus diesem Grund haben wir die Vorteile entfernt, die deinem Konto hinzugefügt wurden, als du das Geschenk eingelöst hast.\n\nWenn du glaubst, dass dies ein Fehler ist, kontaktiere bitte unser Support-Team und gib alle Details an, die du über den Geschenkcode und den Zeitpunkt der Einlösung hast.\n\n– {product_name} Team"
	},
	"harvest_completed": {
		"subject": "Dein {product_name}-Datenexport steht zum Herunterladen bereit",
		"body": "Hallo {username},\n\ndein Datenexport ist bereit.\n\nDownload-Link:\n{downloadUrl}\n\nEnthaltene Nachrichten: {totalMessages, number}\nDateigröße: {fileSizeMB, number} MB\n\nDieser Link läuft am {expiresAt, date, full} um {expiresAt, time, short} ab.\n\nWenn du diesen Export nicht angefordert hast, ändere bitte sofort dein Passwort und kontaktiere unser Support-Team.\n\n– {product_name} Team"
	},
	"inactivity_warning": {
		"subject": "Dein {product_name}-Konto wird wegen Inaktivität gelöscht",
		"body": "Hallo {username},\n\nwir haben seit dem {lastActiveDate, date, full} keine Aktivität auf deinem {product_name}-Konto festgestellt.\n\nWenn du dich nicht bis zum {deletionDate, date, full} um {deletionDate, time, short} anmeldest, wird dein Konto aufgrund von Inaktivität dauerhaft gelöscht.\n\nMelde dich hier an:\n{loginUrl}\n\nWenn du {product_name} kürzlich genutzt hast, kontaktiere bitte sofort unser Support-Team.\n\n– {product_name} Team"
	},
	"ip_authorization": {
		"subject": "Anmeldung von einer neuen IP-Adresse bestätigen",
		"body": "Hallo {username},\n\nwir haben einen Anmeldeversuch für dein {product_name}-Konto von einer neuen IP-Adresse festgestellt:\n\nIP-Adresse: {ipAddress}\nStandort: {location}\n\nWenn du das warst, bestätige diese IP-Adresse bitte, indem du auf den untenstehenden Link klickst:\n\n{authUrl}\n\nWenn du dich nicht anmelden wolltest, ändere bitte sofort dein Passwort.\n\nDieser Link ist 30 Minuten gültig.\n\n– {product_name} Team"
	},
	"password_change_verification": {
		"subject": "Bestätige deine {product_name}-Passwortänderung",
		"body": "Hallo {username},\n\nwir haben eine Anfrage zur Änderung des Passworts deines {product_name}-Kontos erhalten.\n\nUm diese Änderung zu bestätigen, gib diesen Code in der App ein:\n\n{code}\n\nDieser Code läuft um {expiresAt} ab.\n\nWenn du dies nicht angefordert hast, könnte jemand Zugriff auf dein Konto haben. Ändere dein Passwort sofort und aktiviere die Zwei-Faktor-Authentifizierung.\n\n– {product_name} Team"
	},
	"password_reset": {
		"subject": "Setze dein {product_name}-Passwort zurück",
		"body": "Hallo {username},\n\ndu hast eine {product_name}-Passwortzurücksetzung angefordert. Verwende den untenstehenden Link, um ein neues Passwort festzulegen:\n\n{resetUrl}\n\nWenn du dies nicht angefordert hast, kannst du diese E-Mail ignorieren.\n\nDieser Link ist 1 Stunde gültig.\n\n– {product_name} Team"
	},
	"registration_approved": {
		"subject": "Deine {product_name}-Registrierung wurde genehmigt",
		"body": "Hallo {username},\n\ngute Nachrichten: Deine {product_name}-Registrierung wurde genehmigt.\n\nDu kannst dich jetzt hier in die {product_name}-App einloggen:\n{channelsUrl}\n\nWillkommen in der {product_name}-Community.\n\n– {product_name} Team"
	},
	"report_resolved": {
		"subject": "Dein {product_name}-Bericht wurde geprüft",
		"body": "Hallo {username},\n\ndein Bericht (ID: {reportId}) wurde von unserem Sicherheitsteam geprüft.{hasComment, select, yes {\n\nAntwort vom Sicherheitsteam:\n{publicComment}} other {}}\n\nDanke, dass du hilfst, {product_name} für alle sicher zu halten. Wir nehmen alle Berichte ernst und schätzen deinen Beitrag zur Community.\n\nWenn du Fragen oder Bedenken zu diesem Ergebnis hast, kontaktiere bitte {safety_email}.\n\n– {product_name}-Sicherheitsteam"
	},
	"scheduled_deletion_notification": {
		"subject": "Dein {product_name}-Konto wird dauerhaft gelöscht",
		"body": "Hallo {username},\n\ndein {product_name}-Konto wurde zur dauerhaften Löschung vorgemerkt.\n\nGeplante Löschung: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Grund: {reason}}\n}\n\nDies ist eine ernsthafte Maßnahme. Deine Kontodaten werden am geplanten Datum dauerhaft gelöscht.\n\nWenn du glaubst, dass diese Entscheidung falsch war, kannst du Einspruch einlegen. Sende eine E-Mail von dieser E-Mail-Adresse an {appeals_email}.\n\n– {product_name}-Sicherheitsteam"
	},
	"self_deletion_scheduled": {
		"subject": "Die Löschung deines {product_name}-Kontos ist geplant",
		"body": "Hallo {username},\n\ndu hast die Löschung deines {product_name}-Kontos beantragt. Dein Konto ist zur dauerhaften Löschung vorgesehen am:\n\n{deletionDate, date, full} um {deletionDate, time, short}\n\nWenn du dies nicht beantragt hast, melde dich in deinem Konto an, um die Löschung abzubrechen. Wir empfehlen außerdem, dein Passwort zu ändern, um dein Konto zu sichern.\n\n– {product_name} Team"
	},
	"unban_notification": {
		"subject": "Die Sperrung deines {product_name}-Kontos wurde aufgehoben",
		"body": "Hallo {username},\n\ngute Nachrichten: Die Sperrung deines {product_name}-Kontos wurde aufgehoben.\n\n{reason, select,\n  null {}\n  other {Grund: {reason}}\n}\n\nDu kannst dich jetzt wieder anmelden und {product_name} wie gewohnt nutzen.\n\n– {product_name}-Sicherheitsteam"
	}
});

export default EMAIL_I18N_DE_MESSAGES;
