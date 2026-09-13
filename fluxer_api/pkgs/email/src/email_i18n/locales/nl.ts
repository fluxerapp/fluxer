// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '@pkgs/email/src/email_i18n/EmailI18nMessages';

const EMAIL_I18N_NL_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Je {product_name}-account is tijdelijk uitgeschakeld",
		"body": "Hallo {username},\n\nWe hebben je {product_name}-account tijdelijk uitgeschakeld omdat we verdachte activiteit hebben gedetecteerd.\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nOm weer toegang te krijgen tot je account, moet je je wachtwoord opnieuw instellen:\n\n{forgotUrl}\n\nNadat je je wachtwoord opnieuw hebt ingesteld, kun je weer inloggen.\n\nAls je denkt dat dit een fout is, neem dan contact op met ons supportteam.\n\n– Het veiligheidsteam van {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Je {product_name}-account wordt permanent verwijderd",
		"body": "Hallo {username},\n\nJe {product_name}-account staat gepland voor permanente verwijdering vanwege schendingen van onze Gebruiksvoorwaarden of Communityrichtlijnen.\n\nGeplande verwijdering: {deletionDate, date, full} om {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nDit is een ingrijpende handhavingsmaatregel. Je accountgegevens worden permanent verwijderd op de geplande datum.\n\nBekijk ook:\n- Gebruiksvoorwaarden: {termsUrl}\n- Communityrichtlijnen: {guidelinesUrl}\n\nBezwaarprocedure:\nAls je van mening bent dat deze handhavingsbeslissing onjuist of ongerechtvaardigd was, heb je 60 dagen de tijd om bezwaar te maken. Stuur een e-mail naar {appeals_email} vanaf dit e-mailadres.\n\nVermeld in je bezwaar:\n- Een duidelijke uitleg waarom je van mening bent dat de handhavingsbeslissing onjuist of ongerechtvaardigd was\n- Alle relevante bewijzen of context\n\nEen medewerker van het veiligheidsteam van {product_name} zal je bezwaar beoordelen en kan de geplande verwijdering uitstellen totdat een definitieve beslissing is genomen.\n\n– Het veiligheidsteam van {product_name}"
	},
	"account_temp_banned": {
		"subject": "Je {product_name}-account is tijdelijk geschorst",
		"body": "Hallo {username},\n\nJe {product_name}-account is tijdelijk geschorst wegens schending van onze Gebruiksvoorwaarden of Communityrichtlijnen.\n\nDuur: {durationHours, plural,\n  =1 {1 uur}\n  other {# uur}\n}\nGeschorst tot: {bannedUntil, date, full} om {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nGedurende deze periode heb je geen toegang tot je account.\n\nBekijk ook:\n- Gebruiksvoorwaarden: {termsUrl}\n- Communityrichtlijnen: {guidelinesUrl}\n\nAls je van mening bent dat deze handhavingsbeslissing onjuist of ongerechtvaardigd was, kun je bezwaar maken. Stuur een e-mail naar {appeals_email} vanaf dit e-mailadres en leg duidelijk uit waarom je van mening bent dat de beslissing onjuist was. We zullen je bezwaar beoordelen en reageren met onze beslissing.\n\n– Het veiligheidsteam van {product_name}"
	},
	"donation_confirmation": {
		"subject": "Bedankt voor je {product_name}-donatie",
		"body": "Hallo,\n\nBedankt voor je donatie aan {product_name}! Je {interval, select,\n  month {terugkerende donatie}\n  year {terugkerende donatie}\n  other {eenmalige donatie}\n} is succesvol {interval, select,\n  month {ingesteld}\n  year {ingesteld}\n  other {verwerkt}\n}.\n\nDonatiegegevens:\nBedrag: {amount} {currency} {interval, select,\n  month {per maand}\n  year {per jaar}\n  other {}\n}\n\nStripe stuurt je binnenkort per e-mail een apart betalingsbewijs met je factuur als pdf. Dit bevat alle betalingsgegevens en kan worden gebruikt voor belastingdoeleinden.\n\nJe kunt op elk moment je donatiegeschiedenis bekijken, facturen downloaden en {interval, select,\n  month {je abonnement beheren of opzeggen}\n  year {je abonnement beheren of opzeggen}\n  other {toekomstige donaties beheren}\n} via deze link:\n\n{manageUrl}\n\nJe steun helpt {product_name} draaiende te houden. Bedankt!\n\n– Het team van {product_name}"
	},
	"donation_magic_link": {
		"subject": "Beheer je {product_name}-donaties",
		"body": "Hallo,\n\nKlik op de onderstaande link om toegang te krijgen tot je donateursportaal:\n\n{manageUrl}\n\nIn het portaal kun je abonnementen beheren, facturen downloaden en je donatiegeschiedenis bekijken.\n\nDeze link verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je deze link niet hebt aangevraagd, kun je deze e-mail veilig negeren.\n\n– Het team van {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Verifieer je e-mailadres voor een DSA-melding",
		"body": "Hallo,\n\nGebruik de onderstaande verificatiecode om je melding op grond van de Digital Services Act op {product_name} in te dienen:\n\n{code}\n\nDeze code verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je dit niet hebt aangevraagd, kun je deze e-mail negeren.\n\n– Het veiligheidsteam van {product_name}"
	},
	"email_change_new": {
		"subject": "Verifieer je nieuwe e-mailadres op {product_name}",
		"body": "Hallo {username},\n\nVoer deze code in de app in om je nieuwe e-mailadres op {product_name} te verifiëren:\n\n{code}\n\nDeze code verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je dit niet hebt aangevraagd, kun je deze e-mail negeren.\n\n– Het team van {product_name}"
	},
	"email_change_original": {
		"subject": "Bevestig de wijziging van je e-mailadres op {product_name}",
		"body": "Hallo {username},\n\nWe hebben een verzoek ontvangen om het e-mailadres van je {product_name}-account te wijzigen.\n\nOm deze wijziging te bevestigen, voer je deze code in de app in:\n\n{code}\n\nDeze code verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je dit niet hebt aangevraagd, beveilig je account dan onmiddellijk.\n\n– Het team van {product_name}"
	},
	"email_change_revert": {
		"subject": "Je e-mailadres op {product_name} is gewijzigd",
		"body": "Hallo {username},\n\nHet e-mailadres van je {product_name}-account is gewijzigd naar {newEmail}.\n\nAls je deze wijziging zelf hebt aangebracht, hoef je niets te doen. Als je dit niet hebt gedaan, kun je de wijziging ongedaan maken en je account beveiligen via deze link:\n\n{revertUrl}\n\nDit herstelt je vorige e-mailadres, meldt je overal af, verwijdert gekoppelde telefoonnummers, schakelt MFA uit en vereist dat je een nieuw wachtwoord instelt.\n\n– Het veiligheidsteam van {product_name}"
	},
	"email_verification": {
		"subject": "Verifieer je e-mailadres op {product_name}",
		"body": "Hallo {username},\n\nVerifieer het e-mailadres van je {product_name}-account door op de onderstaande link te klikken:\n\n{verifyUrl}\n\nAls je geen {product_name}-account hebt aangemaakt, kun je deze e-mail veilig negeren.\n\nDeze link is 24 uur geldig.\n\n– Het team van {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "De voordelen van je ingewisselde cadeau zijn verwijderd",
		"body": "Hallo {username},\n\nEen cadeaucode die je hebt ingewisseld, is oorspronkelijk door iemand anders betaald. Die betaling is sindsdien teruggedraaid (een terugboeking).\n\nHierdoor hebben we de voordelen verwijderd die aan je account zijn toegevoegd toen je het cadeau inwisselde.\n\nAls je denkt dat dit een fout is, neem dan contact op met ons supportteam en vermeld alle details die je hebt over de cadeaucode en wanneer je deze hebt ingewisseld.\n\n– Het team van {product_name}"
	},
	"harvest_completed": {
		"subject": "Je {product_name}-data-export is klaar om te downloaden",
		"body": "Hallo {username},\n\nJe data-export is klaar.\n\nDownloadlink:\n{downloadUrl}\n\nOpgenomen berichten: {totalMessages, number}\nBestandsgrootte: {fileSizeMB, number} MB\n\nDeze link verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je deze export niet hebt aangevraagd, wijzig dan onmiddellijk je wachtwoord en neem contact op met ons supportteam.\n\n– Het team van {product_name}"
	},
	"inactivity_warning": {
		"subject": "Je {product_name}-account wordt verwijderd wegens inactiviteit",
		"body": "Hallo {username},\n\nWe hebben geen activiteit op je {product_name}-account gezien sinds {lastActiveDate, date, full}.\n\nAls je niet inlogt vóór {deletionDate, date, full} om {deletionDate, time, short}, wordt je account permanent verwijderd wegens inactiviteit.\n\nLog hier in:\n{loginUrl}\n\nAls je {product_name} recentelijk hebt gebruikt, neem dan onmiddellijk contact op met ons supportteam.\n\n– Het team van {product_name}"
	},
	"ip_authorization": {
		"subject": "Sta inloggen vanaf een nieuw IP-adres toe",
		"body": "Hallo {username},\n\nWe hebben een inlogpoging op je {product_name}-account gedetecteerd vanaf een nieuw IP-adres:\n\nIP-adres: {ipAddress}\nLocatie: {location}\n\nAls jij dit was, sta dan inloggen vanaf dit IP-adres toe door op de onderstaande link te klikken:\n\n{authUrl}\n\nAls je niet hebt geprobeerd in te loggen, wijzig dan onmiddellijk je wachtwoord.\n\nDeze link is 30 minuten geldig.\n\n– Het team van {product_name}"
	},
	"mfa_backup_codes_view": {
		"subject": "Bevestig toegang tot je {product_name}-back-upcodes",
		"body": "Hallo {username},\n\nWe hebben een verzoek ontvangen om de back-upcodes van je {product_name}-account te bekijken.\n\nOm dit verzoek te bevestigen, voer je deze code in de app in:\n\n{code}\n\nDeze code verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je dit niet hebt aangevraagd, heeft iemand mogelijk toegang tot je account. Wijzig onmiddellijk je wachtwoord.\n\n– Het team van {product_name}"
	},
	"password_change_verification": {
		"subject": "Bevestig de wijziging van je wachtwoord op {product_name}",
		"body": "Hallo {username},\n\nWe hebben een verzoek ontvangen om het wachtwoord van je {product_name}-account te wijzigen.\n\nOm deze wijziging te bevestigen, voer je deze code in de app in:\n\n{code}\n\nDeze code verloopt om {expiresAt}.\n\nAls je dit niet hebt aangevraagd, heeft iemand mogelijk toegang tot je account. Wijzig onmiddellijk je wachtwoord en schakel tweefactorauthenticatie in.\n\n– Het team van {product_name}"
	},
	"password_reset": {
		"subject": "Stel je {product_name}-wachtwoord opnieuw in",
		"body": "Hallo {username},\n\nJe hebt een {product_name}-wachtwoordreset aangevraagd. Gebruik de onderstaande link om een nieuw wachtwoord in te stellen:\n\n{resetUrl}\n\nAls je dit niet hebt aangevraagd, kun je deze e-mail veilig negeren.\n\nDeze link is 1 uur geldig.\n\n– Het team van {product_name}"
	},
	"registration_approved": {
		"subject": "Je {product_name}-registratie is goedgekeurd",
		"body": "Hallo {username},\n\nGoed nieuws: je {product_name}-registratie is goedgekeurd.\n\nJe kunt nu inloggen op de {product_name}-app via:\n{channelsUrl}\n\nWelkom bij de {product_name}-community.\n\n– Het team van {product_name}"
	},
	"report_resolved": {
		"subject": "Je melding op {product_name} is beoordeeld",
		"body": "Hallo {username},\n\nJe melding (ID: {reportId}) is beoordeeld door ons veiligheidsteam.{hasComment, select, yes {\n\nReactie van het veiligheidsteam:\n{publicComment}} other {}}\n\nBedankt dat je helpt {product_name} veilig te houden voor iedereen. We nemen alle meldingen serieus en waarderen je bijdrage aan de community.\n\nAls je vragen of opmerkingen hebt over deze uitkomst, neem dan contact op met {safety_email}.\n\n– Het veiligheidsteam van {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Je {product_name}-account wordt permanent verwijderd",
		"body": "Hallo {username},\n\nJe {product_name}-account staat gepland voor permanente verwijdering.\n\nGeplande verwijdering: {deletionDate, date, full} om {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nDit is een ingrijpende handhavingsmaatregel. Je accountgegevens worden permanent verwijderd op de geplande datum.\n\nAls je van mening bent dat deze handhavingsbeslissing onjuist was, kun je bezwaar maken. Stuur een e-mail naar {appeals_email} vanaf dit e-mailadres.\n\n– Het veiligheidsteam van {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "De verwijdering van je {product_name}-account staat gepland",
		"body": "Hallo {username},\n\nJe hebt verzocht om je {product_name}-account te verwijderen. Je account staat gepland voor permanente verwijdering op:\n\n{deletionDate, date, full} om {deletionDate, time, short}\n\nAls je dit niet hebt aangevraagd, log dan in op je account om de verwijdering te annuleren. We raden ook aan om je wachtwoord te wijzigen om je account te beveiligen.\n\n– Het team van {product_name}"
	},
	"unban_notification": {
		"subject": "De schorsing van je {product_name}-account is opgeheven",
		"body": "Hallo {username},\n\nGoed nieuws: de schorsing van je {product_name}-account is opgeheven.\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nJe kunt nu weer inloggen en {product_name} normaal blijven gebruiken.\n\n– Het veiligheidsteam van {product_name}"
	}
});

export default EMAIL_I18N_NL_MESSAGES;
