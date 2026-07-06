// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_NL_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Je {product_name}-account is tijdelijk uitgeschakeld",
		"body": "Hallo {username},\n\nWe hebben je {product_name}-account tijdelijk uitgeschakeld omdat we verdachte activiteit hebben gedetecteerd.\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nOm weer toegang te krijgen tot je account, moet je je wachtwoord opnieuw instellen:\n\n{forgotUrl}\n\nNadat je je wachtwoord opnieuw hebt ingesteld, kun je weer inloggen.\n\nAls je denkt dat dit een fout is, neem dan contact op met ons support-team.\n\n– {product_name} Safety Team"
	},
	"account_scheduled_deletion": {
		"subject": "Je {product_name}-account wordt permanent verwijderd",
		"body": "Hallo {username},\n\nJe {product_name}-account is gepland voor permanente verwijdering vanwege schendingen van onze Servicevoorwaarden of Communityrichtlijnen.\n\nGeplande verwijdering: {deletionDate, date, full} om {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nDit is een serieuze handhavingsactie. Je accountgegevens worden permanent verwijderd op de geplande datum.\n\nBekijk ook:\n- Servicevoorwaarden: {termsUrl}\n- Communityrichtlijnen: {guidelinesUrl}\n\nBeroepsprocedure:\nAls je van mening bent dat deze handhavingsbeslissing onjuist of ongerechtvaardigd was, heb je 60 dagen de tijd om beroep aan te tekenen. Stuur een e-mail naar {appeals_email} vanaf dit e-mailadres.\n\nVermeld in je beroep:\n- Een duidelijke uitleg waarom je van mening bent dat de handhavingsbeslissing onjuist of ongerechtvaardigd was\n- Alle relevante bewijzen of context\n\nEen lid van het {product_name} Safety-Team zal je beroep beoordelen en kan de lopende verwijdering pauzeren totdat een definitieve beslissing is genomen.\n\n– {product_name} Safety Team"
	},
	"account_temp_banned": {
		"subject": "Je {product_name}-account is tijdelijk opgeschort",
		"body": "Hallo {username},\n\nJe {product_name}-account is tijdelijk opgeschort wegens schending van onze Servicevoorwaarden of Communityrichtlijnen.\n\nDuur: {durationHours, plural,\n  =1 {1 uur}\n  other {# uur}\n}\nOpgeschort tot: {bannedUntil, date, full} om {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nGedurende deze periode heb je geen toegang tot je account.\n\nBekijk ook:\n- Servicevoorwaarden: {termsUrl}\n- Communityrichtlijnen: {guidelinesUrl}\n\nAls je van mening bent dat deze handhavingsbeslissing onjuist of ongerechtvaardigd was, kun je beroep aantekenen. Stuur een e-mail naar {appeals_email} vanaf dit e-mailadres en leg duidelijk uit waarom je van mening bent dat de beslissing onjuist was. We zullen je beroep beoordelen en reageren met onze beslissing.\n\n– {product_name} Safety Team"
	},
	"donation_confirmation": {
		"subject": "Bedankt voor je {product_name}-donatie",
		"body": "Hallo,\n\nBedankt voor je donatie aan {product_name}! Je {interval, select,\n  month {terugkerende donatie}\n  year {terugkerende donatie}\n  other {eenmalige donatie}\n} is succesvol {interval, select,\n  month {ingesteld}\n  year {ingesteld}\n  other {verwerkt}\n}.\n\nDonatiegegevens:\nBedrag: {amount} {currency} {interval, select,\n  month {per maand}\n  year {per jaar}\n  other {}\n}\n\nStripe stuurt je binnenkort een aparte bon met je factuur-pdf. Deze bevat alle betalingsgegevens en kan worden gebruikt voor belastingdoeleinden.\n\nJe kunt je donatiegeschiedenis bekijken, facturen downloaden en {interval, select,\n  month {je abonnement beheren of opzeggen}\n  year {je abonnement beheren of opzeggen}\n  other {toekomstige donaties beheren}\n} via deze link:\n\n{manageUrl}\n\nJe steun helpt {product_name} draaiende te houden. Bedankt!\n\n– {product_name} Team"
	},
	"donation_magic_link": {
		"subject": "Beheer je {product_name}-donaties",
		"body": "Hallo,\n\nKlik op de onderstaande link om toegang te krijgen tot je donateursportaal:\n\n{manageUrl}\n\nIn het portaal kun je abonnementen beheren, facturen downloaden en je donatiegeschiedenis bekijken.\n\nDeze link verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je deze link niet hebt aangevraagd, kun je deze e-mail veilig negeren.\n\n– {product_name} Team"
	},
	"dsa_report_verification": {
		"subject": "Verifieer je e-mail voor een DSA-rapport",
		"body": "Hallo,\n\nGebruik de onderstaande verificatiecode om je Digital Services Act-rapport op {product_name} in te dienen:\n\n{code}\n\nDeze code verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je dit niet hebt aangevraagd, kun je deze e-mail negeren.\n\n– {product_name} Safety Team"
	},
	"email_change_new": {
		"subject": "Verifieer je nieuwe {product_name}-e-mail",
		"body": "Hallo {username},\n\nVoer deze code in de app in om je nieuwe {product_name}-e-mail te verifiëren:\n\n{code}\n\nDeze code verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je dit niet hebt aangevraagd, kun je deze e-mail negeren.\n\n– {product_name} Team"
	},
	"email_change_original": {
		"subject": "Bevestig je {product_name}-e-mailwijziging",
		"body": "Hallo {username},\n\nWe hebben een verzoek ontvangen om het e-mailadres van je {product_name}-account te wijzigen.\n\nOm deze wijziging te bevestigen, voer je deze code in de app in:\n\n{code}\n\nDeze code verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je dit niet hebt aangevraagd, beveilig je account dan onmiddellijk.\n\n– {product_name} Team"
	},
	"email_change_revert": {
		"subject": "Je {product_name}-e-mail is gewijzigd",
		"body": "Hallo {username},\n\nHet e-mailadres van je {product_name}-account is gewijzigd naar {newEmail}.\n\nAls je deze wijziging zelf hebt aangebracht, hoef je niets te doen. Als je dit niet hebt gedaan, kun je de wijziging ongedaan maken en je account beveiligen via deze link:\n\n{revertUrl}\n\nDit herstelt je vorige e-mail, meldt je overal af, verwijdert gekoppelde telefoonnummers, schakelt MFA uit en vereist dat je een nieuw wachtwoord instelt.\n\n– {product_name} Safety Team"
	},
	"email_verification": {
		"subject": "Verifieer je {product_name}-e-mailadres",
		"body": "Hallo {username},\n\nVerifieer het e-mailadres van je {product_name}-account door op de onderstaande link te klikken:\n\n{verifyUrl}\n\nAls je geen {product_name}-account hebt aangemaakt, kun je deze e-mail veilig negeren.\n\nDeze link is 24 uur geldig.\n\n– {product_name} Team"
	},
	"gift_chargeback_notification": {
		"subject": "Voordelen van je ingewisselde gift zijn verwijderd",
		"body": "Hallo {username},\n\nEen giftcode die je hebt ingewisseld, is oorspronkelijk door iemand anders betaald. Die betaling is sindsdien teruggedraaid (een chargeback).\n\nHierdoor hebben we de voordelen verwijderd die aan je account zijn toegevoegd toen je de gift inwisselde.\n\nAls je denkt dat dit een fout is, neem dan contact op met ons support-team en vermeld alle details die je hebt over de giftcode en wanneer je deze hebt ingewisseld.\n\n– {product_name} Team"
	},
	"harvest_completed": {
		"subject": "Je {product_name}-data-export is klaar om te downloaden",
		"body": "Hallo {username},\n\nJe data-export is klaar.\n\nDownloadlink:\n{downloadUrl}\n\nInbegrepen berichten: {totalMessages, number}\nBestandsgrootte: {fileSizeMB, number} MB\n\nDeze link verloopt op {expiresAt, date, full} om {expiresAt, time, short}.\n\nAls je deze export niet hebt aangevraagd, wijzig dan onmiddellijk je wachtwoord en neem contact op met ons support-team.\n\n– {product_name} Team"
	},
	"inactivity_warning": {
		"subject": "Je {product_name}-account wordt verwijderd wegens inactiviteit",
		"body": "Hallo {username},\n\nWe hebben geen activiteit op je {product_name}-account gezien sinds {lastActiveDate, date, full}.\n\nAls je niet inlogt vóór {deletionDate, date, full} om {deletionDate, time, short}, wordt je account permanent verwijderd wegens inactiviteit.\n\nLog hier in:\n{loginUrl}\n\nAls je {product_name} recentelijk hebt gebruikt, neem dan onmiddellijk contact op met ons support-team.\n\n– {product_name} Team"
	},
	"ip_authorization": {
		"subject": "Autoriseer inlog vanaf een nieuw IP-adres",
		"body": "Hallo {username},\n\nWe hebben een inlogpoging op je {product_name}-account gedetecteerd vanaf een nieuw IP-adres:\n\nIP-adres: {ipAddress}\nLocatie: {location}\n\nAls jij dit was, autoriseer dan dit IP-adres door op de onderstaande link te klikken:\n\n{authUrl}\n\nAls je niet hebt geprobeerd in te loggen, wijzig dan onmiddellijk je wachtwoord.\n\nDeze link is 30 minuten geldig.\n\n– {product_name} Team"
	},
	"password_change_verification": {
		"subject": "Bevestig je {product_name}-wachtwoordwijziging",
		"body": "Hallo {username},\n\nWe hebben een verzoek ontvangen om het wachtwoord van je {product_name}-account te wijzigen.\n\nOm deze wijziging te bevestigen, voer je deze code in de app in:\n\n{code}\n\nDeze code verloopt om {expiresAt}.\n\nAls je dit niet hebt aangevraagd, heeft iemand mogelijk toegang tot je account. Wijzig onmiddellijk je wachtwoord en schakel tweefactorauthenticatie in.\n\n– {product_name} Team"
	},
	"password_reset": {
		"subject": "Stel je {product_name}-wachtwoord opnieuw in",
		"body": "Hallo {username},\n\nJe hebt een {product_name}-wachtwoordreset aangevraagd. Gebruik de onderstaande link om een nieuw wachtwoord in te stellen:\n\n{resetUrl}\n\nAls je dit niet hebt aangevraagd, kun je deze e-mail veilig negeren.\n\nDeze link is 1 uur geldig.\n\n– {product_name} Team"
	},
	"registration_approved": {
		"subject": "Je {product_name}-registratie is goedgekeurd",
		"body": "Hallo {username},\n\nGoed nieuws: je {product_name}-registratie is goedgekeurd.\n\nJe kunt nu inloggen op de {product_name}-app via:\n{channelsUrl}\n\nWelkom bij de {product_name}-community.\n\n– {product_name} Team"
	},
	"report_resolved": {
		"subject": "Je {product_name}-rapport is bekeken",
		"body": "Hallo {username},\n\nJe rapport (ID: {reportId}) is bekeken door ons Safety-Team.{hasComment, select, yes {\n\nReactie van het Safety-Team:\n{publicComment}} other {}}\n\nBedankt dat je helpt {product_name} veilig te houden voor iedereen. We nemen alle meldingen serieus en waarderen je bijdrage aan de community.\n\nAls je vragen of opmerkingen hebt over deze uitkomst, neem dan contact op met {safety_email}.\n\n– {product_name} Safety Team"
	},
	"scheduled_deletion_notification": {
		"subject": "Je {product_name}-account wordt permanent verwijderd",
		"body": "Hallo {username},\n\nJe {product_name}-account is gepland voor permanente verwijdering.\n\nGeplande verwijdering: {deletionDate, date, full} om {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nDit is een serieuze handhavingsactie. Je accountgegevens worden permanent verwijderd op de geplande datum.\n\nAls je van mening bent dat deze handhavingsbeslissing onjuist was, kun je beroep aantekenen. Stuur een e-mail naar {appeals_email} vanaf dit e-mailadres.\n\n– {product_name} Safety-Team"
	},
	"self_deletion_scheduled": {
		"subject": "Verwijdering van je {product_name}-account is gepland",
		"body": "Hallo {username},\n\nJe hebt verzocht om je {product_name}-account te verwijderen. Je account is gepland voor permanente verwijdering op:\n\n{deletionDate, date, full} om {deletionDate, time, short}\n\nAls je dit niet hebt aangevraagd, log dan in op je account om de verwijdering te annuleren. We raden ook aan om je wachtwoord te wijzigen om je account te beveiligen.\n\n– {product_name} Team"
	},
	"unban_notification": {
		"subject": "De opschorting van je {product_name}-account is opgeheven",
		"body": "Hallo {username},\n\nGoed nieuws: de opschorting van je {product_name}-account is opgeheven.\n\n{reason, select,\n  null {}\n  other {Reden: {reason}}\n}\n\nJe kunt nu weer inloggen en {product_name} normaal blijven gebruiken.\n\n– {product_name} Safety-Team"
	}
});

export default EMAIL_I18N_NL_MESSAGES;
