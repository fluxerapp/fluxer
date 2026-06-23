// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_SV_SE_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Ditt {product_name}-konto har tillfälligt inaktiverats",
		"body": "Hej {username},\n\nVi har tillfälligt inaktiverat ditt {product_name}-konto eftersom vi upptäckte misstänkt aktivitet.\n\n{reason, select,\n  null {}\n  other {Anledning: {reason}}\n}\n\nFör att återfå åtkomst till ditt konto måste du återställa ditt lösenord:\n\n{forgotUrl}\n\nEfter att du har återställt ditt lösenord kan du logga in igen.\n\nOm du anser att detta har skett av misstag, kontakta vårt supportteam.\n\n– {product_name} Safety Team"
	},
	"account_scheduled_deletion": {
		"subject": "Ditt {product_name}-konto kommer att raderas permanent",
		"body": "Hej {username},\n\nDitt {product_name}-konto har schemalagts för permanent radering på grund av brott mot våra användarvillkor eller community-riktlinjer.\n\nSchemalagd radering: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Anledning: {reason}}\n}\n\nDetta är en allvarlig åtgärd. Dina kontouppgifter kommer att raderas permanent på det schemalagda datumet.\n\nGranska:\n- Användarvillkor: {termsUrl}\n- Community-riktlinjer: {guidelinesUrl}\n\nÖverklagandeprocess:\nOm du anser att detta beslut var felaktigt eller orättfärdigt har du 60 dagar på dig att överklaga. Mejla {appeals_email} från denna e-postadress.\n\nI din överklagan:\n- Förklara tydligt varför du anser att beslutet var felaktigt eller orättfärdigt\n- Bifoga relevanta bevis eller sammanhang\n\nEn medlem av {product_name} Safety Team kommer att granska din överklagan och kan pausa den pågående raderingen tills ett slutgiltigt beslut har fattats.\n\n– {product_name} Safety Team"
	},
	"account_temp_banned": {
		"subject": "Ditt {product_name}-konto har tillfälligt stängts av",
		"body": "Hej {username},\n\nDitt {product_name}-konto har tillfälligt stängts av på grund av brott mot våra användarvillkor eller community-riktlinjer.\n\nVaraktighet: {durationHours, plural,\n  =1 {1 timme}\n  other {# timmar}\n}\nAvstängd till: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Anledning: {reason}}\n}\n\nUnder denna tid kommer du inte att kunna komma åt ditt konto.\n\nGranska:\n- Användarvillkor: {termsUrl}\n- Community-riktlinjer: {guidelinesUrl}\n\nOm du anser att detta beslut var felaktigt eller orättfärdigt kan du överklaga. Kontakta vårt supportteam på {appeals_email} från denna e-postadress och förklara tydligt varför du anser att beslutet var felaktigt. Vi kommer att granska din överklagan och svara med vårt beslut.\n\n– {product_name} Safety Team"
	},
	"donation_confirmation": {
		"subject": "Tack för din donation till {product_name}",
		"body": "Hej,\n\nTack för din donation till {product_name}! Din {interval, select,\n  month {återkommande donation}\n  year {återkommande donation}\n  other {engångsdonation}\n} har {interval, select,\n  month {ställts in}\n  year {ställts in}\n  other {behandlats}\n} framgångsrikt.\n\nDonationsuppgifter:\nBelopp: {amount} {currency} {interval, select,\n  month {per månad}\n  year {per år}\n  other {}\n}\n\nStripe kommer att skicka ett separat kvitto med din faktura i PDF-format via e-post inom kort. Detta inkluderar alla betalningsuppgifter och kan användas för skatteändamål.\n\nDu kan se din donationshistorik, ladda ner fakturor, {interval, select,\n  month {och hantera eller avbryta ditt abonnemang}\n  year {och hantera eller avbryta ditt abonnemang}\n  other {och hantera framtida donationer}\n} när som helst via denna länk:\n\n{manageUrl}\n\nDitt stöd hjälper till att hålla {product_name} igång. Tack!\n\n– {product_name} Team"
	},
	"donation_magic_link": {
		"subject": "Hantera dina donationer till {product_name}",
		"body": "Hej,\n\nKlicka på länken nedan för att komma åt din donatorportal:\n\n{manageUrl}\n\nI portalen kan du hantera abonnemang, ladda ner fakturor och se din donationshistorik.\n\nDenna länk upphör att gälla {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nOm du inte begärde denna länk kan du tryggt ignorera detta mejl.\n\n– {product_name} Team"
	},
	"dsa_report_verification": {
		"subject": "Verifiera din e-postadress inför en DSA-rapport",
		"body": "Hej,\n\nAnvänd verifieringskoden nedan för att skicka in din DSA-rapport på {product_name}:\n\n{code}\n\nDenna kod upphör att gälla {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nOm du inte begärde detta kan du ignorera detta mejl.\n\n– {product_name} Safety Team"
	},
	"email_change_new": {
		"subject": "Verifiera din nya {product_name}-e-postadress",
		"body": "Hej {username},\n\nAnge denna kod i appen för att verifiera din nya {product_name}-e-postadress:\n\n{code}\n\nDenna kod upphör att gälla {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nOm du inte begärde detta kan du ignorera detta mejl.\n\n– {product_name} Team"
	},
	"email_change_original": {
		"subject": "Bekräfta din {product_name}-e-postadressändring",
		"body": "Hej {username},\n\nVi har mottagit en begäran om att ändra e-postadressen för ditt {product_name}-konto.\n\nFör att bekräfta denna ändring, ange denna kod i appen:\n\n{code}\n\nDenna kod upphör att gälla {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nOm du inte begärde detta, säkra ditt konto omedelbart.\n\n– {product_name} Team"
	},
	"email_change_revert": {
		"subject": "Din {product_name}-e-postadress ändrades",
		"body": "Hej {username},\n\nE-postadressen för ditt {product_name}-konto har ändrats till {newEmail}.\n\nOm du gjorde denna ändring behövs ingen åtgärd. Om du inte gjorde det kan du ångra ändringen och säkra ditt konto med denna länk:\n\n{revertUrl}\n\nDetta kommer att återställa din tidigare e-post, logga ut dig överallt, ta bort länkade telefonnummer, inaktivera MFA och kräva att du ställer in ett nytt lösenord.\n\n– {product_name} Safety Team"
	},
	"email_verification": {
		"subject": "Verifiera din {product_name}-e-postadress",
		"body": "Hej {username},\n\nVerifiera e-postadressen för ditt {product_name}-konto genom att klicka på länken nedan:\n\n{verifyUrl}\n\nOm du inte skapade ett {product_name}-konto kan du tryggt ignorera detta mejl.\n\nDenna länk är giltig i 24 timmar.\n\n– {product_name} Team"
	},
	"gift_chargeback_notification": {
		"subject": "Förmånerna för din inlösta gåva har tagits bort",
		"body": "Hej {username},\n\nEn presentkod du löste in betalades ursprungligen av någon annan. Den betalningen har sedan dess återkallats (en chargeback).\n\nPå grund av detta har vi tagit bort de förmåner som lades till ditt konto när du löste in presenten.\n\nOm du tror att detta är ett misstag, kontakta vår support och inkludera alla detaljer du har om presentkoden och när du löste in den.\n\n– {product_name} Team"
	},
	"harvest_completed": {
		"subject": "Din {product_name}-dataexport är redo att laddas ner",
		"body": "Hej {username},\n\nDin dataexport är redo.\n\nNedladdningslänk:\n{downloadUrl}\n\nMeddelanden inkluderade: {totalMessages, number}\nFilstorlek: {fileSizeMB, number} MB\n\nDenna länk upphör att gälla {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nOm du inte begärde denna export, ändra ditt lösenord omedelbart och kontakta vårt supportteam.\n\n– {product_name} Team"
	},
	"inactivity_warning": {
		"subject": "Ditt {product_name}-konto kommer att raderas på grund av inaktivitet",
		"body": "Hej {username},\n\nVi har inte sett någon aktivitet på ditt {product_name}-konto sedan {lastActiveDate, date, full}.\n\nOm du inte loggar in senast {deletionDate, date, full} kl. {deletionDate, time, short}, kommer ditt konto att raderas permanent på grund av inaktivitet.\n\nLogga in här:\n{loginUrl}\n\nOm du har använt {product_name} nyligen, kontakta vårt supportteam omedelbart.\n\n– {product_name} Team"
	},
	"ip_authorization": {
		"subject": "Auktorisera inloggning från en ny IP-adress",
		"body": "Hej {username},\n\nVi upptäckte ett inloggningsförsök till ditt {product_name}-konto från en ny IP-adress:\n\nIP-adress: {ipAddress}\nPlats: {location}\n\nOm detta var du, godkänn denna IP-adress genom att klicka på länken nedan:\n\n{authUrl}\n\nOm du inte försökte logga in, ändra ditt lösenord omedelbart.\n\nDenna länk är giltig i 30 minuter.\n\n– {product_name} Team"
	},
	"password_change_verification": {
		"subject": "Bekräfta din {product_name}-lösenordsändring",
		"body": "Hej {username},\n\nVi har mottagit en begäran om att ändra lösenordet för ditt {product_name}-konto.\n\nFör att bekräfta denna ändring, ange denna kod i appen:\n\n{code}\n\nDenna kod upphör att gälla {expiresAt}.\n\nOm du inte begärde detta kan någon ha åtkomst till ditt konto. Ändra ditt lösenord omedelbart och aktivera tvåfaktorsautentisering.\n\n– {product_name} Team"
	},
	"password_reset": {
		"subject": "Återställ ditt {product_name}-lösenord",
		"body": "Hej {username},\n\nDu begärde en återställning av ditt {product_name}-lösenord. Använd länken nedan för att ställa in ett nytt lösenord:\n\n{resetUrl}\n\nOm du inte begärde detta kan du tryggt ignorera detta mejl.\n\nDenna länk är giltig i 1 timme.\n\n– {product_name} Team"
	},
	"registration_approved": {
		"subject": "Din {product_name}-registrering har godkänts",
		"body": "Hej {username},\n\nGoda nyheter: din {product_name}-registrering har godkänts.\n\nDu kan nu logga in på {product_name}-appen här:\n{channelsUrl}\n\nVälkommen till {product_name}-communityn.\n\n– {product_name} Team"
	},
	"report_resolved": {
		"subject": "Din {product_name}-rapport har granskats",
		"body": "Hej {username},\n\nDin rapport (ID: {reportId}) har granskats av vårt säkerhetsteam.{hasComment, select, yes {\n\nSvar från säkerhetsteamet:\n{publicComment}} other {}}\n\nTack för att du hjälper till att hålla {product_name} säkert för alla. Vi tar alla rapporter på allvar och uppskattar ditt bidrag till communityn.\n\nOm du har några frågor eller funderingar kring detta resultat, kontakta oss på {safety_email}.\n\n– {product_name} Safety Team"
	},
	"scheduled_deletion_notification": {
		"subject": "Ditt {product_name}-konto kommer att raderas permanent",
		"body": "Hej {username},\n\nDitt {product_name}-konto har schemalagts för permanent radering.\n\nSchemalagd radering: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Anledning: {reason}}\n}\n\nDetta är en allvarlig åtgärd. Dina kontouppgifter kommer att raderas permanent på det schemalagda datumet.\n\nOm du anser att detta beslut var felaktigt kan du överklaga. Mejla {appeals_email} från denna e-postadress.\n\n– {product_name} Safety Team"
	},
	"self_deletion_scheduled": {
		"subject": "Raderingen av ditt {product_name}-konto är schemalagd",
		"body": "Hej {username},\n\nDu begärde att radera ditt {product_name}-konto. Ditt konto är schemalagt för permanent radering den:\n\n{deletionDate, date, full} kl. {deletionDate, time, short}\n\nOm du inte begärde detta, logga in på ditt konto för att avbryta raderingen. Vi rekommenderar också att du ändrar ditt lösenord för att säkra ditt konto.\n\n– {product_name} Team"
	},
	"unban_notification": {
		"subject": "Avstängningen av ditt {product_name}-konto har hävts",
		"body": "Hej {username},\n\nGoda nyheter: avstängningen av ditt {product_name}-konto har hävts.\n\n{reason, select,\n  null {}\n  other {Anledning: {reason}}\n}\n\nDu kan nu logga in igen och fortsätta använda {product_name} som vanligt.\n\n– {product_name} Safety Team"
	}
});

export default EMAIL_I18N_SV_SE_MESSAGES;
