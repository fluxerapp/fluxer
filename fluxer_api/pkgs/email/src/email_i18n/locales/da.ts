// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_DA_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Din {product_name}-konto er midlertidigt deaktiveret",
		"body": "Hej {username},\n\nVi har midlertidigt deaktiveret din {product_name}-konto, fordi vi har registreret mistænkelig aktivitet.\n\n{reason, select,\n  null {}\n  other {Årsag: {reason}}\n}\n\nFor at få adgang til din konto igen skal du nulstille din adgangskode:\n\n{forgotUrl}\n\nNår du har nulstillet din adgangskode, kan du logge ind igen.\n\nHvis du mener, at dette er sket ved en fejl, kan du kontakte vores supportteam.\n\n– {product_name} Safety Team"
	},
	"account_scheduled_deletion": {
		"subject": "Din {product_name}-konto slettes permanent",
		"body": "Hej {username},\n\nDin {product_name}-konto er planlagt til permanent sletning på grund af overtrædelser af vores servicevilkår eller retningslinjer for community'et.\n\nPlanlagt sletning: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Årsag: {reason}}\n}\n\nDette er en alvorlig håndhævelseshandling. Dine kontodata bliver permanent slettet på den planlagte dato.\n\nGennemgå:\n- Servicevilkår: {termsUrl}\n- Retningslinjer for community'et: {guidelinesUrl}\n\nAnkeproces:\nHvis du mener, at denne håndhævelsesbeslutning var forkert eller uberettiget, har du 60 dage til at indsende en anke. Send en e-mail til {appeals_email} fra denne e-mailadresse.\n\nI din anke:\n- Forklar tydeligt, hvorfor du mener, at håndhævelsesbeslutningen var forkert eller uberettiget\n- Fremlæg relevant bevis eller kontekst\n\nEt medlem af {product_name} Safety Team vil gennemgå din anke og kan sætte den afventende sletning på pause, indtil en endelig beslutning er truffet.\n\n– {product_name} Safety Team"
	},
	"account_temp_banned": {
		"subject": "Din {product_name}-konto er midlertidigt suspenderet",
		"body": "Hej {username},\n\nDin {product_name}-konto er midlertidigt suspenderet for at overtræde vores servicevilkår eller retningslinjer for community'et.\n\nVarighed: {durationHours, plural,\n  =1 {1 time}\n  other {# timer}\n}\nSuspenderet indtil: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Årsag: {reason}}\n}\n\nI denne periode vil du ikke kunne få adgang til din konto.\n\nGennemgå:\n- Servicevilkår: {termsUrl}\n- Retningslinjer for community'et: {guidelinesUrl}\n\nHvis du mener, at denne håndhævelsesbeslutning var forkert eller uberettiget, kan du indsende en anke. Send en e-mail til {appeals_email} fra denne e-mailadresse og forklar tydeligt, hvorfor du mener, at beslutningen var forkert. Vi vil gennemgå din anke og svare med vores beslutning.\n\n– {product_name} Safety Team"
	},
	"donation_confirmation": {
		"subject": "Tak for din {product_name}-donation",
		"body": "Hej,\n\nTak for din donation til {product_name}! Din {interval, select,\n  month {tilbagevendende donation}\n  year {tilbagevendende donation}\n  other {engangsdonation}\n} er blevet {interval, select,\n  month {opsat}\n  year {opsat}\n  other {behandlet}\n} med succes.\n\nDonationsdetaljer:\nBeløb: {amount} {currency} {interval, select,\n  month {pr. måned}\n  year {pr. år}\n  other {}\n}\n\nStripe sender dig en separat kvittering med din faktura-PDF inden længe. Denne inkluderer alle betalingsdetaljer og kan bruges til skatteformål.\n\nDu kan til enhver tid se din donationshistorik, downloade fakturaer, {interval, select,\n  month {og administrere eller annullere dit abonnement}\n  year {og administrere eller annullere dit abonnement}\n  other {og administrere fremtidige donationer}\n} ved at bruge dette link:\n\n{manageUrl}\n\nDin støtte hjælper med at holde {product_name} kørende. Tak!\n\n– {product_name} Team"
	},
	"donation_magic_link": {
		"subject": "Administrer dine {product_name}-donationer",
		"body": "Hej,\n\nKlik på linket nedenfor for at få adgang til din donorportal:\n\n{manageUrl}\n\nI portalen kan du administrere abonnementer, downloade fakturaer og se din donationshistorik.\n\nDette link udløber den {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke har anmodet om dette link, kan du trygt ignorere denne e-mail.\n\n– {product_name} Team"
	},
	"dsa_report_verification": {
		"subject": "Bekræft din e-mail til en DSA-rapport",
		"body": "Hej,\n\nBrug bekræftelseskoden nedenfor til at indsende din Digital Services Act-rapport for {product_name}:\n\n{code}\n\nDenne kode udløber den {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke har anmodet om dette, kan du ignorere denne e-mail.\n\n– {product_name} Safety Team"
	},
	"email_change_new": {
		"subject": "Bekræft din nye {product_name}-e-mail",
		"body": "Hej {username},\n\nIndtast denne kode i appen for at bekræfte din nye {product_name}-e-mail:\n\n{code}\n\nDenne kode udløber den {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke har anmodet om dette, kan du ignorere denne e-mail.\n\n– {product_name} Team"
	},
	"email_change_original": {
		"subject": "Bekræft din {product_name}-e-mail-ændring",
		"body": "Hej {username},\n\nVi har modtaget en anmodning om at ændre e-mailadressen på din {product_name}-konto.\n\nFor at bekræfte denne ændring skal du indtaste denne kode i appen:\n\n{code}\n\nDenne kode udløber den {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke har anmodet om dette, skal du sikre din konto med det samme.\n\n– {product_name} Team"
	},
	"email_change_revert": {
		"subject": "Din {product_name}-e-mail blev ændret",
		"body": "Hej {username},\n\nE-mailadressen på din {product_name}-konto blev ændret til {newEmail}.\n\nHvis du har foretaget denne ændring, er der ingen handling nødvendig. Hvis du ikke har, kan du fortryde ændringen og sikre din konto ved at bruge dette link:\n\n{revertUrl}\n\nDette vil gendanne din tidligere e-mail, logge dig ud overalt, fjerne tilknyttede telefonnumre, deaktivere MFA og kræve, at du indstiller en ny adgangskode.\n\n– {product_name} Safety Team"
	},
	"email_verification": {
		"subject": "Bekræft din {product_name}-e-mailadresse",
		"body": "Hej {username},\n\nBekræft e-mailadressen for din {product_name}-konto ved at klikke på linket nedenfor:\n\n{verifyUrl}\n\nHvis du ikke har oprettet en {product_name}-konto, kan du trygt ignorere denne e-mail.\n\nDette link er gyldigt i 24 timer.\n\n– {product_name} Team"
	},
	"gift_chargeback_notification": {
		"subject": "Fordele fra din indløste gave er fjernet",
		"body": "Hej {username},\n\nEn gavekode, du indløste, blev oprindeligt betalt af en anden. Den betaling er siden annulleret (en tilbageførsel).\n\nDerfor har vi fjernet de fordele, der blev tilføjet din konto, da du indløste gaven.\n\nHvis du mener, at dette er en fejl, kan du kontakte vores supportteam og oplyse alle detaljer, du har om gavekoden, og hvornår du indløste den.\n\n– {product_name} Team"
	},
	"harvest_completed": {
		"subject": "Din {product_name}-dataeksport er klar til at downloade",
		"body": "Hej {username},\n\nDin dataeksport er klar.\n\nDownload-link:\n{downloadUrl}\n\nAntal beskeder: {totalMessages, number}\nFilstørrelse: {fileSizeMB, number} MB\n\nDette link udløber den {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke har anmodet om denne eksport, skal du straks ændre din adgangskode og kontakte vores supportteam.\n\n– {product_name} Team"
	},
	"inactivity_warning": {
		"subject": "Din {product_name}-konto slettes på grund af inaktivitet",
		"body": "Hej {username},\n\nVi har ikke set nogen aktivitet på din {product_name}-konto siden {lastActiveDate, date, full}.\n\nHvis du ikke logger ind inden {deletionDate, date, full} kl. {deletionDate, time, short}, slettes din konto permanent på grund af inaktivitet.\n\nLog ind her:\n{loginUrl}\n\nHvis du har brugt {product_name} for nylig, skal du kontakte vores supportteam med det samme.\n\n– {product_name} Team"
	},
	"ip_authorization": {
		"subject": "Godkend login fra en ny IP-adresse",
		"body": "Hej {username},\n\nVi har registreret et login-forsøg på din {product_name}-konto fra en ny IP-adresse:\n\nIP-adresse: {ipAddress}\nSted: {location}\n\nHvis dette var dig, skal du godkende denne IP-adresse ved at klikke på linket nedenfor:\n\n{authUrl}\n\nHvis du ikke har forsøgt at logge ind, skal du straks ændre din adgangskode.\n\nDette link er gyldigt i 30 minutter.\n\n– {product_name} Team"
	},
	"password_change_verification": {
		"subject": "Bekræft din {product_name}-adgangskodeændring",
		"body": "Hej {username},\n\nVi har modtaget en anmodning om at ændre adgangskoden på din {product_name}-konto.\n\nFor at bekræfte denne ændring skal du indtaste denne kode i appen:\n\n{code}\n\nDenne kode udløber kl. {expiresAt}.\n\nHvis du ikke har anmodet om dette, har nogen muligvis adgang til din konto. Skift din adgangskode med det samme, og aktiver totrinsgodkendelse.\n\n– {product_name} Team"
	},
	"password_reset": {
		"subject": "Nulstil din {product_name}-adgangskode",
		"body": "Hej {username},\n\nDu har anmodet om at nulstille din {product_name}-adgangskode. Brug linket nedenfor til at indstille en ny adgangskode:\n\n{resetUrl}\n\nHvis du ikke har anmodet om dette, kan du trygt ignorere denne e-mail.\n\nDette link er gyldigt i 1 time.\n\n– {product_name} Team"
	},
	"registration_approved": {
		"subject": "Din {product_name}-registrering er godkendt",
		"body": "Hej {username},\n\nGode nyheder: Din {product_name}-registrering er godkendt.\n\nDu kan nu logge ind på {product_name}-appen her:\n{channelsUrl}\n\nVelkommen til {product_name}-community'et.\n\n– {product_name} Team"
	},
	"report_resolved": {
		"subject": "Din {product_name}-rapport er gennemgået",
		"body": "Hej {username},\n\nDin rapport (ID: {reportId}) er gennemgået af vores Safety Team.{hasComment, select, yes {\n\nSvar fra Safety Team:\n{publicComment}} other {}}\n\nTak for din hjælp til at holde {product_name} sikkert for alle. Vi tager alle rapporter alvorligt og værdsætter dit bidrag til community'et.\n\nHvis du har spørgsmål eller bekymringer vedrørende dette resultat, kan du kontakte {safety_email}.\n\n– {product_name} Safety Team"
	},
	"scheduled_deletion_notification": {
		"subject": "Din {product_name}-konto slettes permanent",
		"body": "Hej {username},\n\nDin {product_name}-konto er planlagt til permanent sletning.\n\nPlanlagt sletning: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Årsag: {reason}}\n}\n\nDette er en alvorlig håndhævelseshandling. Dine kontodata bliver permanent slettet på den planlagte dato.\n\nHvis du mener, at denne håndhævelsesbeslutning var forkert, kan du indsende en anke. Send en e-mail til {appeals_email} fra denne e-mailadresse.\n\n– {product_name} Safety Team"
	},
	"self_deletion_scheduled": {
		"subject": "Din {product_name}-kontosletning er planlagt",
		"body": "Hej {username},\n\nDu har anmodet om at slette din {product_name}-konto. Din konto slettes permanent den:\n\n{deletionDate, date, full} kl. {deletionDate, time, short}\n\nHvis du ikke har anmodet om dette, skal du logge ind på din konto for at annullere sletningen. Vi anbefaler også, at du ændrer din adgangskode for at sikre din konto.\n\n– {product_name} Team"
	},
	"unban_notification": {
		"subject": "Din {product_name}-kontosuspension er ophævet",
		"body": "Hej {username},\n\nGode nyheder: Din {product_name}-kontosuspension er ophævet.\n\n{reason, select,\n  null {}\n  other {Årsag: {reason}}\n}\n\nDu kan nu logge ind igen og fortsætte med at bruge {product_name} som normalt.\n\n– {product_name} Safety Team"
	}
});

export default EMAIL_I18N_DA_MESSAGES;
