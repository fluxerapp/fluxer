// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_NO_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "{product_name}-kontoen din er midlertidig deaktivert",
		"body": "Hei {username},\n\nVi deaktiverte {product_name}-kontoen din midlertidig fordi vi oppdaget mistenkelig aktivitet.\n\n{reason, select,\n  null {}\n  other {Årsak: {reason}}\n}\n\nFor å få tilgang til kontoen din igjen, må du tilbakestille passordet ditt:\n\n{forgotUrl}\n\nEtter at du har tilbakestilt passordet ditt, kan du logge inn igjen.\n\nHvis du mener dette er en feil, kontakt kundestøtte.\n\n– {product_name} sikkerhetsteam"
	},
	"account_scheduled_deletion": {
		"subject": "{product_name}-kontoen din blir permanent slettet",
		"body": "Hei {username},\n\n{product_name}-kontoen din er planlagt for permanent sletting på grunn av brudd på våre tjenestevilkår eller community-retningslinjer.\n\nPlanlagt sletting: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Årsak: {reason}}\n}\n\nDette er et alvorlig tiltak. Kontodataene dine blir permanent slettet på den planlagte datoen.\n\nSe over:\n- Tjenestevilkår: {termsUrl}\n- Community-retningslinjer: {guidelinesUrl}\n\nAnkeprosess:\nHvis du mener denne håndhevelsesbeslutningen var feil eller uberettiget, har du 60 dager på deg til å sende inn en anke. E-post {appeals_email} fra denne e-postadressen.\n\nI anken din:\n- Forklar tydelig hvorfor du mener håndhevelsesbeslutningen var feil eller uberettiget\n- Gi relevant bevis eller kontekst\n\nEt medlem av {product_name} sikkerhetsteam vil gjennomgå anken din og kan sette den ventende slettingen på pause til en endelig beslutning er tatt.\n\n– {product_name} sikkerhetsteam"
	},
	"account_temp_banned": {
		"subject": "{product_name}-kontoen din er midlertidig suspendert",
		"body": "Hei {username},\n\n{product_name}-kontoen din er midlertidig suspendert for brudd på våre tjenestevilkår eller community-retningslinjer.\n\nVarighet: {durationHours, plural,\n  =1 {1 time}\n  other {# timer}\n}\nSuspendert til: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Årsak: {reason}}\n}\n\nI løpet av denne tiden vil du ikke kunne få tilgang til kontoen din.\n\nSe over:\n- Tjenestevilkår: {termsUrl}\n- Community-retningslinjer: {guidelinesUrl}\n\nHvis du mener denne håndhevelsesbeslutningen var feil eller uberettiget, kan du sende inn en anke. E-post {appeals_email} fra denne e-postadressen og forklar tydelig hvorfor du mener beslutningen var feil. Vi vil gjennomgå anken din og svare med vår beslutning.\n\n– {product_name} sikkerhetsteam"
	},
	"donation_confirmation": {
		"subject": "Takk for ditt {product_name}-bidrag",
		"body": "Hei,\n\nTakk for ditt bidrag til {product_name}! Ditt {interval, select,\n  month {gjentakende bidrag}\n  year {gjentakende bidrag}\n  other {engangsbidrag}\n} er {interval, select,\n  month {satt opp}\n  year {satt opp}\n  other {behandlet}\n} vellykket.\n\nBidragsdetaljer:\nBeløp: {amount} {currency} {interval, select,\n  month {per måned}\n  year {per år}\n  other {}\n}\n\nStripe sender deg en separat kvittering med din faktura-PDF om kort tid. Denne inneholder alle betalingsdetaljer og kan brukes til skatteformål.\n\nDu kan se bidragshistorikken din, laste ned fakturaer, {interval, select,\n  month {og administrere eller kansellere abonnementet ditt}\n  year {og administrere eller kansellere abonnementet ditt}\n  other {og administrere fremtidige bidrag}\n} når som helst ved å bruke denne lenken:\n\n{manageUrl}\n\nDin støtte bidrar til å holde {product_name} i gang. Takk!\n\n– {product_name} Team"
	},
	"donation_magic_link": {
		"subject": "Administrer {product_name}-bidragene dine",
		"body": "Hei,\n\nKlikk på lenken nedenfor for å få tilgang til din giverportal:\n\n{manageUrl}\n\nI portalen kan du administrere abonnementer, laste ned fakturaer og se bidragshistorikken din.\n\nDenne lenken utløper {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke ba om denne lenken, kan du trygt ignorere denne e-posten.\n\n– {product_name} Team"
	},
	"dsa_report_verification": {
		"subject": "Bekreft e-posten din for en DSA-rapport",
		"body": "Hei,\n\nBruk bekreftelseskoden nedenfor for å sende inn din Digital Services Act-rapport for {product_name}:\n\n{code}\n\nDenne koden utløper {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke ba om dette, ignorer denne e-posten.\n\n– {product_name} Safety Team"
	},
	"email_change_new": {
		"subject": "Bekreft din nye {product_name}-e-postadresse",
		"body": "Hei {username},\n\nSkriv inn denne koden i appen for å bekrefte din nye {product_name}-e-postadresse:\n\n{code}\n\nDenne koden utløper {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke ba om dette, ignorer denne e-posten.\n\n– {product_name} Team"
	},
	"email_change_original": {
		"subject": "Bekreft endring av {product_name}-e-postadressen din",
		"body": "Hei {username},\n\nVi mottok en forespørsel om å endre e-postadressen til {product_name}-kontoen din.\n\nFor å bekrefte denne endringen, skriv inn denne koden i appen:\n\n{code}\n\nDenne koden utløper {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke ba om dette, sikre kontoen din med en gang.\n\n– {product_name} Team"
	},
	"email_change_revert": {
		"subject": "{product_name}-e-postadressen din ble endret",
		"body": "Hei {username},\n\nE-postadressen til {product_name}-kontoen din ble endret til {newEmail}.\n\nHvis du gjorde denne endringen, er ingen handling nødvendig. Hvis du ikke gjorde det, angre endringen og sikre kontoen din med denne lenken:\n\n{revertUrl}\n\nDette vil gjenopprette din tidligere e-post, logge deg ut overalt, fjerne tilknyttede telefonnumre, deaktivere MFA og kreve at du må angi et nytt passord.\n\n– {product_name} Safety Team"
	},
	"email_verification": {
		"subject": "Bekreft {product_name}-e-postadressen din",
		"body": "Hei {username},\n\nBekreft e-postadressen til {product_name}-kontoen din ved å klikke på lenken nedenfor:\n\n{verifyUrl}\n\nHvis du ikke opprettet en {product_name}-konto, ignorer denne e-posten.\n\nDenne lenken er gyldig i 24 timer.\n\n– {product_name} Team"
	},
	"gift_chargeback_notification": {
		"subject": "Fordelene fra gaven du løste inn er fjernet",
		"body": "Hei {username},\n\nEn gavekode du løste inn ble opprinnelig betalt av noen andre. Den betalingen er siden blitt tilbakeført (en tilbakeføring).\n\nPå grunn av dette har vi fjernet fordelene som ble lagt til kontoen din da du løste inn gaven.\n\nHvis du tror dette er en feil, kontakt kundestøtte og inkluder alle detaljer du har om gavekoden og når du løste den inn.\n\n– {product_name} Team"
	},
	"harvest_completed": {
		"subject": "{product_name}-dataeksporten din er klar til nedlasting",
		"body": "Hei {username},\n\nDataeksporten din er klar.\n\nNedlastingslenke:\n{downloadUrl}\n\nAntall meldinger: {totalMessages, number}\nFilstørrelse: {fileSizeMB, number} MB\n\nDenne lenken utløper {expiresAt, date, full} kl. {expiresAt, time, short}.\n\nHvis du ikke ba om denne eksporten, endre passordet ditt umiddelbart og kontakt kundestøtte.\n\n– {product_name} Team"
	},
	"inactivity_warning": {
		"subject": "{product_name}-kontoen din vil bli slettet på grunn av inaktivitet",
		"body": "Hei {username},\n\nVi har ikke sett noen aktivitet på {product_name}-kontoen din siden {lastActiveDate, date, full}.\n\nHvis du ikke logger inn innen {deletionDate, date, full} kl. {deletionDate, time, short}, blir kontoen din permanent slettet på grunn av inaktivitet.\n\nLogg inn her:\n{loginUrl}\n\nHvis du har brukt {product_name} nylig, kontakt kundestøtte med en gang.\n\n– {product_name} Team"
	},
	"ip_authorization": {
		"subject": "Godkjenn pålogging fra en ny IP-adresse",
		"body": "Hei {username},\n\nVi oppdaget et påloggingsforsøk til {product_name}-kontoen din fra en ny IP-adresse:\n\nIP-adresse: {ipAddress}\nSted: {location}\n\nHvis dette var deg, godkjenn denne IP-adressen ved å klikke på lenken nedenfor:\n\n{authUrl}\n\nHvis du ikke forsøkte å logge inn, endre passordet ditt med en gang.\n\nDenne lenken er gyldig i 30 minutter.\n\n– {product_name} Team"
	},
	"password_change_verification": {
		"subject": "Bekreft din {product_name}-passordendring",
		"body": "Hei {username},\n\nVi mottok en forespørsel om å endre passordet på {product_name}-kontoen din.\n\nFor å bekrefte denne endringen, skriv inn denne koden i appen:\n\n{code}\n\nDenne koden utløper {expiresAt}.\n\nHvis du ikke ba om dette, kan noen ha tilgang til kontoen din. Endre passordet ditt med en gang og aktiver tofaktorautentisering.\n\n– {product_name} Team"
	},
	"password_reset": {
		"subject": "Tilbakestill ditt {product_name}-passord",
		"body": "Hei {username},\n\nDu ba om å tilbakestille {product_name}-passordet ditt. Bruk lenken nedenfor for å angi et nytt passord:\n\n{resetUrl}\n\nHvis du ikke ba om dette, ignorer denne e-posten.\n\nDenne lenken er gyldig i 1 time.\n\n– {product_name} Team"
	},
	"registration_approved": {
		"subject": "{product_name}-registreringen din er godkjent",
		"body": "Hei {username},\n\nGode nyheter: din {product_name}-registrering er godkjent.\n\nDu kan nå logge inn på {product_name}-appen her:\n{channelsUrl}\n\nVelkommen til {product_name}-communityet.\n\n– {product_name} Team"
	},
	"report_resolved": {
		"subject": "{product_name}-rapporten din er gjennomgått",
		"body": "Hei {username},\n\nRapporten din (ID: {reportId}) er gjennomgått av vårt Safety Team.{hasComment, select, yes {\n\nSvar fra Safety Team:\n{publicComment}} other {}}\n\nTakk for at du bidrar til å holde {product_name} trygt for alle. Vi tar alle rapporter på alvor og setter pris på ditt bidrag til communityet.\n\nHvis du har spørsmål eller bekymringer angående dette utfallet, kontakt {safety_email}.\n\n– {product_name} Safety Team"
	},
	"scheduled_deletion_notification": {
		"subject": "{product_name}-kontoen din blir permanent slettet",
		"body": "Hei {username},\n\n{product_name}-kontoen din er planlagt for permanent sletting.\n\nPlanlagt sletting: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Årsak: {reason}}\n}\n\nDette er et alvorlig tiltak. Kontodataene dine blir permanent slettet på den planlagte datoen.\n\nHvis du mener denne håndhevelsesbeslutningen var feil, kan du sende inn en anke. E-post {appeals_email} fra denne e-postadressen.\n\n– {product_name} Safety Team"
	},
	"self_deletion_scheduled": {
		"subject": "Slettingen av {product_name}-kontoen din er planlagt",
		"body": "Hei {username},\n\nDu ba om å slette {product_name}-kontoen din. Kontoen din er planlagt for permanent sletting:\n\n{deletionDate, date, full} kl. {deletionDate, time, short}\n\nHvis du ikke ba om dette, logg inn på kontoen din for å avbryte slettingen. Vi anbefaler også å endre passordet ditt for å sikre kontoen din.\n\n– {product_name} Team"
	},
	"unban_notification": {
		"subject": "Suspensjonen av {product_name}-kontoen din er opphevet",
		"body": "Hei {username},\n\nGode nyheter: suspensjonen av {product_name}-kontoen din er opphevet.\n\n{reason, select,\n  null {}\n  other {Årsak: {reason}}\n}\n\nDu kan nå logge inn igjen og fortsette å bruke {product_name} som normalt.\n\n– {product_name} Safety Team"
	}
});

export default EMAIL_I18N_NO_MESSAGES;
