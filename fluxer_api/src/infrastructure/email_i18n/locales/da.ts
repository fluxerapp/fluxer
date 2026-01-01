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

export const da: EmailTranslations = {
	passwordReset: {
		subject: 'Nulstil din Fluxer-adgangskode',
		body: `Hej {username},

Du har anmodet om at nulstille adgangskoden til din Fluxer-konto. Følg venligst linket nedenfor for at vælge en ny adgangskode:

{resetUrl}

Hvis du ikke har anmodet om at nulstille adgangskoden, kan du roligt ignorere denne e-mail.

Dette link udløber om 1 time.

– Fluxer-teamet`,
	},
	emailVerification: {
		subject: 'Bekræft din e-mailadresse til Fluxer',
		body: `Hej {username},

Bekræft venligst din e-mailadresse til din Fluxer-konto ved at klikke på linket nedenfor:

{verifyUrl}

Hvis du ikke har oprettet en Fluxer-konto, kan du roligt ignorere denne e-mail.

Dette link udløber om 24 timer.

– Fluxer-teamet`,
	},
	ipAuthorization: {
		subject: 'Godkend login fra ny IP-adresse',
		body: `Hej {username},

Vi har registreret et loginforsøg på din Fluxer-konto fra en ny IP-adresse:

IP-adresse: {ipAddress}
Placering: {location}

Hvis det var dig, skal du godkende denne IP-adresse ved at klikke på linket nedenfor:

{authUrl}

Hvis du ikke forsøgte at logge ind, bør du straks ændre din adgangskode.

Dette godkendelseslink udløber om 30 minutter.

– Fluxer-teamet`,
	},
	accountDisabledSuspicious: {
		subject: 'Din Fluxer-konto er midlertidigt deaktiveret',
		body: `Hej {username},

Din Fluxer-konto er midlertidigt blevet deaktiveret på grund af mistænkelig aktivitet.

{reason, select,
	null {}
	other {Årsag: {reason}

}}For at få adgang til din konto igen skal du nulstille din adgangskode:

{forgotUrl}

Når du har nulstillet din adgangskode, kan du logge ind igen.

Hvis du mener, at denne handling er foretaget ved en fejl, bedes du kontakte vores supportteam.

– Fluxer-sikkerhedsteamet`,
	},
	accountTempBanned: {
		subject: 'Din Fluxer-konto er midlertidigt suspenderet',
		body: `Hej {username},

Din Fluxer-konto er midlertidigt suspenderet for overtrædelse af vores servicevilkår eller fællesskabsretningslinjer.

Varighed: {durationHours, plural,
	=1 {1 time}
	other {# timer}
}
Suspenderet til: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Årsag: {reason}}
}

I denne periode vil du ikke kunne få adgang til din konto.

Vi anbefaler, at du gennemgår vores:
- Servicevilkår: {termsUrl}
- Fællesskabsretningslinjer: {guidelinesUrl}

Hvis du mener, at denne afgørelse er forkert eller uberettiget, kan du indsende en klage til appeals@fluxer.app fra denne e-mailadresse. Forklar venligst tydeligt, hvorfor du mener, at afgørelsen var forkert. Vi vil gennemgå din klage og vende tilbage med vores afgørelse.

– Fluxer-sikkerhedsteamet`,
	},
	accountScheduledDeletion: {
		subject: 'Din Fluxer-konto er planlagt til sletning',
		body: `Hej {username},

Din Fluxer-konto er blevet planlagt til permanent sletning på grund af overtrædelser af vores servicevilkår eller fællesskabsretningslinjer.

Planlagt sletningsdato: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Årsag: {reason}}
}

Dette er en alvorlig håndhævelsesforanstaltning. Dine kontodata vil blive slettet permanent på den planlagte dato.

Vi anbefaler, at du gennemgår vores:
- Servicevilkår: {termsUrl}
- Fællesskabsretningslinjer: {guidelinesUrl}

KLAGEPROCES:
Hvis du mener, at denne håndhævelsesbeslutning er forkert eller uberettiget, har du 30 dage til at indsende en klage til appeals@fluxer.app fra denne e-mailadresse.

I din klage bør du:
- Tydeligt forklare, hvorfor du mener, at beslutningen er forkert eller uberettiget
- Give relevant dokumentation eller kontekst

Et medlem af Fluxer-sikkerhedsteamet vil gennemgå din klage og kan midlertidigt sætte den planlagte sletning på pause, indtil der er truffet en endelig afgørelse.

– Fluxer-sikkerhedsteamet`,
	},
	selfDeletionScheduled: {
		subject: 'Sletning af din Fluxer-konto er planlagt',
		body: `Hej {username},

Vi er kede af at se dig gå! Sletning af din Fluxer-konto er blevet planlagt.

Planlagt sletningsdato: {deletionDate, date, full} {deletionDate, time, short}

VIGTIGT: Du kan til enhver tid annullere denne sletning inden {deletionDate, date, full} {deletionDate, time, short} ved blot at logge ind på din konto igen.

FØR DU GÅR:
Dit privatlivskontrolcenter i brugerindstillingerne giver dig mulighed for at:
- Slette dine beskeder på platformen
- Eksportere vigtige data, før du forlader tjenesten

Bemærk: Når din konto først er blevet slettet, er det ikke længere muligt at slette dine beskeder. Hvis du ønsker at slette dine beskeder, skal du gøre det via privatlivskontrolcenteret, inden kontosletningen fuldføres.

Hvis du ændrer mening, skal du blot logge ind igen for at annullere sletningen.

– Fluxer-teamet`,
	},
	inactivityWarning: {
		subject: 'Din Fluxer-konto bliver slettet på grund af inaktivitet',
		body: `Hej {username},

Vi har bemærket, at du ikke har logget ind på din Fluxer-konto i over 2 år.

Seneste login: {lastActiveDate, date, full} {lastActiveDate, time, short}

Som en del af vores politik for opbevaring af data bliver inaktive konti automatisk planlagt til sletning. Din konto vil blive permanent slettet på:

Planlagt sletningsdato: {deletionDate, date, full} {deletionDate, time, short}

SÅDAN BEHOLDER DU DIN KONTO:
Du skal blot logge ind på din konto på {loginUrl} før sletningsdatoen for at annullere denne automatiske sletning. Der kræves ingen yderligere handling.

HVAD SKER DER, HVIS DU IKKE LOGGER IND:
- Din konto og alle tilknyttede data vil blive slettet permanent
- Dine beskeder vil blive anonymiseret (tilskrevet “Slettet bruger”)
- Denne handling kan ikke fortrydes

VIL DU SLETTE DINE BESKEDER?
Hvis du ønsker at slette dine beskeder, inden din konto slettes, skal du logge ind og bruge privatlivskontrolcenteret i brugerindstillingerne.

Vi håber at se dig tilbage på Fluxer!

– Fluxer-teamet`,
	},
	harvestCompleted: {
		subject: 'Din Fluxer-dataeksport er klar',
		body: `Hej {username},

Din dataeksport er fuldført og er klar til download!

Eksportsammendrag:
- Samlet antal beskeder: {totalMessages, number}
- Filstørrelse: {fileSizeMB} MB
- Format: ZIP-arkiv med JSON-filer

Download dine data: {downloadUrl}

VIGTIGT: Dette downloadlink udløber den {expiresAt, date, full} {expiresAt, time, short}

Hvad er inkluderet i eksporten:
- Alle dine beskeder organiseret efter kanal
- Kanalmetadata
- Din brugerprofil og kontooplysninger
- Guild-medlemskaber og indstillinger
- Godkendelsessessioner og sikkerhedsoplysninger

Dataene er organiseret i JSON-format for nem parsing og analyse.

Hvis du har spørgsmål til din dataeksport, kan du kontakte support@fluxer.app

– Fluxer-teamet`,
	},
	unbanNotification: {
		subject: 'Suspenderingen af din Fluxer-konto er ophævet',
		body: `Hej {username},

Gode nyheder! Suspenderingen af din Fluxer-konto er blevet ophævet.

Årsag: {reason}

Du kan nu logge ind på din konto igen og fortsætte med at bruge Fluxer.

– Fluxer-sikkerhedsteamet`,
	},
	scheduledDeletionNotification: {
		subject: 'Din Fluxer-konto er planlagt til sletning',
		body: `Hej {username},

Din Fluxer-konto er blevet planlagt til permanent sletning.

Planlagt sletningsdato: {deletionDate, date, full} {deletionDate, time, short}
Årsag: {reason}

Dette er en alvorlig håndhævelsesforanstaltning. Dine kontodata vil blive slettet permanent på den planlagte dato.

Hvis du mener, at denne beslutning er forkert, kan du indsende en klage til appeals@fluxer.app fra denne e-mailadresse.

– Fluxer-sikkerhedsteamet`,
	},
	giftChargebackNotification: {
		subject: 'Din Fluxer Premium-gave er blevet tilbagekaldt',
		body: `Hej {username},

Vi skriver for at informere dig om, at den Fluxer Premium-gave, du har indløst, er blevet tilbagekaldt på grund af en betalingstvist (chargeback), som den oprindelige køber har rejst.

Dine premiumfordele er blevet fjernet fra din konto. Denne handling blev foretaget, fordi betalingen for gaven blev omstødt.

Hvis du har spørgsmål til dette, kan du kontakte support@fluxer.app.

– Fluxer-teamet`,
	},
	reportResolved: {
		subject: 'Din Fluxer-rapport er blevet gennemgået',
		body: `Hej {username},

Din rapport (ID: {reportId}) er blevet gennemgået af vores sikkerhedsteam.

Svar fra sikkerhedsteamet:
{publicComment}

Tak fordi du hjælper med at holde Fluxer sikkert for alle. Vi tager alle rapporter alvorligt og værdsætter dit bidrag til vores fællesskab.

Hvis du har spørgsmål eller bekymringer vedrørende denne afgørelse, kan du kontakte safety@fluxer.app.

– Fluxer-sikkerhedsteamet`,
	},
	dsaReportVerification: {
		subject: 'Bekræft din e-mail til en DSA-rapport',
		body: `Hej,

Brug følgende verifikationskode til at indsende din rapport i henhold til loven om digitale tjenester på Fluxer:

{code}

Denne kode udløber den {expiresAt, date, full} {expiresAt, time, short}.

Hvis du ikke har anmodet om dette, kan du roligt ignorere denne e-mail.

– Fluxer-sikkerhedsteamet`,
	},
	registrationApproved: {
		subject: 'Din Fluxer-registrering er godkendt',
		body: `Hej {username},

Gode nyheder! Din registrering på Fluxer er blevet godkendt.

Du kan nu logge ind i Fluxer-appen på:
{channelsUrl}

Velkommen til Fluxer-fællesskabet!

– Fluxer-teamet`,
	},
	emailChangeRevert: {
		subject: 'Din Fluxer-e-mail er blevet ændret',
		body: `Hej {username},

E-mailen for din Fluxer-konto er blevet ændret til {newEmail}.

Hvis du foretog ændringen, behøver du ikke gøre mere. Hvis ikke, kan du fortryde og sikre kontoen via dette link:

{revertUrl}

Det gendanner din tidligere e-mail, logger dig ud alle steder, fjerner tilknyttede telefonnumre, deaktiverer MFA og kræver en ny adgangskode.

- Fluxer Sikkerhedsteam`,
	},
};
