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

export const no: EmailTranslations = {
	passwordReset: {
		subject: 'Tilbakestill Fluxer-passordet ditt',
		body: `Hei {username},

Du har bedt om å tilbakestille passordet for Fluxer-kontoen din. Følg lenken nedenfor for å opprette et nytt passord:

{resetUrl}

Hvis du ikke ba om dette, kan du trygt ignorere denne e-posten.

Denne lenken utløper om 1 time.

- Fluxer-teamet`,
	},
	emailVerification: {
		subject: 'Bekreft Fluxer-e-postadressen din',
		body: `Hei {username},

Bekreft e-postadressen for Fluxer-kontoen din ved å klikke på lenken nedenfor:

{verifyUrl}

Hvis du ikke opprettet en Fluxer-konto, kan du ignorere denne e-posten.

Denne lenken utløper om 24 timer.

- Fluxer-teamet`,
	},
	ipAuthorization: {
		subject: 'Godkjenn innlogging fra ny IP-adresse',
		body: `Hei {username},

Vi oppdaget et innloggingsforsøk på Fluxer-kontoen din fra en ny IP-adresse:

IP-adresse: {ipAddress}
Sted: {location}

Hvis dette var deg, vennligst godkjenn IP-adressen ved å klikke på lenken nedenfor:

{authUrl}

Hvis du ikke forsøkte å logge inn, bør du endre passordet ditt umiddelbart.

Denne godkjenningslenken utløper om 30 minutter.

- Fluxer-teamet`,
	},
	accountDisabledSuspicious: {
		subject: 'Fluxer-kontoen din er midlertidig deaktivert',
		body: `Hei {username},

Fluxer-kontoen din har blitt midlertidig deaktivert på grunn av mistenkelig aktivitet.

{reason, select,
	null {}
	other {Årsak: {reason}

}}For å gjenopprette tilgangen må du tilbakestille passordet ditt:

{forgotUrl}

Etter at passordet er tilbakestilt, kan du logge inn igjen.

Hvis du mener dette er en feil, vennligst kontakt brukerstøtten vår.

- Fluxer sikkerhetsteam`,
	},
	accountTempBanned: {
		subject: 'Fluxer-kontoen din er midlertidig utestengt',
		body: `Hei {username},

Fluxer-kontoen din har blitt midlertidig utestengt for brudd på våre tjenestevilkår eller retningslinjer for fellesskapet.

Varighet: {durationHours, plural,
	=1 {1 time}
	other {# timer}
}
Utestengt til: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Årsak: {reason}}
}

I denne perioden vil du ikke ha tilgang til kontoen din.

Vi anbefaler at du leser:
- Tjenestevilkår: {termsUrl}
- Retningslinjer for fellesskapet: {guidelinesUrl}

Hvis du mener at denne avgjørelsen er feil eller urettferdig, kan du sende en klage til appeals@fluxer.app fra denne e-postadressen. Forklar tydelig hvorfor du mener avgjørelsen er feil. Vi vil gjennomgå klagen og gi deg et svar.

- Fluxer sikkerhetsteam`,
	},
	accountScheduledDeletion: {
		subject: 'Fluxer-kontoen din er planlagt for sletting',
		body: `Hei {username},

Fluxer-kontoen din er planlagt for permanent sletting på grunn av brudd på våre tjenestevilkår eller retningslinjer for fellesskapet.

Planlagt slettedato: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Årsak: {reason}}
}

Dette er et alvorlig tiltak. Kontoens data vil bli slettet permanent på den planlagte datoen.

Vi anbefaler at du leser:
- Tjenestevilkår: {termsUrl}
- Retningslinjer for fellesskapet: {guidelinesUrl}

KLAGEPROSESS:
Hvis du mener at denne avgjørelsen er feil eller urettferdig, har du 30 dager på deg til å sende en klage til appeals@fluxer.app fra denne e-posten.

Inkluder i klagen:
- En klar forklaring på hvorfor du mener avgjørelsen er feil
- Eventuelle relevante bevis eller kontekst

Et medlem av Fluxers sikkerhetsteam vil gjennomgå klagen og kan utsette slettingen inntil en endelig avgjørelse tas.

- Fluxer sikkerhetsteam`,
	},
	selfDeletionScheduled: {
		subject: 'Sletting av Fluxer-kontoen din er planlagt',
		body: `Hei {username},

Vi synes det er trist å se deg gå! Sletting av Fluxer-kontoen din er planlagt.

Planlagt slettedato: {deletionDate, date, full} {deletionDate, time, short}

VIKTIG: Du kan avbryte kontoslettingen når som helst før {deletionDate, date, full} {deletionDate, time, short} ved å logge inn på kontoen din igjen.

FØR DU DRAR:
Personvernpanelet under brukerinnstillinger lar deg:
- Slette meldingene dine på plattformen
- Laste ned viktige data før du drar

Merk: Når kontoen er slettet, kan ikke meldingene slettes lenger. Hvis du ønsker å fjerne meldingene dine, gjør det før kontoen slettes.

Hvis du ombestemmer deg, er det bare å logge inn igjen for å kansellere slettingen.

- Fluxer-teamet`,
	},
	inactivityWarning: {
		subject: 'Fluxer-kontoen din vil bli slettet på grunn av inaktivitet',
		body: `Hei {username},

Vi har registrert at du ikke har logget inn på Fluxer-kontoen din på over 2 år.

Siste innlogging: {lastActiveDate, date, full} {lastActiveDate, time, short}

I henhold til våre retningslinjer for datalagring blir inaktive kontoer automatisk planlagt for sletting.

Planlagt slettedato: {deletionDate, date, full} {deletionDate, time, short}

SLIK BEHOLDER DU KONTOEN:
Logg inn på kontoen din via {loginUrl} før slettedatoen. Ingen ytterligere handling kreves.

HVIS DU IKKE LOGGER INN:
- Kontoen din og all tilknyttet data vil bli slettet permanent
- Meldingene dine vil bli anonymisert (“Slettet bruker”)
- Denne handlingen kan ikke angres

VIL DU SLETTE MELDINGENE DINE SELV?
Logg inn og bruk personvernpanelet før kontoen slettes.

Vi håper å se deg tilbake på Fluxer!

- Fluxer-teamet`,
	},
	harvestCompleted: {
		subject: 'Fluxer-dataeksporten din er klar',
		body: `Hei {username},

Dataeksporten din er fullført og klar for nedlasting!

Eksportoversikt:
- Totalt antall meldinger: {totalMessages, number}
- Filstørrelse: {fileSizeMB} MB
- Format: ZIP-arkiv med JSON-filer

Last ned dataene dine her: {downloadUrl}

VIKTIG: Denne nedlastingslenken utløper {expiresAt, date, full} {expiresAt, time, short}

Eksporten inkluderer:
- Alle meldingene dine, sortert per kanal
- Kanalmetadata
- Brukerprofil og kontoinformasjon
- Guild-medlemskap og innstillinger
- Autentiseringsøkter og sikkerhetsinformasjon

Data leveres i JSON-format for enkel analyse.

Har du spørsmål? Kontakt support@fluxer.app

- Fluxer-teamet`,
	},
	unbanNotification: {
		subject: 'Utestengelsen av Fluxer-kontoen din er opphevet',
		body: `Hei {username},

Gode nyheter! Utestengelsen av Fluxer-kontoen din er opphevet.

Årsak: {reason}

Du kan nå logge inn igjen og fortsette å bruke Fluxer.

- Fluxer sikkerhetsteam`,
	},
	scheduledDeletionNotification: {
		subject: 'Fluxer-kontoen din står planlagt for sletting',
		body: `Hei {username},

Fluxer-kontoen din er planlagt for permanent sletting.

Slettedato: {deletionDate, date, full} {deletionDate, time, short}
Årsak: {reason}

Dette er et alvorlig tiltak. Kontoen og dataene dine vil bli slettet på den planlagte datoen.

Hvis du mener at dette er feil, kan du sende en klage til appeals@fluxer.app.

- Fluxer sikkerhetsteam`,
	},
	giftChargebackNotification: {
		subject: 'Fluxer Premium-gaven din er tilbakekalt',
		body: `Hei {username},

Vi informerer deg om at Fluxer Premium-gaven du løste inn, er tilbakekalt på grunn av en betalingskonflikt (chargeback) som ble initiert av den opprinnelige kjøperen.

Dine premiumfordeler er fjernet fra kontoen din. Dette ble gjort fordi betalingen ble tilbakeført.

Har du spørsmål? Kontakt support@fluxer.app.

- Fluxer-teamet`,
	},
	reportResolved: {
		subject: 'Fluxer-rapporten din er gjennomgått',
		body: `Hei {username},

Rapporten din (ID: {reportId}) er gjennomgått av vårt sikkerhetsteam.

Tilbakemelding fra sikkerhetsteamet:
{publicComment}

Takk for at du bidrar til å holde Fluxer trygt for alle. Vi tar alle rapporter på alvor og setter pris på ditt engasjement for fellesskapet.

Hvis du har spørsmål eller bekymringer, kontakt safety@fluxer.app.

- Fluxer sikkerhetsteam`,
	},
	dsaReportVerification: {
		subject: 'Bekreft e-posten din for en DSA-rapport',
		body: `Hei,

Bruk følgende bekreftelseskode for å sende inn din Digital Services Act-rapport på Fluxer:

{code}

Denne koden utløper {expiresAt, date, full} {expiresAt, time, short}.

Hvis du ikke ba om dette, kan du ignorere denne e-posten.

- Fluxer sikkerhetsteam`,
	},
	registrationApproved: {
		subject: 'Fluxer-registreringen din er godkjent',
		body: `Hei {username},

God nyhet! Registreringen din hos Fluxer er godkjent.

Du kan nå logge inn i Fluxer-appen via:
{channelsUrl}

Velkommen til Fluxer-fellesskapet!

- Fluxer-teamet`,
	},
	emailChangeRevert: {
		subject: 'E-posten din i Fluxer er endret',
		body: `Hei {username},

E-postadressen til Fluxer-kontoen din er endret til {newEmail}.

Hvis du gjorde endringen selv, trenger du ikke gjøre noe mer. Hvis ikke, kan du angre og sikre kontoen via denne lenken:

{revertUrl}

Dette gjenoppretter den forrige e-posten, logger deg ut overalt, fjerner tilknyttede telefonnumre, deaktiverer MFA og krever et nytt passord.

- Fluxer sikkerhetsteam`,
	},
};
