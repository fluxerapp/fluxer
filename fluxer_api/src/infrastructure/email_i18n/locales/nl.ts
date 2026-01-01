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

export const nl: EmailTranslations = {
	passwordReset: {
		subject: 'Stel je Fluxer-wachtwoord opnieuw in',
		body: `Hallo {username},

Je hebt verzocht om het wachtwoord van je Fluxer-account opnieuw in te stellen. Volg de link hieronder om een nieuw wachtwoord te kiezen:

{resetUrl}

Als je deze aanvraag niet hebt gedaan, kun je deze e-mail veilig negeren.

Deze link verloopt over 1 uur.

- Het Fluxer-team`,
	},
	emailVerification: {
		subject: 'Bevestig je Fluxer e-mailadres',
		body: `Hallo {username},

Bevestig het e-mailadres van je Fluxer-account door op de onderstaande link te klikken:

{verifyUrl}

Als je geen Fluxer-account hebt aangemaakt, kun je deze e-mail negeren.

Deze link verloopt over 24 uur.

- Het Fluxer-team`,
	},
	ipAuthorization: {
		subject: 'Autoriseer login vanaf een nieuw IP-adres',
		body: `Hallo {username},

We hebben een poging tot inloggen op je Fluxer-account gedetecteerd vanaf een nieuw IP-adres:

IP-adres: {ipAddress}
Locatie: {location}

Als jij dit was, autoriseer dan dit IP-adres via de onderstaande link:

{authUrl}

Als jij dit niet was, wijzig dan onmiddellijk je wachtwoord.

Deze autorisatielink verloopt over 30 minuten.

- Het Fluxer-team`,
	},
	accountDisabledSuspicious: {
		subject: 'Je Fluxer-account is tijdelijk uitgeschakeld',
		body: `Hallo {username},

Je Fluxer-account is tijdelijk uitgeschakeld vanwege verdachte activiteiten.

{reason, select,
	null {}
	other {Reden: {reason}

}}Om opnieuw toegang te krijgen, moet je je wachtwoord opnieuw instellen:

{forgotUrl}

Na het opnieuw instellen van je wachtwoord kun je weer inloggen.

Als je denkt dat dit een fout is, neem dan contact op met ons ondersteuningsteam.

- Het Fluxer Veiligheidsteam`,
	},
	accountTempBanned: {
		subject: 'Je Fluxer-account is tijdelijk geschorst',
		body: `Hallo {username},

Je Fluxer-account is tijdelijk geschorst wegens schending van onze Servicevoorwaarden of Gemeenschapsrichtlijnen.

Duur: {durationHours, plural,
	=1 {1 uur}
	other {# uur}
}
Geschorst tot: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Reden: {reason}}
}

Tijdens deze schorsing heb je geen toegang tot je account.

We raden je aan onze documenten te bekijken:
- Servicevoorwaarden: {termsUrl}
- Gemeenschapsrichtlijnen: {guidelinesUrl}

Als je denkt dat deze beslissing onjuist is, kun je een beroep indienen via appeals@fluxer.app. Leg duidelijk uit waarom de beslissing volgens jou onterecht is. We beoordelen je beroep en laten je onze beslissing weten.

- Het Fluxer Veiligheidsteam`,
	},
	accountScheduledDeletion: {
		subject: 'Je Fluxer-account staat gepland voor verwijdering',
		body: `Hallo {username},

Je Fluxer-account staat gepland voor permanente verwijdering wegens overtreding van onze Servicevoorwaarden of Gemeenschapsrichtlijnen.

Geplande verwijderingsdatum: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Reden: {reason}}
}

Dit is een serieuze handhavingsmaatregel. Je accountgegevens worden permanent verwijderd op de geplande datum.

We raden je aan het volgende door te nemen:
- Servicevoorwaarden: {termsUrl}
- Gemeenschapsrichtlijnen: {guidelinesUrl}

BEROEPSPROCEDURE:
Als je denkt dat deze beslissing onjuist of onterecht is, heb je 30 dagen om een beroep in te dienen via appeals@fluxer.app.

Vermeld in je beroep:
- Waarom je denkt dat de beslissing onjuist is
- Eventuele relevante context of bewijs

Een lid van het Fluxer Veiligheidsteam beoordeelt je beroep en kan de verwijdering uitstellen tot een definitieve beslissing is genomen.

- Het Fluxer Veiligheidsteam`,
	},
	selfDeletionScheduled: {
		subject: 'De verwijdering van je Fluxer-account is ingepland',
		body: `Hallo {username},

Jammer dat je ons verlaat! De verwijdering van je Fluxer-account is ingepland.

Geplande verwijderingsdatum: {deletionDate, date, full} {deletionDate, time, short}

BELANGRIJK: Je kunt deze verwijdering op elk moment annuleren vóór {deletionDate, date, full} {deletionDate, time, short} door opnieuw in te loggen op je account.

VOORDAT JE GAAT:
Je Privacydashboard in de gebruikersinstellingen laat je:
- Je berichten op het platform verwijderen
- Waardevolle data exporteren voordat je vertrekt

Let op: zodra je account is verwijderd, kun je geen berichten meer verwijderen. Doe dit via het Privacydashboard voordat de verwijdering is voltooid.

Als je van gedachten verandert, log dan gewoon opnieuw in om de verwijdering te annuleren.

- Het Fluxer-team`,
	},
	inactivityWarning: {
		subject: 'Je Fluxer-account wordt verwijderd wegens inactiviteit',
		body: `Hallo {username},

We hebben gemerkt dat je al meer dan 2 jaar niet hebt ingelogd op je Fluxer-account.

Laatste login: {lastActiveDate, date, full} {lastActiveDate, time, short}

Volgens ons beleid voor gegevensbewaring worden inactieve accounts automatisch gepland voor verwijdering. Je account wordt permanent verwijderd op:

Geplande verwijderingsdatum: {deletionDate, date, full} {deletionDate, time, short}

HOE JE JE ACCOUNT KUNT BEHOUDEN:
Log gewoon in op {loginUrl} vóór de verwijderingsdatum om automatische verwijdering te annuleren.

ALS JE NIET INLOGT:
- Je account en alle gegevens worden permanent verwijderd
- Je berichten worden geanonimiseerd (“Verwijderde gebruiker”)
- Deze actie kan niet ongedaan worden gemaakt

WIL JE JE BERICHTEN VERWIJDEREN?
Als je je berichten wilt verwijderen voordat je account wordt verwijderd, log dan in en gebruik het Privacydashboard.

We hopen je terug te zien op Fluxer!

- Het Fluxer-team`,
	},
	harvestCompleted: {
		subject: 'Je Fluxer-gegevensexport is klaar',
		body: `Hallo {username},

Je gegevensexport is voltooid en staat klaar om te worden gedownload!

Exportoverzicht:
- Totaal aantal berichten: {totalMessages, number}
- Bestandsgrootte: {fileSizeMB} MB
- Formaat: ZIP-archief met JSON-bestanden

Download je gegevens: {downloadUrl}

BELANGRIJK: Deze downloadlink verloopt op {expiresAt, date, full} {expiresAt, time, short}

Je export bevat:
- Al je berichten per kanaal georganiseerd
- Kanaalmetadata
- Je gebruikersprofiel en accountinformatie
- Guild-lidmaatschappen en instellingen
- Authenticatiesessies en beveiligingsinformatie

De data wordt in JSON-formaat aangeleverd voor eenvoudige analyse.

Heb je vragen? Neem contact op via support@fluxer.app

- Het Fluxer-team`,
	},
	unbanNotification: {
		subject: 'Je schorsing voor Fluxer is opgeheven',
		body: `Hallo {username},

Goed nieuws! De schorsing van je Fluxer-account is opgeheven.

Reden: {reason}

Je kunt nu opnieuw inloggen en Fluxer blijven gebruiken.

- Het Fluxer Veiligheidsteam`,
	},
	scheduledDeletionNotification: {
		subject: 'Je Fluxer-account staat gepland voor verwijdering',
		body: `Hallo {username},

Je Fluxer-account staat gepland voor permanente verwijdering.

Verwijderingsdatum: {deletionDate, date, full} {deletionDate, time, short}
Reden: {reason}

Dit is een serieuze maatregel. Je accountgegevens worden verwijderd op de geplande datum.

Als je denkt dat dit onterecht is, kun je een beroep indienen via appeals@fluxer.app.

- Het Fluxer Veiligheidsteam`,
	},
	giftChargebackNotification: {
		subject: 'Je Fluxer Premium-cadeau is ingetrokken',
		body: `Hallo {username},

We informeren je dat het Fluxer Premium-cadeau dat je hebt ingewisseld, is ingetrokken vanwege een betalingsgeschil (chargeback) dat door de oorspronkelijke koper is gestart.

Je premiumvoordelen zijn van je account verwijderd omdat de betaling is teruggedraaid.

Bij vragen kun je contact opnemen via support@fluxer.app.

- Het Fluxer-team`,
	},
	reportResolved: {
		subject: 'Je Fluxer-melding is beoordeeld',
		body: `Hallo {username},

Je melding (ID: {reportId}) is beoordeeld door ons Veiligheidsteam.

Reactie van het Veiligheidsteam:
{publicComment}

Bedankt dat je helpt Fluxer veilig te houden voor iedereen. We nemen alle meldingen serieus en waarderen je bijdrage aan onze community.

Als je vragen of zorgen hebt, neem dan contact op via safety@fluxer.app.

- Het Fluxer Veiligheidsteam`,
	},
	dsaReportVerification: {
		subject: 'Verifieer je e-mail voor een DSA-melding',
		body: `Hallo,

Gebruik de volgende verificatiecode om je Digital Services Act-melding in te dienen op Fluxer:

{code}

Deze code verloopt op {expiresAt, date, full} {expiresAt, time, short}.

Als je dit niet hebt aangevraagd, kun je deze e-mail negeren.

- Het Fluxer Veiligheidsteam`,
	},
	registrationApproved: {
		subject: 'Je Fluxer-registratie is goedgekeurd',
		body: `Hallo {username},

Goed nieuws! Je registratie voor Fluxer is goedgekeurd.

Je kunt nu inloggen in de Fluxer-app via:
{channelsUrl}

Welkom bij de Fluxer-community!

- Het Fluxer-team`,
	},
	emailChangeRevert: {
		subject: 'Je Fluxer-e-mail is gewijzigd',
		body: `Hallo {username},

Het e-mailadres van je Fluxer-account is gewijzigd naar {newEmail}.

Als je dit zelf hebt gedaan, hoef je niets te doen. Zo niet, dan kun je het ongedaan maken en je account beveiligen via deze link:

{revertUrl}

Hiermee wordt je vorige e-mail hersteld, word je overal uitgelogd, worden gekoppelde telefoonnummers verwijderd, MFA uitgeschakeld en een nieuw wachtwoord vereist.

- Fluxer-beveiligingsteam`,
	},
};
