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

export const svSE: EmailTranslations = {
	passwordReset: {
		subject: 'Återställ ditt Fluxer-lösenord',
		body: `Hej {username},

Du har begärt att återställa lösenordet för ditt Fluxer-konto. Följ länken nedan för att skapa ett nytt lösenord:

{resetUrl}

Om du inte begärde detta kan du ignorera detta mejl.

Länken är giltig i 1 timme.

- Fluxer-teamet`,
	},
	emailVerification: {
		subject: 'Verifiera din e-postadress för Fluxer',
		body: `Hej {username},

Verifiera din e-postadress för ditt Fluxer-konto genom att klicka på länken nedan:

{verifyUrl}

Om du inte skapade ett Fluxer-konto kan du bortse från detta mejl.

Länken är giltig i 24 timmar.

- Fluxer-teamet`,
	},
	ipAuthorization: {
		subject: 'Godkänn inloggning från ny IP-adress',
		body: `Hej {username},

Vi upptäckte ett inloggningsförsök till ditt Fluxer-konto från en ny IP-adress:

IP-adress: {ipAddress}
Plats: {location}

Om detta var du, godkänn IP-adressen via länken nedan:

{authUrl}

Om du inte försökte logga in bör du omedelbart byta lösenord.

Denna länk upphör att gälla om 30 minuter.

- Fluxer-teamet`,
	},
	accountDisabledSuspicious: {
		subject: 'Ditt Fluxer-konto har tillfälligt inaktiverats',
		body: `Hej {username},

Ditt Fluxer-konto har tillfälligt inaktiverats på grund av misstänkt aktivitet.

{reason, select,
	null {}
	other {Anledning: {reason}

}}För att återfå åtkomst måste du återställa ditt lösenord:

{forgotUrl}

När ditt lösenord är återställt kan du logga in igen.

Om du anser att detta skedde av misstag, kontakta vårt supportteam.

- Fluxers säkerhetsteam`,
	},
	accountTempBanned: {
		subject: 'Ditt Fluxer-konto har tillfälligt spärrats',
		body: `Hej {username},

Ditt Fluxer-konto har tillfälligt spärrats för att du brutit mot våra användarvillkor eller riktlinjer för communityt.

Varaktighet: {durationHours, plural,
	=1 {1 timme}
	other {# timmar}
}
Spärrat till: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Anledning: {reason}}
}

Under denna period har du inte åtkomst till ditt konto.

Vi rekommenderar att du läser:
- Användarvillkor: {termsUrl}
- Community-riktlinjer: {guidelinesUrl}

Om du anser att detta beslut är felaktigt eller orättvist kan du överklaga genom att mejla appeals@fluxer.app från denna e-postadress.  
Förklara tydligt varför du anser att beslutet är felaktigt. Vi kommer att granska ditt överklagande och återkomma med ett beslut.

- Fluxers säkerhetsteam`,
	},
	accountScheduledDeletion: {
		subject: 'Ditt Fluxer-konto är schemalagt för radering',
		body: `Hej {username},

Ditt Fluxer-konto är schemalagt för permanent radering på grund av överträdelser av våra användarvillkor eller community-riktlinjer.

Planerat raderingsdatum: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Anledning: {reason}}
}

Detta är en allvarlig åtgärd. Dina kontodata kommer att raderas permanent det angivna datumet.

Vi rekommenderar att du läser:
- Användarvillkor: {termsUrl}
- Community-riktlinjer: {guidelinesUrl}

ÖVERKLAGANDEPROCESS:
Om du anser att detta beslut är felaktigt eller orättvist har du 30 dagar på dig att skicka ett överklagande till appeals@fluxer.app från denna e-postadress.

Inkludera i ditt överklagande:
- En tydlig förklaring till varför du anser att beslutet är felaktigt
- Eventuell relevant bevisning eller sammanhang

En medlem av Fluxers säkerhetsteam kommer att granska ditt ärende och kan skjuta upp raderingen tills ett slutgiltigt beslut tas.

- Fluxers säkerhetsteam`,
	},
	selfDeletionScheduled: {
		subject: 'Radering av ditt Fluxer-konto har schemalagts',
		body: `Hej {username},

Vi är ledsna att se dig lämna! Radering av ditt Fluxer-konto har schemalagts.

Planerat raderingsdatum: {deletionDate, date, full} {deletionDate, time, short}

VIKTIGT: Du kan avbryta denna radering när som helst före {deletionDate, date, full} {deletionDate, time, short} genom att logga in igen.

INNAN DU GÅR:
Integritetspanelen under användarinställningar låter dig:
- Radera dina meddelanden på plattformen
- Exportera viktig data innan du lämnar

Observera: När kontot väl är raderat går det inte längre att ta bort meddelanden. Om du vill radera dem måste du göra det innan raderingen slutförs.

Om du ångrar dig kan du bara logga in igen för att avbryta raderingen.

- Fluxer-teamet`,
	},
	inactivityWarning: {
		subject: 'Ditt Fluxer-konto kommer att raderas på grund av inaktivitet',
		body: `Hej {username},

Vi har märkt att du inte har loggat in på ditt Fluxer-konto på över 2 år.

Senaste inloggning: {lastActiveDate, date, full} {lastActiveDate, time, short}

Enligt vår policy för datalagring schemaläggs inaktiva konton automatiskt för radering.

Planerat raderingsdatum: {deletionDate, date, full} {deletionDate, time, short}

SÅ HÄR BEHÅLLER DU DITT KONTO:
Logga bara in på {loginUrl} innan raderingsdatumet så avbryts den automatiska raderingen.

OM DU INTE LOGGAR IN:
- Ditt konto och alla tillhörande data kommer att raderas permanent
- Dina meddelanden anonymiseras ("Deleted User")
- Denna åtgärd kan inte ångras

VILL DU SJÄLV RADERA DINA MEDDELANDEN?
Logga in och använd integritetspanelen innan kontot raderas.

Vi hoppas få se dig tillbaka på Fluxer!

- Fluxer-teamet`,
	},
	harvestCompleted: {
		subject: 'Din Fluxer-dataexport är klar',
		body: `Hej {username},

Din dataexport har slutförts och är nu redo för nedladdning!

Exportöversikt:
- Totalt antal meddelanden: {totalMessages, number}
- Filstorlek: {fileSizeMB} MB
- Format: ZIP-arkiv med JSON-filer

Ladda ner din data här: {downloadUrl}

VIKTIGT: Denna nedladdningslänk upphör att gälla {expiresAt, date, full} {expiresAt, time, short}

Exporten inkluderar:
- Alla dina meddelanden organiserade per kanal
- Kanalmetadata
- Din profil- och kontoinformation
- Inställningar och medlemskap i guilds
- Autentiseringssessioner och säkerhetsinformation

Data levereras i JSON-format för enkel analys.

Om du har frågor är du välkommen att kontakta support@fluxer.app

- Fluxer-teamet`,
	},
	unbanNotification: {
		subject: 'Din Fluxer-avstängning har hävts',
		body: `Hej {username},

Goda nyheter! Avstängningen av ditt Fluxer-konto har hävts.

Anledning: {reason}

Du kan nu logga in och fortsätta använda Fluxer.

- Fluxers säkerhetsteam`,
	},
	scheduledDeletionNotification: {
		subject: 'Ditt Fluxer-konto är planerat för radering',
		body: `Hej {username},

Ditt Fluxer-konto är planerat för permanent radering.

Raderingsdatum: {deletionDate, date, full} {deletionDate, time, short}
Anledning: {reason}

Detta är en allvarlig åtgärd — dina kontodata kommer att raderas permanent.

Om du anser att detta är fel kan du skicka ett överklagande till appeals@fluxer.app

- Fluxers säkerhetsteam`,
	},
	giftChargebackNotification: {
		subject: 'Din Fluxer Premium-gåva har återkallats',
		body: `Hej {username},

Vi vill informera dig om att din inlösta Fluxer Premium-gåva har återkallats på grund av en betalningstvist (chargeback) initierad av den ursprungliga köparen.

Dina premiumförmåner har tagits bort från kontot. Detta beror på att betalningen återkallats.

Vid frågor kan du kontakta support@fluxer.app

- Fluxer-teamet`,
	},
	reportResolved: {
		subject: 'Din Fluxer-anmälan har behandlats',
		body: `Hej {username},

Din anmälan (ID: {reportId}) har nu behandlats av vårt säkerhetsteam.

Säkerhetsteamets svar:
{publicComment}

Tack för att du hjälper till att hålla Fluxer säkert för alla. Vi uppskattar ditt engagemang för vårt community.

Om du har frågor eller funderingar, kontakta safety@fluxer.app

- Fluxers säkerhetsteam`,
	},
	dsaReportVerification: {
		subject: 'Verifiera din e-post för en DSA-anmälan',
		body: `Hej,

Använd följande verifieringskod för att skicka in din anmälan enligt lagen om digitala tjänster på Fluxer:

{code}

Denna kod upphör att gälla {expiresAt, date, full} {expiresAt, time, short}.

Om du inte begärde detta kan du ignorera detta mejl.

- Fluxers säkerhetsteam`,
	},
	registrationApproved: {
		subject: 'Din Fluxer-registrering har godkänts',
		body: `Hej {username},

Goda nyheter! Din registrering för Fluxer har godkänts.

Du kan nu logga in i Fluxer-appen här:
{channelsUrl}

Välkommen till Fluxer-communityt!

- Fluxer-teamet`,
	},
	emailChangeRevert: {
		subject: 'Din Fluxer-e-post har ändrats',
		body: `Hej {username},

E-postadressen för ditt Fluxer-konto har ändrats till {newEmail}.

Om du gjorde ändringen behöver du inte göra något mer. Om inte kan du ångra den och säkra kontot via denna länk:

{revertUrl}

Detta återställer din tidigare e-post, loggar ut dig överallt, tar bort kopplade telefonnummer, inaktiverar MFA och kräver ett nytt lösenord.

- Fluxers säkerhetsteam`,
	},
};
