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

export const lt: EmailTranslations = {
	passwordReset: {
		subject: 'Atstatykite savo Fluxer slaptažodį',
		body: `Sveiki, {username},

Prašėte atstatyti savo Fluxer paskyros slaptažodį. Norėdami nustatyti naują slaptažodį, paspauskite žemiau esančią nuorodą:

{resetUrl}

Jei neprašėte slaptažodžio atstatymo, tiesiog ignoruokite šį laišką.

Nuoroda nustos galioti po 1 valandos.

- Fluxer komanda`,
	},
	emailVerification: {
		subject: 'Patvirtinkite savo Fluxer el. pašto adresą',
		body: `Sveiki, {username},

Norėdami patvirtinti savo Fluxer paskyros el. pašto adresą, spustelėkite žemiau esančią nuorodą:

{verifyUrl}

Jei nekūrėte Fluxer paskyros, galite saugiai ignoruoti šį laišką.

Nuoroda nustos galioti po 24 valandų.

- Fluxer komanda`,
	},
	ipAuthorization: {
		subject: 'Patvirtinkite prisijungimą iš naujo IP adreso',
		body: `Sveiki, {username},

Nustatėme bandymą prisijungti prie jūsų Fluxer paskyros iš naujo IP adreso:

IP adresas: {ipAddress}
Vieta: {location}

Jei tai buvote jūs, patvirtinkite šį IP adresą spustelėję žemiau esančią nuorodą:

{authUrl}

Jei tai nebuvote jūs, nedelsdami pakeiskite slaptažodį.

Ši patvirtinimo nuoroda nustos galioti po 30 minučių.

- Fluxer komanda`,
	},
	accountDisabledSuspicious: {
		subject: 'Jūsų Fluxer paskyra laikinai išjungta',
		body: `Sveiki, {username},

Jūsų Fluxer paskyra buvo laikinai išjungta dėl įtartinos veiklos.

{reason, select,
	null {}
	other {Priežastis: {reason}

}}Norėdami atgauti prieigą, turite atstatyti slaptažodį:

{forgotUrl}

Atstatę slaptažodį, galėsite vėl prisijungti.

Jei manote, kad klaidingai buvote užblokuoti, susisiekite su mūsų palaikymo komanda.

- Fluxer saugos komanda`,
	},
	accountTempBanned: {
		subject: 'Jūsų Fluxer paskyra laikinai suspenduota',
		body: `Sveiki, {username},

Jūsų Fluxer paskyra laikinai suspenduota dėl mūsų paslaugų teikimo taisyklių arba bendruomenės gairių pažeidimo.

Trukmė: {durationHours, plural,
	=1 {1 valanda}
	other {# valandos}
}
Suspenduota iki: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Priežastis: {reason}}
}

Šiuo laikotarpiu negalėsite naudotis savo paskyra.

Rekomenduojame perskaityti:
- Paslaugų teikimo taisykles: {termsUrl}
- Bendruomenės gaires: {guidelinesUrl}

Jei manote, kad sprendimas neteisingas, galite pateikti apeliaciją el. adresu appeals@fluxer.app. Parašykite, kodėl manote, kad sprendimas klaidingas. Mes peržiūrėsime jūsų apeliaciją ir pateiksime atsakymą.

- Fluxer saugos komanda`,
	},
	accountScheduledDeletion: {
		subject: 'Jūsų Fluxer paskyra numatyta ištrinti',
		body: `Sveiki, {username},

Jūsų Fluxer paskyra numatyta nuolatiniam ištrynimui dėl paslaugų teikimo taisyklių arba bendruomenės gairių pažeidimų.

Numatoma ištrynimo data: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Priežastis: {reason}}
}

Tai rimtas sprendimas. Vis jūsų paskyros duomenys bus visam laikui ištrinti nurodytą dieną.

Rekomenduojame perskaityti:
- Paslaugų teikimo taisykles: {termsUrl}
- Bendruomenės gaires: {guidelinesUrl}

APELIACIJŲ PROCESAS:
Jei manote, kad sprendimas neteisingas, turite 30 dienų apeliaciniam prašymui pateikti el. adresu appeals@fluxer.app.

Savo apeliacijoje:
- Aiškiai išdėstykite, kodėl sprendimas neteisingas
- Pateikite papildomų įrodymų ar konteksto

Fluxer saugos komandos narys peržiūrės apeliaciją ir gali sustabdyti ištrynimą iki galutinio sprendimo.

- Fluxer saugos komanda`,
	},
	selfDeletionScheduled: {
		subject: 'Jūsų Fluxer paskyros ištrynimas suplanuotas',
		body: `Sveiki, {username},

Gaila matyti jus išeinant! Jūsų Fluxer paskyros ištrynimas suplanuotas.

Numatoma ištrynimo data: {deletionDate, date, full} {deletionDate, time, short}

SVARBU: Galite atšaukti paskyros ištrynimą bet kada iki {deletionDate, date, full} {deletionDate, time, short}, tiesiog prisijungę prie savo paskyros.

PRIEŠ IŠEIDAMI:
Privatumo skydelis naudotojo nustatymuose leidžia:
- Ištrinti savo žinutes platformoje
- Atsisiųsti svarbius duomenis prieš išeinant

Pastaba: Kai paskyra bus ištrinta, žinučių ištrinti nebegalėsite. Jei norite jas ištrinti, padarykite tai iš anksto.

Jei persigalvosite, prisijunkite dar kartą ir atšaukite ištrynimą.

- Fluxer komanda`,
	},
	inactivityWarning: {
		subject: 'Jūsų Fluxer paskyra bus ištrinta dėl neaktyvumo',
		body: `Sveiki, {username},

Pastebėjome, kad daugiau nei 2 metus nesijungėte prie savo Fluxer paskyros.

Paskutinis prisijungimas: {lastActiveDate, date, full} {lastActiveDate, time, short}

Pagal mūsų duomenų saugojimo politiką, neaktyvios paskyros yra automatiškai suplanuojamos ištrinimui.

Numatoma ištrynimo data: {deletionDate, date, full} {deletionDate, time, short}

KAIP IŠSAUGOTI PASKYRĄ:
Tiesiog prisijunkite prie savo paskyros {loginUrl} iki nurodytos datos. Nereikia jokių papildomų veiksmų.

JEI NESIJUNGSITE:
- Jūsų paskyra ir visi duomenys bus ištrinti
- Jūsų žinutės bus anonimizuotos („Ištrintas naudotojas“)
- Šio veiksmo anuliuoti nebus galima

NORITE IŠTRINTI SAVO ŽINUTES?
Prisijunkite ir naudokite privatumo skydelį prieš ištrinant paskyrą.

Tikimės, kad dar sugrįšite į Fluxer!

- Fluxer komanda`,
	},
	harvestCompleted: {
		subject: 'Jūsų Fluxer duomenų eksportas paruoštas',
		body: `Sveiki, {username},

Jūsų duomenų eksportas baigtas ir jau paruoštas atsisiųsti!

Eksporto suvestinė:
- Žinučių skaičius: {totalMessages, number}
- Failo dydis: {fileSizeMB} MB
- Format: ZIP archyvas su JSON failais

Atsisiųsti duomenis: {downloadUrl}

Svarbu: ši nuoroda nustos galioti {expiresAt, date, full} {expiresAt, time, short}.

Eksporte rasite:
- Visas jūsų žinutes, suskirstytas pagal kanalus
- Kanalų metaduomenis
- Paskyros ir profilio informaciją
- Gildijų narystes ir nustatymus
- Autentifikacijos sesijų duomenis

Duomenys pateikiami JSON formatu, kad būtų lengva analizuoti.

Jei turite klausimų, parašykite support@fluxer.app

- Fluxer komanda`,
	},
	unbanNotification: {
		subject: 'Jūsų Fluxer paskyros suspendavimas panaikintas',
		body: `Sveiki, {username},

Geros naujienos! Jūsų Fluxer paskyros suspendavimas panaikintas.

Priežastis: {reason}

Dabar galite vėl prisijungti prie savo paskyros ir naudotis Fluxer.

- Fluxer saugos komanda`,
	},
	scheduledDeletionNotification: {
		subject: 'Jūsų Fluxer paskyra numatyta ištrinti',
		body: `Sveiki, {username},

Jūsų Fluxer paskyra numatyta nuolatiniam ištrynimui.

Numatoma ištrynimo data: {deletionDate, date, full} {deletionDate, time, short}
Priežastis: {reason}

Tai rimta priemonė. Jūsų duomenys bus ištrinti nurodytu metu.

Jei manote, kad sprendimas neteisingas, galite pateikti apeliaciją adresu appeals@fluxer.app.

- Fluxer saugos komanda`,
	},
	giftChargebackNotification: {
		subject: 'Jūsų Fluxer Premium dovana buvo atšaukta',
		body: `Sveiki, {username},

Informuojame, kad jūsų aktyvuota Fluxer Premium dovana buvo panaikinta dėl mokėjimo ginčo (chargeback), kurį pradėjo pirminis mokėtojas.

Jūsų Premium funkcijos buvo pašalintos iš paskyros.

Jei turite klausimų, rašykite support@fluxer.app

- Fluxer komanda`,
	},
	reportResolved: {
		subject: 'Jūsų Fluxer pranešimas peržiūrėtas',
		body: `Sveiki, {username},

Jūsų pranešimas (ID: {reportId}) buvo peržiūrėtas mūsų saugos komandos.

Saugos komandos atsakymas:
{publicComment}

Ačiū, kad padedate išlaikyti Fluxer saugią bendruomenę.

Jei turite klausimų ar nuogąstavimų, rašykite safety@fluxer.app

- Fluxer saugos komanda`,
	},
	dsaReportVerification: {
		subject: 'Patvirtinkite savo el. paštą DSA pranešimui',
		body: `Sveiki,

Naudokite šį patvirtinimo kodą pateikti Skaitmeninių paslaugų akto pranešimui Fluxer platformoje:

{code}

Šis kodas nustos galioti {expiresAt, date, full} {expiresAt, time, short}.

Jei neprašėte šio patvirtinimo, tiesiog ignoruokite šį laišką.

- Fluxer saugos komanda`,
	},
	registrationApproved: {
		subject: 'Jūsų registracija Fluxer patvirtinta',
		body: `Sveiki, {username},

Puiki žinia! Jūsų registracija Fluxer patvirtinta.

Dabar galite prisijungti prie Fluxer programėlės naudodami šią nuorodą:
{channelsUrl}

Sveiki prisijungę prie Fluxer bendruomenės!

- Fluxer komanda`,
	},
	emailChangeRevert: {
		subject: 'Jūsų Fluxer el. paštas buvo pakeistas',
		body: `Sveiki, {username},

Jūsų Fluxer paskyros el. paštas pakeistas į {newEmail}.

Jei pakeitimą atlikote jūs, jokių veiksmų nereikia. Jei ne, galite jį atšaukti ir apsaugoti paskyrą naudodami šią nuorodą:

{revertUrl}

Tai atstatys ankstesnį el. paštą, atjungs jus iš visų sesijų, pašalins susietus telefono numerius, išjungs MFA ir pareikalaus naujo slaptažodžio.

- Fluxer saugumo komanda`,
	},
};
