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

export const hr: EmailTranslations = {
	passwordReset: {
		subject: 'Resetirajte svoju Fluxer lozinku',
		body: `Pozdrav {username},

Zatražili ste resetiranje lozinke za svoj Fluxer račun. Slijedite poveznicu u nastavku kako biste postavili novu lozinku:

{resetUrl}

Ako niste zatražili resetiranje lozinke, možete zanemariti ovu poruku.

Ova poveznica istječe za 1 sat.

- Fluxer tim`,
	},
	emailVerification: {
		subject: 'Potvrdite svoju Fluxer adresu e-pošte',
		body: `Pozdrav {username},

Molimo vas da potvrdite adresu e-pošte svog Fluxer računa klikom na poveznicu u nastavku:

{verifyUrl}

Ako niste izradili Fluxer račun, možete zanemariti ovu poruku.

Ova poveznica istječe za 24 sata.

- Fluxer tim`,
	},
	ipAuthorization: {
		subject: 'Autorizirajte prijavu s nove IP adrese',
		body: `Pozdrav {username},

Otkrili smo pokušaj prijave na vaš Fluxer račun s nove IP adrese:

IP adresa: {ipAddress}
Lokacija: {location}

Ako ste to bili vi, molimo vas da autorizirate ovu IP adresu klikom na poveznicu u nastavku:

{authUrl}

Ako se niste pokušali prijaviti, odmah promijenite svoju lozinku.

Ova autorizacijska poveznica istječe za 30 minuta.

- Fluxer tim`,
	},
	accountDisabledSuspicious: {
		subject: 'Vaš Fluxer račun je privremeno onemogućen',
		body: `Pozdrav {username},

Vaš Fluxer račun privremeno je onemogućen zbog sumnjive aktivnosti.

{reason, select,
	null {}
	other {Razlog: {reason}

}}Kako biste ponovno dobili pristup svom računu, morate resetirati lozinku:

{forgotUrl}

Nakon resetiranja lozinke moći ćete se ponovno prijaviti.

Ako smatrate da je ovo pogreška, molimo kontaktirajte naš tim podrške.

- Fluxer sigurnosni tim`,
	},
	accountTempBanned: {
		subject: 'Vaš Fluxer račun je privremeno suspendiran',
		body: `Pozdrav {username},

Vaš Fluxer račun privremeno je suspendiran zbog kršenja naših Uvjeta korištenja ili Smjernica zajednice.

Trajanje: {durationHours, plural,
	=1 {1 sat}
	other {# sati}
}
Suspendirano do: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Razlog: {reason}}
}

Tijekom ove suspenzije nećete moći pristupiti svom računu.

Preporučujemo da pregledate:
- Uvjete korištenja: {termsUrl}
- Smjernice zajednice: {guidelinesUrl}

Ako smatrate da je ova odluka pogrešna ili neopravdana, možete poslati žalbu na appeals@fluxer.app s ove e-mail adrese. Jasno objasnite zašto smatrate da je odluka pogrešna. Razmotrit ćemo vašu žalbu i poslati našu odluku.

- Fluxer sigurnosni tim`,
	},
	accountScheduledDeletion: {
		subject: 'Vaš Fluxer račun je zakazan za brisanje',
		body: `Pozdrav {username},

Vaš Fluxer račun zakazan je za trajno brisanje zbog kršenja naših Uvjeta korištenja ili Smjernica zajednice.

Zakazani datum brisanja: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Razlog: {reason}}
}

Ovo je ozbiljna mjera provedbe. Podaci vašeg računa bit će trajno izbrisani na zakazani datum.

Preporučujemo da pregledate:
- Uvjete korištenja: {termsUrl}
- Smjernice zajednice: {guidelinesUrl}

POSTUPAK ŽALBE:
Ako smatrate da je odluka pogrešna ili neopravdana, imate 30 dana da pošaljete žalbu na appeals@fluxer.app s ove e-mail adrese.

U žalbi:
- Jasno objasnite zašto smatrate da je odluka pogrešna ili neopravdana
- Pružite sve relevantne dokaze ili dodatni kontekst

Član Fluxer sigurnosnog tima pregledat će vašu žalbu i može odgoditi brisanje dok se ne donese konačna odluka.

- Fluxer sigurnosni tim`,
	},
	selfDeletionScheduled: {
		subject: 'Brisanje vašeg Fluxer računa je zakazano',
		body: `Pozdrav {username},

Žao nam je što odlazite! Brisanje vašeg Fluxer računa je zakazano.

Zakazani datum brisanja: {deletionDate, date, full} {deletionDate, time, short}

VAŽNO: Brisanje možete otkazati u bilo kojem trenutku prije {deletionDate, date, full} {deletionDate, time, short} jednostavnom prijavom u svoj račun.

PRIJE NEGO ODETE:
Nadzorna ploča privatnosti u korisničkim postavkama omogućuje vam da:
- Obrišete svoje poruke na platformi
- Izvezete važne podatke prije odlaska

Napomena: Kada se račun izbriše, nećete moći obrisati poruke. Ako želite obrisati svoje poruke, učinite to prije završetka brisanja računa.

Ako promijenite mišljenje, jednostavno se ponovno prijavite kako biste otkazali brisanje.

- Fluxer tim`,
	},
	inactivityWarning: {
		subject: 'Vaš Fluxer račun bit će izbrisan zbog neaktivnosti',
		body: `Pozdrav {username},

Primijetili smo da se niste prijavili u svoj Fluxer račun više od 2 godine.

Zadnja prijava: {lastActiveDate, date, full} {lastActiveDate, time, short}

U sklopu naše politike zadržavanja podataka, neaktivni računi automatski se zakazuju za brisanje. Vaš račun će biti trajno izbrisan:

Zakazani datum brisanja: {deletionDate, date, full} {deletionDate, time, short}

KAKO SAČUVATI SVOJ RAČUN:
Dovoljno je da se prijavite prije zakazanog datuma brisanja putem {loginUrl}. Nije potrebna dodatna radnja.

AKO SE NE PRIJAVITE:
- Vaš račun i svi povezani podaci bit će trajno izbrisani
- Vaše poruke bit će anonimizirane (prikazane kao „Izbrisani korisnik“)
- Ova radnja je nepovratna

ŽELITE OBRISATI SVOJE PORUKE?
Ako želite obrisati poruke prije nego što se račun izbriše, prijavite se i koristite nadzornu ploču privatnosti.

Nadamo se da ćemo vas ponovno vidjeti na Fluxeru!

- Fluxer tim`,
	},
	harvestCompleted: {
		subject: 'Vaš izvoz Fluxer podataka je spreman',
		body: `Pozdrav {username},

Vaš izvoz podataka je dovršen i spreman za preuzimanje!

Sažetak izvoza:
- Ukupan broj poruka: {totalMessages, number}
- Veličina datoteke: {fileSizeMB} MB
- Format: ZIP arhiva s JSON datotekama

Preuzmite svoje podatke: {downloadUrl}

VAŽNO: Ova poveznica za preuzimanje istječe {expiresAt, date, full} {expiresAt, time, short}

U izvozu se nalazi:
- Sve vaše poruke organizirane po kanalima
- Metapodaci kanala
- Vaš korisnički profil i informacije o računu
- Članstva u guildovima i postavke
- Autentifikacijske sesije i sigurnosni podaci

Podaci su organizirani u JSON formatu radi lakše analize.

Ako imate pitanja o izvozu podataka, kontaktirajte support@fluxer.app

- Fluxer tim`,
	},
	unbanNotification: {
		subject: 'Suspencija vašeg Fluxer računa je ukinuta',
		body: `Pozdrav {username},

Dobre vijesti! Suspencija vašeg Fluxer računa je ukinuta.

Razlog: {reason}

Sada se možete ponovno prijaviti i nastaviti koristiti Fluxer.

- Fluxer sigurnosni tim`,
	},
	scheduledDeletionNotification: {
		subject: 'Vaš Fluxer račun je zakazan za brisanje',
		body: `Pozdrav {username},

Vaš Fluxer račun zakazan je za trajno brisanje.

Zakazani datum brisanja: {deletionDate, date, full} {deletionDate, time, short}
Razlog: {reason}

Ovo je ozbiljna mjera provedbe. Vaši podaci bit će trajno izbrisani na navedeni datum.

Ako mislite da je odluka pogrešna, možete poslati žalbu na appeals@fluxer.app.

- Fluxer sigurnosni tim`,
	},
	giftChargebackNotification: {
		subject: 'Vaš Fluxer Premium dar je opozvan',
		body: `Pozdrav {username},

Obavještavamo vas da je vaš Fluxer Premium dar opozvan zbog povrata uplate (chargeback) koji je podnio izvorni kupac.

Premium pogodnosti su uklonjene s vašeg računa. Ovo je učinjeno jer je uplata za dar osporena i vraćena.

Ako imate pitanja, kontaktirajte support@fluxer.app.

- Fluxer tim`,
	},
	reportResolved: {
		subject: 'Vaša Fluxer prijava je pregledana',
		body: `Pozdrav {username},

Vaša prijava (ID: {reportId}) pregledana je od strane našeg sigurnosnog tima.

Odgovor sigurnosnog tima:
{publicComment}

Hvala vam što pomažete održavati Fluxer sigurnim za sve. Ozbiljno shvaćamo sve prijave i cijenimo vaš doprinos zajednici.

Ako imate pitanja ili brige u vezi ove odluke, kontaktirajte safety@fluxer.app.

- Fluxer sigurnosni tim`,
	},
	dsaReportVerification: {
		subject: 'Potvrdite svoju e-poštu za DSA prijavu',
		body: `Pozdrav,

Koristite sljedeći kod za potvrdu kako biste podnijeli prijavu prema Zakonu o digitalnim uslugama na Fluxer:

{code}

Ovaj kod istječe {expiresAt, date, full} {expiresAt, time, short}.

Ako niste zatražili ovo, možete zanemariti ovu poruku.

- Fluxer sigurnosni tim`,
	},
	registrationApproved: {
		subject: 'Vaša Fluxer registracija je odobrena',
		body: `Pozdrav {username},

Sjajne vijesti! Vaša Fluxer registracija je odobrena.

Sada se možete prijaviti u Fluxer aplikaciju ovdje:
{channelsUrl}

Dobro došli u Fluxer zajednicu!

- Fluxer tim`,
	},
	emailChangeRevert: {
		subject: 'Tvoja Fluxer e-pošta je promijenjena',
		body: `Bok {username},

E-pošta tvog Fluxer računa promijenjena je u {newEmail}.

Ako si ti napravio/la ovu promjenu, ne moraš ništa dalje raditi. Ako nisi, možeš je poništiti i osigurati račun putem ove poveznice:

{revertUrl}

Time će se vratiti prijašnja e-pošta, bit ćeš odjavljen/a svugdje, uklonit će se povezani telefonski brojevi, MFA će biti onemogućen i tražit će se nova lozinka.

- Fluxer sigurnosni tim`,
	},
};
