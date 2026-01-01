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

export const ro: EmailTranslations = {
	passwordReset: {
		subject: 'Resetează-ți parola Fluxer',
		body: `Salut {username},

Ai solicitat resetarea parolei pentru contul tău Fluxer. Te rugăm să urmezi linkul de mai jos pentru a seta o parolă nouă:

{resetUrl}

Dacă nu ai cerut această resetare, poți ignora acest email în siguranță.

Acest link va expira în 1 oră.

- Echipa Fluxer`,
	},
	emailVerification: {
		subject: 'Verifică-ți adresa de email Fluxer',
		body: `Salut {username},

Te rugăm să îți verifici adresa de email pentru contul Fluxer accesând linkul de mai jos:

{verifyUrl}

Dacă nu ai creat un cont Fluxer, poți ignora acest email.

Acest link va expira în 24 de ore.

- Echipa Fluxer`,
	},
	ipAuthorization: {
		subject: 'Autorizează conectarea de pe o adresă IP nouă',
		body: `Salut {username},

Am detectat o încercare de conectare la contul tău Fluxer de pe o adresă IP nouă:

Adresă IP: {ipAddress}
Locație: {location}

Dacă tu ai fost, te rugăm să autorizezi această adresă IP accesând linkul de mai jos:

{authUrl}

Dacă nu ai încercat să te conectezi, schimbă imediat parola contului tău.

Acest link de autorizare va expira în 30 de minute.

- Echipa Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Contul tău Fluxer a fost dezactivat temporar',
		body: `Salut {username},

Contul tău Fluxer a fost dezactivat temporar din cauza activității suspecte.

{reason, select,
	null {}
	other {Motiv: {reason}

}}Pentru a recăpăta accesul, trebuie să îți resetezi parola:

{forgotUrl}

După resetarea parolei, vei putea să te conectezi din nou.

Dacă crezi că aceasta este o greșeală, te rugăm să contactezi echipa noastră de suport.

- Echipa de Securitate Fluxer`,
	},
	accountTempBanned: {
		subject: 'Contul tău Fluxer a fost suspendat temporar',
		body: `Salut {username},

Contul tău Fluxer a fost suspendat temporar pentru încălcarea Termenilor de Utilizare sau a Ghidurilor Comunității.

Durată: {durationHours, plural,
	=1 {1 oră}
	other {# ore}
}
Suspendat până la: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {Motiv: {reason}}
}

Pe durata suspendării nu vei avea acces la contul tău.

Îți recomandăm să consulți:
- Termenii de Utilizare: {termsUrl}
- Ghidurile Comunității: {guidelinesUrl}

Dacă consideri că această decizie este greșită sau nedreaptă, poți trimite o contestație la appeals@fluxer.app de pe această adresă de email.  
Te rugăm să explici clar de ce consideri că decizia este incorectă. Vom analiza contestația ta și îți vom comunica rezultatul.

- Echipa de Securitate Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Contul tău Fluxer este programat pentru ștergere',
		body: `Salut {username},

Contul tău Fluxer a fost programat pentru ștergere permanentă din cauza încălcării Termenilor de Utilizare sau a Ghidurilor Comunității.

Data programată pentru ștergere: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {Motiv: {reason}}
}

Aceasta este o măsură serioasă. Toate datele contului vor fi șterse definitiv la data programată.

Îți recomandăm să consulți:
- Termenii de Utilizare: {termsUrl}
- Ghidurile Comunității: {guidelinesUrl}

PROCES DE CONTESTAȚIE:
Dacă consideri că această decizie este greșită sau nedreaptă, ai la dispoziție 30 de zile pentru a trimite o contestație la appeals@fluxer.app de pe această adresă de email.

În contestație:
- Explică clar de ce consideri decizia incorectă sau nedreaptă
- Oferă orice dovezi sau context suplimentar relevant

Un membru al Echipei de Securitate Fluxer va analiza contestația și poate suspenda ștergerea până la o decizie finală.

- Echipa de Securitate Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'Ștergerea contului tău Fluxer a fost programată',
		body: `Salut {username},

Ne pare rău să te vedem plecând! Ștergerea contului tău Fluxer a fost programată.

Data programată pentru ștergere: {deletionDate, date, full} {deletionDate, time, short}

IMPORTANT: Poți anula această ștergere în orice moment înainte de {deletionDate, date, full} {deletionDate, time, short} prin simpla reconectare la cont.

ÎNAINTE DE A PLECA:
Panoul de Confidențialitate din Setările Utilizatorului îți permite să:
- Ștergi mesajele tale din platformă
- Exporezi date utile înainte de plecare

Notă: După ștergerea contului, nu vei mai putea șterge mesajele. Dacă dorești să le ștergi, te rugăm să faci acest lucru înainte de finalizarea ștergerii contului.

Dacă te răzgândești, reconectează-te pentru a anula ștergerea.

- Echipa Fluxer`,
	},
	inactivityWarning: {
		subject: 'Contul tău Fluxer va fi șters din cauza inactivității',
		body: `Salut {username},

Am observat că nu te-ai conectat la contul tău Fluxer de peste 2 ani.

Ultima conectare: {lastActiveDate, date, full} {lastActiveDate, time, short}

Conform politicii noastre de păstrare a datelor, conturile inactive sunt programate automat pentru ștergere.

Data programată pentru ștergere: {deletionDate, date, full} {deletionDate, time, short}

CUM SĂ ÎȚI PĂSTREZI CONTUL:
Trebuie doar să te conectezi la contul tău la {loginUrl} înainte de data ștergerii. Nu este necesară nicio altă acțiune.

DACĂ NU TE CONECTEZI:
- Contul și toate datele tale vor fi șterse definitiv
- Mesajele tale vor fi anonimizate („Utilizator Șters”)
- Această acțiune nu poate fi anulată

VREI SĂ ȘTERGI MESAJELE TALE?
Te rugăm să te conectezi și să folosești Panoul de Confidențialitate înainte de ștergerea contului.

Sperăm să te revedem pe Fluxer!

- Echipa Fluxer`,
	},
	harvestCompleted: {
		subject: 'Exportul tău de date Fluxer este gata',
		body: `Salut {username},

Exportul datelor tale Fluxer a fost finalizat și este gata pentru descărcare!

Rezumatul exportului:
- Număr total de mesaje: {totalMessages, number}
- Dimensiunea fișierului: {fileSizeMB} MB
- Format: Arhivă ZIP cu fișiere JSON

Descarcă datele tale aici: {downloadUrl}

IMPORTANT: Acest link de descărcare va expira la {expiresAt, date, full} {expiresAt, time, short}

Exportul include:
- Toate mesajele tale organizate pe canale
- Metadate ale canalelor
- Profilul utilizatorului și informațiile contului
- Apartenența la guild-uri și setările
- Sesiuni de autentificare și informații de securitate

Datele sunt livrate în format JSON pentru a facilita analiza.

Dacă ai întrebări, ne poți contacta la support@fluxer.app

- Echipa Fluxer`,
	},
	unbanNotification: {
		subject: 'Suspendarea contului tău Fluxer a fost ridicată',
		body: `Salut {username},

Vești bune! Suspendarea contului tău Fluxer a fost ridicată.

Motiv: {reason}

Acum poți să te conectezi din nou și să folosești Fluxer.

- Echipa de Securitate Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Contul tău Fluxer este programat pentru ștergere',
		body: `Salut {username},

Contul tău Fluxer a fost programat pentru ștergere permanentă.

Data ștergerii: {deletionDate, date, full} {deletionDate, time, short}
Motiv: {reason}

Aceasta este o acțiune serioasă. Toate datele tale vor fi șterse la data programată.

Dacă consideri că decizia este incorectă, poți trimite o contestație la appeals@fluxer.app

- Echipa de Securitate Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Cadoul tău Fluxer Premium a fost revocat',
		body: `Salut {username},

Îți aducem la cunoștință că darul tău Fluxer Premium a fost revocat din cauza unui litigiu de plată (chargeback) inițiat de cumpărătorul original.

Beneficiile premium au fost eliminate din contul tău.

Dacă ai întrebări, ne poți contacta la support@fluxer.app

- Echipa Fluxer`,
	},
	reportResolved: {
		subject: 'Raportul tău către Fluxer a fost analizat',
		body: `Salut {username},

Raportul tău (ID: {reportId}) a fost analizat de către Echipa de Securitate Fluxer.

Răspunsul Echipei de Securitate:
{publicComment}

Îți mulțumim că ajuți la menținerea siguranței pe Fluxer. Apreciem contribuția ta la comunitate.

Dacă ai nelămuriri sau întrebări, ne poți contacta la safety@fluxer.app

- Echipa de Securitate Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Verifică-ți e-mailul pentru un raport DSA',
		body: `Salut,

Folosește următorul cod de verificare pentru a trimite raportul tău conform Legii serviciilor digitale pe Fluxer:

{code}

Acest cod expiră la {expiresAt, date, full} {expiresAt, time, short}.

Dacă nu ai solicitat aceasta, te rugăm să ignori acest e-mail.

- Echipa de Securitate Fluxer`,
	},
	registrationApproved: {
		subject: 'Înregistrarea ta pe Fluxer a fost aprobată',
		body: `Salut {username},

Felicitări! Înregistrarea ta pe Fluxer a fost aprobată.

Poți acum să te conectezi la aplicația Fluxer aici:
{channelsUrl}

Bine ai venit în comunitatea Fluxer!

- Echipa Fluxer`,
	},
	emailChangeRevert: {
		subject: 'E-mailul tău Fluxer a fost modificat',
		body: `Bună {username},

E-mailul contului tău Fluxer a fost schimbat în {newEmail}.

Dacă tu ai făcut schimbarea, nu este nevoie de alte acțiuni. Dacă nu, o poți anula și îți poți securiza contul cu acest link:

{revertUrl}

Aceasta va restaura e-mailul anterior, te va deconecta de peste tot, va elimina numerele de telefon asociate, va dezactiva MFA și va necesita o parolă nouă.

- Echipa de Securitate Fluxer`,
	},
};
