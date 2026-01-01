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

export const hu: EmailTranslations = {
	passwordReset: {
		subject: 'Állítsd vissza a Fluxer-jelszavad',
		body: `Szia {username},

Jelszó-visszaállítást kértél a Fluxer-fiókodhoz. Kérjük, kövesd az alábbi hivatkozást, hogy új jelszót állíts be:

{resetUrl}

Ha nem te kérted a jelszó visszaállítását, egyszerűen hagyd figyelmen kívül ezt az e-mailt.

A hivatkozás 1 órán belül lejár.

- A Fluxer csapata`,
	},
	emailVerification: {
		subject: 'Erősítsd meg a Fluxer e-mail címed',
		body: `Szia {username},

Kérjük, erősítsd meg a Fluxer-fiókodhoz tartozó e-mail címed az alábbi hivatkozásra kattintva:

{verifyUrl}

Ha nem te hoztál létre Fluxer-fiókot, figyelmen kívül hagyhatod ezt az e-mailt.

A hivatkozás 24 órán belül lejár.

- A Fluxer csapata`,
	},
	ipAuthorization: {
		subject: 'Engedélyezd a bejelentkezést új IP-címről',
		body: `Szia {username},

Új IP-címről történő bejelentkezési kísérletet észleltünk a Fluxer-fiókodban:

IP-cím: {ipAddress}
Hely: {location}

Ha te próbáltál bejelentkezni, kattints az alábbi hivatkozásra az IP-cím engedélyezéséhez:

{authUrl}

Ha nem te voltál, azonnal változtasd meg a jelszavad.

Ez az engedélyezési hivatkozás 30 perc múlva lejár.

- A Fluxer csapata`,
	},
	accountDisabledSuspicious: {
		subject: 'Fluxer-fiókod ideiglenesen letiltásra került',
		body: `Szia {username},

Fluxer-fiókodat ideiglenesen letiltottuk gyanús aktivitás miatt.

{reason, select,
	null {}
	other {Indoklás: {reason}

}}A hozzáférés visszanyeréséhez vissza kell állítanod a jelszavad:

{forgotUrl}

A jelszó visszaállítása után ismét be tudsz jelentkezni.

Ha úgy gondolod, hogy a letiltás tévedés volt, kérjük, keresd fel ügyfélszolgálatunkat.

- A Fluxer biztonsági csapata`,
	},
	accountTempBanned: {
		subject: 'Fluxer-fiókod ideiglenesen fel lett függesztve',
		body: `Szia {username},

Fluxer-fiókodat ideiglenesen felfüggesztettük a Szolgáltatási feltételek vagy a Közösségi irányelvek megsértése miatt.

Időtartam: {durationHours, plural,
	=1 {1 óra}
	other {# óra}
}
Felfüggesztés vége: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Indoklás: {reason}}
}

Ebben az időszakban nem fogsz tudni hozzáférni a fiókodhoz.

Kérjük, tekintsd át:
- Szolgáltatási feltételek: {termsUrl}
- Közösségi irányelvek: {guidelinesUrl}

Ha úgy gondolod, hogy a döntés helytelen vagy indokolatlan, küldhetsz fellebbezést az appeals@fluxer.app címre erről az e-mail címről. Kérjük, részletesen magyarázd el, miért tartod hibásnak a döntést. Áttekintjük a fellebbezést és értesítünk az eredményről.

- A Fluxer biztonsági csapata`,
	},
	accountScheduledDeletion: {
		subject: 'Fluxer-fiókod törlésre lett ütemezve',
		body: `Szia {username},

Fluxer-fiókod törlését ütemeztük, mivel megsértetted a Szolgáltatási feltételeket vagy a Közösségi irányelveket.

Tervezett törlési időpont: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Indoklás: {reason}}
}

Ez egy komoly intézkedés. A fiókhoz tartozó összes adat véglegesen törlődik a megadott időpontban.

Kérjük, tekintsd át:
- Szolgáltatási feltételek: {termsUrl}
- Közösségi irányelvek: {guidelinesUrl}

FELLEBBEZÉSI FOLYAMAT:
Ha úgy véled, hogy a döntés hibás vagy indokolatlan, 30 napod van fellebbezést küldeni az appeals@fluxer.app címre erről az e-mail címről.

A fellebbezésben:
- Részletesen írd le, miért tartod helytelennek a döntést
- Adj meg minden releváns bizonyítékot vagy kontextust

A biztonsági csapat egy tagja felülvizsgálja a fellebbezést, és akár felfüggesztheti a törlést a végső döntésig.

- A Fluxer biztonsági csapata`,
	},
	selfDeletionScheduled: {
		subject: 'Fluxer-fiókod törlése ütemezve lett',
		body: `Szia {username},

Sajnáljuk, hogy távozol! Fluxer-fiókod törlését ütemeztük.

Tervezett törlési időpont: {deletionDate, date, full} {deletionDate, time, short}

FONTOS: Bármikor leállíthatod a törlést {deletionDate, date, full} {deletionDate, time, short} előtt, ha újra bejelentkezel a fiókodba.

MIELŐTT TOVÁBBLÉPNÉL:
A felhasználói beállításoknál található adatvédelmi irányítópult lehetővé teszi:
- Üzeneteid törlését a platformról
- Fontos adatok exportálását távozás előtt

Kérjük, vedd figyelembe: a fiók törlése után nem lehet visszamenőleg törölni az üzeneteket. Ha törölni szeretnéd őket, tedd meg a törlés véglegesítése előtt.

Ha meggondolod magad, csak jelentkezz be újra a törlés megszakításához.

- A Fluxer csapata`,
	},
	inactivityWarning: {
		subject: 'Fluxer-fiókod inaktivitás miatt törlésre kerül',
		body: `Szia {username},

Úgy tűnik, több mint 2 éve nem jelentkeztél be a Fluxer-fiókodba.

Utolsó bejelentkezés: {lastActiveDate, date, full} {lastActiveDate, time, short}

Adatmegőrzési irányelveink részeként az inaktív fiókok automatikusan törlésre ütemeződnek. A fiókodat véglegesen töröljük:

Tervezett törlési időpont: {deletionDate, date, full} {deletionDate, time, short}

HOGYAN TARTHATOD MEG A FIÓKODAT:
Egyszerűen jelentkezz be a {loginUrl} címen a törlési időpont előtt. Semmi mást nem kell tenned.

HA NEM JELENTKEZEL BE:
- A fiókod és minden kapcsolódó adat véglegesen törlődik
- Üzeneteid anonimizálva lesznek („Törölt felhasználó” megjelöléssel)
- Ez a művelet nem visszafordítható

SZERETNÉD TÖRÖLNI AZ ÜZENETEIDET?
Jelentkezz be, és használd az adatvédelmi irányítópultot a fiók törlése előtt.

Reméljük, visszatérsz a Fluxerre!

- A Fluxer csapata`,
	},
	harvestCompleted: {
		subject: 'A Fluxer adat-exportod elkészült',
		body: `Szia {username},

Az adataid exportálása sikeresen befejeződött, és most már letölthető!

Export összegzés:
- Üzenetek száma összesen: {totalMessages, number}
- Fájlméret: {fileSizeMB} MB
- Formátum: ZIP archívum JSON fájlokkal

Adataid letöltése: {downloadUrl}

FONTOS: A letöltési hivatkozás lejár ekkor: {expiresAt, date, full} {expiresAt, time, short}

Az export tartalmazza:
- Minden üzeneted, csatornánként rendezve
- Csatorna-metaadatok
- Felhasználói profilod és fiókinformációk
- Guild-tagságok és beállítások
- Hitelesítési munkamenetek és biztonsági információk

Az adatok JSON formátumban érkeznek, így könnyen feldolgozhatók.

Kérdés esetén írj a support@fluxer.app címre.

- A Fluxer csapata`,
	},
	unbanNotification: {
		subject: 'Fluxer-fiókod felfüggesztése megszűnt',
		body: `Szia {username},

Jó hír! Fluxer-fiókod felfüggesztését feloldottuk.

Indoklás: {reason}

Most ismét bejelentkezhetsz, és használhatod a Fluxert.

- A Fluxer biztonsági csapata`,
	},
	scheduledDeletionNotification: {
		subject: 'Fluxer-fiókod törlésre ütemezve',
		body: `Szia {username},

Fluxer-fiókod törlése véglegesen ütemezve lett.

Törlési időpont: {deletionDate, date, full} {deletionDate, time, short}
Indoklás: {reason}

Ez egy komoly lépés. A fiókod adatai véglegesen törlésre kerülnek.

Ha úgy gondolod, hogy a döntés helytelen, írj az appeals@fluxer.app címre.

- A Fluxer biztonsági csapata`,
	},
	giftChargebackNotification: {
		subject: 'A Fluxer Premium ajándékod visszavonásra került',
		body: `Szia {username},

Azért írunk, hogy tájékoztassunk: a Fluxer Premium ajándékot, amelyet beváltottál, visszavontuk egy fizetési vita (chargeback) miatt, amelyet az eredeti vásárló nyújtott be.

A Premium előnyöket eltávolítottuk a fiókodból. Ez azért történt, mert a fizetés visszafordításra került.

Ha kérdésed van, írj a support@fluxer.app címre.

- A Fluxer csapata`,
	},
	reportResolved: {
		subject: 'A Fluxer jelentésedet felülvizsgáltuk',
		body: `Szia {username},

A jelentésedet (azonosító: {reportId}) a biztonsági csapatunk átnézte.

A biztonsági csapat válasza:
{publicComment}

Köszönjük, hogy segítesz biztonságossá tenni a Fluxert mindenki számára. Nagyra értékeljük a közösséghez való hozzájárulásodat.

Ha kérdésed vagy aggályod van a döntéssel kapcsolatban, írj a safety@fluxer.app címre.

- A Fluxer biztonsági csapata`,
	},
	dsaReportVerification: {
		subject: 'Erősítsd meg az e-mailedet a DSA jelentéshez',
		body: `Szia,

Az alábbi ellenőrző kóddal küldheted be a Digitális Szolgáltatásokról szóló törvény szerinti jelentésedet a Fluxeren:

{code}

Ez a kód {expiresAt, date, full} {expiresAt, time, short} időpontban lejár.

Ha nem te kérted ezt, figyelmen kívül hagyhatod ezt az e-mailt.

- A Fluxer biztonsági csapata`,
	},
	registrationApproved: {
		subject: 'Fluxer-regisztrációd jóváhagyva',
		body: `Szia {username},

Nagyszerű hír! A Fluxer-regisztrációd jóvá lett hagyva.

Most már bejelentkezhetsz a Fluxer alkalmazásba:
{channelsUrl}

Üdvözlünk a Fluxer közösségben!

- A Fluxer csapata`,
	},
	emailChangeRevert: {
		subject: 'Megváltozott a Fluxer e-mail címed',
		body: `Szia {username},

A Fluxer-fiókod e-mail címe {newEmail} címre változott.

Ha te módosítottad, nincs további teendő. Ha nem, az alábbi linken visszavonhatod és biztosíthatod a fiókodat:

{revertUrl}

Ez visszaállítja a korábbi e-mail címet, mindenhol kijelentkeztet, eltávolítja a társított telefonszámokat, letiltja az MFA-t, és új jelszót kér.

- Fluxer biztonsági csapat`,
	},
};
