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

export const fi: EmailTranslations = {
	passwordReset: {
		subject: 'Nollaa Fluxer-salasanasi',
		body: `Hei {username},

Olet pyytänyt Fluxer-tilisi salasanan palauttamista. Seuraa alla olevaa linkkiä asettaaksesi uuden salasanan:

{resetUrl}

Jos et pyytänyt salasanan palautusta, voit turvallisesti jättää tämän sähköpostin huomiotta.

Tämä linkki vanhenee 1 tunnin kuluttua.

- Fluxer-tiimi`,
	},
	emailVerification: {
		subject: 'Vahvista Fluxer-sähköpostiosoitteesi',
		body: `Hei {username},

Vahvista Fluxer-tilisi sähköpostiosoite napsauttamalla alla olevaa linkkiä:

{verifyUrl}

Jos et luonut Fluxer-tiliä, voit turvallisesti jättää tämän viestin huomiotta.

Tämä linkki vanhenee 24 tunnin kuluttua.

- Fluxer-tiimi`,
	},
	ipAuthorization: {
		subject: 'Hyväksy kirjautuminen uudesta IP-osoitteesta',
		body: `Hei {username},

Havaitsimme kirjautumisyrityksen Fluxer-tilillesi uudesta IP-osoitteesta:

IP-osoite: {ipAddress}
Sijainti: {location}

Jos tämä olit sinä, hyväksy tämä IP-osoite napsauttamalla alla olevaa linkkiä:

{authUrl}

Jos et yrittänyt kirjautua sisään, vaihda salasanasi välittömästi.

Tämä valtuutuslinkki vanhenee 30 minuutissa.

- Fluxer-tiimi`,
	},
	accountDisabledSuspicious: {
		subject: 'Fluxer-tilisi on tilapäisesti poistettu käytöstä',
		body: `Hei {username},

Fluxer-tilisi on poistettu tilapäisesti käytöstä epäilyttävän toiminnan vuoksi.

{reason, select,
	null {}
	other {Syy: {reason}

}}Saadaksesi tilisi takaisin käyttöösi sinun täytyy palauttaa salasanasi:

{forgotUrl}

Kun olet palauttanut salasanan, voit kirjautua sisään uudelleen.

Jos epäilet tämän tapahtuneen virheellisesti, ota yhteyttä tukitiimiimme.

- Fluxerin turvallisuustiimi`,
	},
	accountTempBanned: {
		subject: 'Fluxer-tilisi on tilapäisesti estetty',
		body: `Hei {username},

Fluxer-tilisi on tilapäisesti estetty, koska olet rikkonut käyttöehtojamme tai yhteisöohjeitamme.

Kesto: {durationHours, plural,
	=1 {1 tunti}
	other {# tuntia}
}
Estetty asti: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Syy: {reason}}
}

Tänä aikana et voi käyttää tiliäsi.

Suosittelemme tutustumaan seuraaviin:
- Käyttöehdot: {termsUrl}
- Yhteisöohjeet: {guidelinesUrl}

Jos uskot, että tämä päätös on virheellinen tai perusteeton, voit lähettää valituksen osoitteeseen appeals@fluxer.app tästä sähköpostiosoitteesta. Kerro selkeästi, miksi päätös mielestäsi oli väärä. Arvioimme valituksesi ja ilmoitamme ratkaisusta.

- Fluxerin turvallisuustiimi`,
	},
	accountScheduledDeletion: {
		subject: 'Fluxer-tilisi on aikataulutettu poistettavaksi',
		body: `Hei {username},

Fluxer-tilisi on aikataulutettu pysyvästi poistettavaksi, koska olet rikkonut käyttöehtojamme tai yhteisöohjeitamme.

Poistopäivämäärä: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Syy: {reason}}
}

Tämä on vakava toimenpide. Tilisi tiedot poistetaan pysyvästi annetun aikataulun mukaisesti.

Suosittelemme tutustumaan:
- Käyttöehdot: {termsUrl}
- Yhteisöohjeet: {guidelinesUrl}

VALITUSPROSESSI:
Jos uskot, että tämä päätös on virheellinen, sinulla on 30 päivää aikaa lähettää valitus osoitteeseen appeals@fluxer.app tästä sähköpostiosoitteesta.

Valituksessa:
- Selitä selkeästi, miksi päätös on mielestäsi väärä
- Toimita tarvittavat lisätiedot tai todisteet

Turvallisuustiimimme arvioi valituksesi ja voi keskeyttää poiston, kunnes lopullinen päätös tehdään.

- Fluxerin turvallisuustiimi`,
	},
	selfDeletionScheduled: {
		subject: 'Fluxer-tilisi poisto on aikataulutettu',
		body: `Hei {username},

Ikävä kuulla, että olet lähdössä! Fluxer-tilisi on aikataulutettu poistettavaksi.

Poistopäivämäärä: {deletionDate, date, full} {deletionDate, time, short}

TÄRKEÄÄ: Voit peruuttaa poiston milloin tahansa ennen {deletionDate, date, full} {deletionDate, time, short} kirjautumalla uudelleen sisään.

ENNEN KUIN LÄHDET:
Tietosuoja-asetuksesi käyttäjäasetuksissa sallivat sinun:
- Poistaa viestisi alustalta
- Ladata arvokasta dataa ennen lähtöä

Huomio: Kun tilisi on poistettu, et voi enää poistaa viestejäsi. Jos haluat poistaa ne, tee se ennen tilin poistoa.

Jos muutat mielesi, kirjaudu uudelleen sisään peruuttaaksesi poiston.

- Fluxer-tiimi`,
	},
	inactivityWarning: {
		subject: 'Fluxer-tilisi poistetaan toimettomuuden vuoksi',
		body: `Hei {username},

Emme ole havainneet kirjautumisia Fluxer-tilillesi yli kahteen vuoteen.

Viimeisin kirjautuminen: {lastActiveDate, date, full} {lastActiveDate, time, short}

Tietojen säilytyskäytännön mukaisesti toimettomat tilit aikataulutetaan poistettaviksi automaattisesti. Tilisi poistetaan pysyvästi:

Poistopäivämäärä: {deletionDate, date, full} {deletionDate, time, short}

NÄIN SÄILYTÄT TILISI:
Kirjaudu sisään osoitteessa {loginUrl} ennen poistopäivää. Tämä riittää – muita toimenpiteitä ei tarvita.

JOS ET KIRJAUDU SISÄÄN:
- Tilisi ja kaikki siihen liittyvät tiedot poistetaan pysyvästi
- Viestisi anonymisoidaan (“Poistettu käyttäjä”)
- Toimintoa ei voi perua

HALUATKO POISTAA VIESTEJÄSI?
Jos haluat poistaa viestit ennen tilisi poistoa, kirjaudu sisään ja käytä Tietosuoja-paneelia.

Toivottavasti näemme sinut vielä Fluxerissa!

- Fluxer-tiimi`,
	},
	harvestCompleted: {
		subject: 'Fluxer-datan vienti on valmis',
		body: `Hei {username},

Datan vientisi on valmis ja ladattavissa!

Yhteenveto:
- Viestejä yhteensä: {totalMessages, number}
- Tiedoston koko: {fileSizeMB} Mt
- Muoto: ZIP-arkisto, joka sisältää JSON-tiedostoja

Lataa datasi: {downloadUrl}

TÄRKEÄÄ: Tämä latauslinkki vanhenee {expiresAt, date, full} {expiresAt, time, short}

Vienti sisältää:
- Kaikki viestisi kanavittain järjestettynä
- Kanavien metadata
- Käyttäjäprofiilisi ja tilitietosi
- Guild-jäsenyydet ja asetukset
- Autentikaatiosessiot ja turvallisuustiedot

Data on JSON-muodossa helppoa käsittelyä varten.

Kysyttävää? Ota yhteyttä: support@fluxer.app

- Fluxer-tiimi`,
	},
	unbanNotification: {
		subject: 'Fluxer-tunnuksesi porttikielto on poistettu',
		body: `Hei {username},

Hyviä uutisia! Fluxer-tilisi porttikielto on poistettu.

Syy: {reason}

Voit nyt kirjautua takaisin sisään ja jatkaa Fluxerin käyttöä.

- Fluxerin turvallisuustiimi`,
	},
	scheduledDeletionNotification: {
		subject: 'Fluxer-tilisi on aikataulutettu poistettavaksi',
		body: `Hei {username},

Fluxer-tilisi on aikataulutettu pysyvästi poistettavaksi.

Poistopäivämäärä: {deletionDate, date, full} {deletionDate, time, short}
Syy: {reason}

Tämä on vakava toimenpide. Tilisi tiedot poistetaan pysyvästi annetun aikataulun mukaan.

Jos uskot päätöksen olleen virheellinen, voit lähettää valituksen osoitteeseen appeals@fluxer.app tästä sähköpostista.

- Fluxerin turvallisuustiimi`,
	},
	giftChargebackNotification: {
		subject: 'Fluxer Premium -lahjasi on peruttu',
		body: `Hei {username},

Haluamme ilmoittaa, että Fluxer Premium -lahja, jonka lunastit, on peruttu maksukiistan (chargeback) vuoksi, jonka alkuperäinen ostaja teki.

Premium-edut on poistettu tililtäsi. Tämä tapahtui, koska lahjan maksu peruutettiin.

Jos sinulla on kysymyksiä, ota yhteyttä: support@fluxer.app.

- Fluxer-tiimi`,
	},
	reportResolved: {
		subject: 'Fluxer-ilmoituksesi on käsitelty',
		body: `Hei {username},

Ilmoituksesi (ID: {reportId}) on käsitelty turvallisuustiimimme toimesta.

Turvallisuustiimin vastaus:
{publicComment}

Kiitos, että autat pitämään Fluxerin turvallisena kaikille. Arvostamme panostasi yhteisöömme.

Jos sinulla on kysymyksiä tai huolia tästä päätöksestä, ota yhteyttä: safety@fluxer.app.

- Fluxerin turvallisuustiimi`,
	},
	dsaReportVerification: {
		subject: 'Vahvista sähköpostisi DSA-ilmoitusta varten',
		body: `Hei,

Käytä seuraavaa vahvistuskoodia lähettääksesi digitaalisten palveluiden lain (DSA) ilmoituksen Fluxerissa:

{code}

Tämä koodi vanhenee {expiresAt, date, full} {expiresAt, time, short}.

Jos et pyytänyt tätä, voit jättää tämän viestin huomiotta.

- Fluxerin turvallisuustiimi`,
	},
	registrationApproved: {
		subject: 'Fluxer-rekisteröintisi on hyväksytty',
		body: `Hei {username},

Hienoja uutisia! Fluxer-rekisteröintisi on hyväksytty.

Pääset kirjautumaan Fluxer-sovellukseen osoitteessa:
{channelsUrl}

Tervetuloa Fluxer-yhteisöön!

- Fluxer-tiimi`,
	},
	emailChangeRevert: {
		subject: 'Fluxer-sähköpostisi on muutettu',
		body: `Hei {username},

Fluxer-tilisi sähköpostiosoite on muutettu osoitteeseen {newEmail}.

Jos teit muutoksen itse, mitään ei tarvitse tehdä. Ellet tehnyt, voit perua muutoksen ja suojata tilisi tämän linkin kautta:

{revertUrl}

Tämä palauttaa aiemman sähköpostin, kirjaa sinut ulos kaikkialta, poistaa liitetyt puhelinnumerot, poistaa MFA:n käytöstä ja edellyttää uutta salasanaa.

- Fluxer-turvatiimi`,
	},
};
