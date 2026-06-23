// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_FI_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Käyttäjätilisi {product_name}-palvelussa on tilapäisesti poistettu käytöstä",
		"body": "Hei {username},\n\nOlemme poistaneet {product_name}-tilisi tilapäisesti käytöstä, koska havaitsimme epäilyttävää toimintaa.\n\n{reason, select,\n  null {}\n  other {Syy: {reason}}\n}\n\nPäästäksesi takaisin tilillesi sinun on nollattava salasanasi:\n\n{forgotUrl}\n\nKun olet nollannut salasanasi, voit kirjautua sisään uudelleen.\n\nJos uskot, että tämä oli virhe, ota yhteyttä tukitiimiimme.\n\n– {product_name}-turvallisuustiimi"
	},
	"account_scheduled_deletion": {
		"subject": "Käyttäjätilisi {product_name}-palvelussa poistetaan pysyvästi",
		"body": "Hei {username},\n\n{product_name}-käyttäjätilisi on ajoitettu poistettavaksi pysyvästi käyttöehtojemme tai yhteisön sääntöjemme rikkomusten vuoksi.\n\nAjoitettu poisto: {deletionDate, date, full} klo {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Syy: {reason}}\n}\n\nTämä on vakava toimenpide. Tilisi tiedot poistetaan pysyvästi ajoitettuna päivänä.\n\nTarkista:\n- Käyttöehdot: {termsUrl}\n- Yhteisön säännöt: {guidelinesUrl}\n\nValitusprosessi:\nJos uskot, että tämä päätös oli virheellinen tai perusteeton, sinulla on 60 päivää aikaa tehdä valitus. Lähetä sähköpostia osoitteeseen {appeals_email} tästä sähköpostiosoitteesta.\n\nValituksessasi:\n- Selitä selkeästi, miksi uskot päätöksen olleen virheellinen tai perusteeton\n- Anna kaikki asiaankuuluvat todisteet tai konteksti\n\n{product_name}-turvallisuustiimin jäsen tarkistaa valituksesi ja voi keskeyttää vireillä olevan poiston, kunnes lopullinen päätös on tehty.\n\n– {product_name}-turvallisuustiimi"
	},
	"account_temp_banned": {
		"subject": "Käyttäjätilisi {product_name}-palvelussa on tilapäisesti jäädytetty",
		"body": "Hei {username},\n\n{product_name}-käyttäjätilisi on jäädytetty tilapäisesti käyttöehtojemme tai yhteisön sääntöjemme rikkomisen vuoksi.\n\nKesto: {durationHours, plural,\n  =1 {1 tunti}\n  other {# tuntia}\n}\nJäädytettynä {bannedUntil, date, full} klo {bannedUntil, time, short} asti\n\n{reason, select,\n  null {}\n  other {Syy: {reason}}\n}\n\nTänä aikana et voi käyttää tiliäsi.\n\nTarkista:\n- Käyttöehdot: {termsUrl}\n- Yhteisön säännöt: {guidelinesUrl}\n\nJos uskot, että tämä päätös oli virheellinen tai perusteeton, voit tehdä valituksen. Lähetä sähköpostia osoitteeseen {appeals_email} tästä sähköpostiosoitteesta ja selitä selkeästi, miksi uskot päätöksen olleen virheellinen. Tarkistamme valituksesi ja vastaamme päätöksellämme.\n\n– {product_name}-turvallisuustiimi"
	},
	"donation_confirmation": {
		"subject": "Kiitos {product_name}-lahjoituksestasi",
		"body": "Hei,\n\nKiitos {product_name}-lahjoituksestasi! {interval, select,\n  month {Toistuva lahjoituksesi}\n  year {Toistuva lahjoituksesi}\n  other {Kertalahjoituksesi}\n} on {interval, select,\n  month {asetettu}\n  year {asetettu}\n  other {käsitelty}\n} onnistuneesti.\n\nLahjoituksen tiedot:\nSumma: {amount} {currency} {interval, select,\n  month {kuukaudessa}\n  year {vuodessa}\n  other {}\n}\n\nStripe lähettää sinulle erillisen kuitin PDF-laskun kanssa pian. Tämä sisältää kaikki maksutiedot ja sitä voidaan käyttää verotuksessa.\n\nVoit tarkastella lahjoitushistoriaasi, ladata laskuja {interval, select,\n  month {ja hallinnoida tai peruuttaa tilaustasi}\n  year {ja hallinnoida tai peruuttaa tilaustasi}\n  other {ja hallita tulevia lahjoituksia}\n} milloin tahansa käyttämällä tätä linkkiä:\n\n{manageUrl}\n\nTukesi auttaa pitämään {product_name}-palvelun toiminnassa. Kiitos!\n\n– {product_name}-tiimi"
	},
	"donation_magic_link": {
		"subject": "Hallinnoi {product_name}-palvelun lahjoituksiasi",
		"body": "Hei,\n\nNapsauta alla olevaa linkkiä päästäksesi lahjoittajaportaaliisi:\n\n{manageUrl}\n\nPortaalissa voit hallinnoida tilauksia, ladata laskuja ja tarkastella lahjoitushistoriaasi.\n\nTämä linkki vanhenee {expiresAt, date, full} klo {expiresAt, time, short}.\n\nJos et pyytänyt tätä linkkiä, voit jättää tämän sähköpostin turvallisesti huomiotta.\n\n– {product_name}-tiimi"
	},
	"dsa_report_verification": {
		"subject": "Vahvista sähköpostiosoite DSA-raporttia varten",
		"body": "Hei,\n\nKäytä alla olevaa vahvistuskoodia lähettääksesi DSA-raporttisi {product_name}-palveluun:\n\n{code}\n\nTämä koodi vanhenee {expiresAt, date, full} klo {expiresAt, time, short}.\n\nJos et pyytänyt tätä, voit jättää tämän sähköpostin huomiotta.\n\n– {product_name}-turvallisuustiimi"
	},
	"email_change_new": {
		"subject": "Vahvista uusi sähköpostiosoitteesi {product_name}-tilillä",
		"body": "Hei {username},\n\nSyötä tämä koodi sovellukseen vahvistaaksesi uuden sähköpostiosoitteesi {product_name}-tilillä:\n\n{code}\n\nTämä koodi vanhenee {expiresAt, date, full} klo {expiresAt, time, short}.\n\nJos et pyytänyt tätä, voit jättää tämän sähköpostin huomiotta.\n\n– {product_name}-tiimi"
	},
	"email_change_original": {
		"subject": "Vahvista sähköpostiosoitteen muutos {product_name}-tilillä",
		"body": "Hei {username},\n\nSaimme pyynnön muuttaa sähköpostiosoitetta {product_name}-tililläsi.\n\nVahvistaaksesi tämän muutoksen, syötä tämä koodi sovellukseen:\n\n{code}\n\nTämä koodi vanhenee {expiresAt, date, full} klo {expiresAt, time, short}.\n\nJos et pyytänyt tätä, turvaa tilisi välittömästi.\n\n– {product_name}-tiimi"
	},
	"email_change_revert": {
		"subject": "{product_name}-tilisi sähköpostiosoite on vaihdettu",
		"body": "Hei {username},\n\nSähköpostiosoitteesi {product_name}-tilillä on vaihdettu osoitteeseen {newEmail}.\n\nJos teit tämän muutoksen, toimenpiteitä ei tarvita. Jos et tehnyt, voit kumota muutoksen ja turvata tilisi käyttämällä tätä linkkiä:\n\n{revertUrl}\n\nTämä palauttaa edellisen sähköpostiosoitteesi, kirjaa sinut ulos kaikkialta, poistaa linkitetyt puhelinnumerot, poistaa MFA:n käytöstä ja edellyttää uuden salasanan asettamista.\n\n– {product_name}-turvallisuustiimi"
	},
	"email_verification": {
		"subject": "Vahvista sähköpostiosoitteesi {product_name}-tilillä",
		"body": "Hei {username},\n\nVahvista {product_name}-tilisi sähköpostiosoite napsauttamalla alla olevaa linkkiä:\n\n{verifyUrl}\n\nJos et luonut {product_name}-tiliä, voit jättää tämän sähköpostin turvallisesti huomiotta.\n\nTämä linkki on voimassa 24 tuntia.\n\n– {product_name}-tiimi"
	},
	"gift_chargeback_notification": {
		"subject": "Lunastamasi lahjan edut on poistettu",
		"body": "Hei {username},\n\nLunastamasi lahjakoodin oli alun perin maksanut joku muu. Kyseinen maksu on sittemmin peruutettu (takaisinperintä).\n\nTämän vuoksi olemme poistaneet edut, jotka lisättiin tilillesi, kun lunastit lahjan.\n\nJos uskot tämän olevan virhe, ota yhteyttä tukitiimiimme ja liitä mukaan kaikki tiedot lahjakoodista ja sen lunastusajankohdasta.\n\n– {product_name}-tiimi"
	},
	"harvest_completed": {
		"subject": "{product_name}-tietojesi vienti on ladattavissa",
		"body": "Hei {username},\n\nTietojesi vienti on valmis.\n\nLatauslinkki:\n{downloadUrl}\n\nMukana olevat viestit: {totalMessages, number}\nTiedoston koko: {fileSizeMB, number} MB\n\nTämä linkki vanhenee {expiresAt, date, full} klo {expiresAt, time, short}.\n\nJos et pyytänyt tätä vientiä, vaihda salasanasi välittömästi ja ota yhteyttä tukitiimiimme.\n\n– {product_name}-tiimi"
	},
	"inactivity_warning": {
		"subject": "{product_name}-tilisi poistetaan käyttämättömyyden vuoksi",
		"body": "Hei {username},\n\nEmme ole havainneet toimintaa {product_name}-tililläsi {lastActiveDate, date, full} jälkeen.\n\nJos et kirjaudu sisään {deletionDate, date, full} klo {deletionDate, time, short} mennessä, tilisi poistetaan pysyvästi passiivisuuden vuoksi.\n\nKirjaudu sisään täältä:\n{loginUrl}\n\nJos olet käyttänyt {product_name}-palvelua äskettäin, ota yhteyttä tukitiimiimme välittömästi.\n\n– {product_name}-tiimi"
	},
	"ip_authorization": {
		"subject": "Hyväksy kirjautuminen uudesta IP-osoitteesta",
		"body": "Hei {username},\n\nHavaitsimme kirjautumisyrityksen {product_name}-tilillesi uudesta IP-osoitteesta:\n\nIP-osoite: {ipAddress}\nSijainti: {location}\n\nJos tämä olit sinä, valtuuta tämä IP-osoite napsauttamalla alla olevaa linkkiä:\n\n{authUrl}\n\nJos et yrittänyt kirjautua sisään, vaihda salasanasi välittömästi.\n\nTämä linkki on voimassa 30 minuuttia.\n\n– {product_name}-tiimi"
	},
	"password_change_verification": {
		"subject": "Vahvista salasanan muutos {product_name}-tilillä",
		"body": "Hei {username},\n\nSaimme pyynnön muuttaa salasanaa {product_name}-tililläsi.\n\nVahvistaaksesi tämän muutoksen, syötä tämä koodi sovellukseen:\n\n{code}\n\nTämä koodi vanhenee {expiresAt}.\n\nJos et pyytänyt tätä, joku saattaa päästä tilillesi. Vaihda salasanasi välittömästi ja ota käyttöön kaksivaiheinen todennus.\n\n– {product_name}-tiimi"
	},
	"password_reset": {
		"subject": "Nollaa {product_name}-salasanasi",
		"body": "Hei {username},\n\nPyysit {product_name}-salasanan nollausta. Käytä alla olevaa linkkiä asettaaksesi uuden salasanan:\n\n{resetUrl}\n\nJos et pyytänyt tätä, voit jättää tämän sähköpostin turvallisesti huomiotta.\n\nTämä linkki on voimassa 1 tunnin.\n\n– {product_name}-tiimi"
	},
	"registration_approved": {
		"subject": "{product_name}-rekisteröitymisesi on hyväksytty",
		"body": "Hei {username},\n\nHyviä uutisia: {product_name}-rekisteröitymisesi on hyväksytty.\n\nVoit nyt kirjautua {product_name}-sovellukseen täältä:\n{channelsUrl}\n\nTervetuloa {product_name}-yhteisöön.\n\n– {product_name}-tiimi"
	},
	"report_resolved": {
		"subject": "{product_name}-raporttisi on tarkistettu",
		"body": "Hei {username},\n\nTurvallisuustiimimme on tarkistanut raporttisi (ID: {reportId}).{hasComment, select, yes {\n\nTurvallisuustiimin vastaus:\n{publicComment}} other {}}\n\nKiitos, että autat pitämään {product_name}-palvelun turvallisena kaikille. Otamme kaikki raportit vakavasti ja arvostamme panostasi yhteisöön.\n\nJos sinulla on kysyttävää tai huolenaiheita tästä tuloksesta, ota yhteyttä osoitteeseen {safety_email}.\n\n– {product_name}-turvallisuustiimi"
	},
	"scheduled_deletion_notification": {
		"subject": "Käyttäjätilisi {product_name}-palvelussa poistetaan pysyvästi",
		"body": "Hei {username},\n\n{product_name}-tilisi on ajoitettu poistettavaksi pysyvästi.\n\nAjoitettu poisto: {deletionDate, date, full} klo {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Syy: {reason}}\n}\n\nTämä on vakava toimenpide. Tilisi tiedot poistetaan pysyvästi ajoitettuna päivänä.\n\nJos uskot, että tämä päätös oli virheellinen, voit tehdä valituksen. Lähetä sähköpostia osoitteeseen {appeals_email} tästä sähköpostiosoitteesta.\n\n– {product_name}-turvallisuustiimi"
	},
	"self_deletion_scheduled": {
		"subject": "{product_name}-tilisi poistaminen on ajoitettu",
		"body": "Hei {username},\n\nPyysit {product_name}-tilisi poistamista. Tilisi on ajoitettu poistettavaksi pysyvästi:\n\n{deletionDate, date, full} klo {deletionDate, time, short}\n\nJos et pyytänyt tätä, kirjaudu tilillesi peruuttaaksesi poiston. Suosittelemme myös vaihtamaan salasanasi tilisi turvaamiseksi.\n\n– {product_name}-tiimi"
	},
	"unban_notification": {
		"subject": "{product_name}-tilisi jäädytys on poistettu",
		"body": "Hei {username},\n\nHyviä uutisia: {product_name}-tilisi jäädytys on poistettu.\n\n{reason, select,\n  null {}\n  other {Syy: {reason}}\n}\n\nVoit nyt kirjautua takaisin sisään ja jatkaa {product_name}-palvelun käyttöä normaalisti.\n\n– {product_name}-turvallisuustiimi"
	}
});

export default EMAIL_I18N_FI_MESSAGES;
