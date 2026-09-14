// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '@pkgs/email/src/email_i18n/EmailI18nMessages';

const EMAIL_I18N_HU_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "{product_name}-fiókodat ideiglenesen letiltottuk",
		"body": "Szia, {username}!\n\nIdeiglenesen letiltottuk a {product_name}-fiókodat, mert gyanús tevékenységet észleltünk.\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nA fiókodhoz való hozzáférés visszaszerzéséhez vissza kell állítanod a jelszavadat:\n\n{forgotUrl}\n\nA jelszó visszaállítása után újra be tudsz jelentkezni.\n\nHa úgy gondolod, hogy ez tévedésből történt, vedd fel a kapcsolatot az ügyfélszolgálatunkkal.\n\n– {product_name} biztonsági csapata"
	},
	"account_scheduled_deletion": {
		"subject": "{product_name}-fiókodat véglegesen törölni fogjuk",
		"body": "Szia, {username}!\n\n{product_name}-fiókod végleges törlését ütemeztük a szolgáltatási feltételeink vagy a közösségi irányelveink megsértése miatt.\n\nA törlés tervezett időpontja: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nEz súlyos moderációs intézkedés. Fiókod adatait a megadott időpontban véglegesen töröljük.\n\nKérjük, olvasd át az alábbiakat:\n- Szolgáltatási feltételek: {termsUrl}\n- Közösségi irányelvek: {guidelinesUrl}\n\nFellebbezés:\nHa úgy gondolod, hogy ez a döntés hibás vagy indokolatlan volt, 60 napod van fellebbezni. Erről az e-mail-címről írj a következő címre: {appeals_email}.\n\nA fellebbezésben:\n- Világosan fejtsd ki, miért gondolod, hogy a döntés hibás vagy indokolatlan volt.\n- Add meg a kapcsolódó bizonyítékokat és a körülményeket.\n\nA {product_name} biztonsági csapatának egyik tagja elbírálja a fellebbezésedet, és a végleges döntésig felfüggesztheti a tervezett törlést.\n\n– {product_name} biztonsági csapata"
	},
	"account_temp_banned": {
		"subject": "{product_name}-fiókodat ideiglenesen felfüggesztettük",
		"body": "Szia, {username}!\n\n{product_name}-fiókodat ideiglenesen felfüggesztettük a szolgáltatási feltételeink vagy a közösségi irányelveink megsértése miatt.\n\nIdőtartam: {durationHours, plural,\n  =1 {1 óra}\n  other {# óra}\n}\nA felfüggesztés vége: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nEz idő alatt nem férsz hozzá a fiókodhoz.\n\nKérjük, olvasd át az alábbiakat:\n- Szolgáltatási feltételek: {termsUrl}\n- Közösségi irányelvek: {guidelinesUrl}\n\nHa úgy gondolod, hogy ez a döntés hibás vagy indokolatlan volt, fellebbezhetsz. Erről az e-mail-címről írj a következő címre: {appeals_email}, és világosan fejtsd ki, miért tartod hibásnak a döntést. Elbíráljuk a fellebbezésedet, és értesítünk a döntésünkről.\n\n– {product_name} biztonsági csapata"
	},
	"donation_confirmation": {
		"subject": "Köszönjük, hogy adományoddal támogatod a {product_name} működését",
		"body": "Szia!\n\nKöszönjük, hogy adományoddal támogatod a {product_name} működését! {interval, select,\n  month {Sikeresen beállítottuk a rendszeres adományodat.}\n  year {Sikeresen beállítottuk a rendszeres adományodat.}\n  other {Sikeresen feldolgoztuk az egyszeri adományodat.}\n}\n\nAz adomány részletei:\nÖsszeg: {amount} {currency} {interval, select,\n  month {havonta}\n  year {évente}\n  other {}\n}\n\nA Stripe hamarosan külön e-mailben küld nyugtát és egy PDF-számlát. Ezek tartalmazzák az összes fizetési adatot, és adózási célokra is használhatók.\n\nAz alábbi linken bármikor megtekintheted korábbi adományaidat, letöltheted a számlákat, {interval, select,\n  month {valamint kezelheted vagy lemondhatod az előfizetésedet}\n  year {valamint kezelheted vagy lemondhatod az előfizetésedet}\n  other {valamint kezelheted a jövőbeli adományaidat}\n}:\n\n{manageUrl}\n\nTámogatásod segít fenntartani a {product_name} működését. Köszönjük!\n\n– {product_name} csapata"
	},
	"donation_magic_link": {
		"subject": "A {product_name} működését támogató adományaid kezelése",
		"body": "Szia!\n\nKattints az alábbi linkre az adományozói portálod eléréséhez:\n\n{manageUrl}\n\nA portálon kezelheted az előfizetéseket, letöltheted a számlákat, és megtekintheted az adománytörténetedet.\n\nEz a link {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt a linket, nyugodtan figyelmen kívül hagyhatod ezt az e-mailt.\n\n– {product_name} csapata"
	},
	"dsa_report_verification": {
		"subject": "Erősítsd meg e-mail-címedet a DSA-bejelentéshez",
		"body": "Szia!\n\nAz alábbi ellenőrző kóddal küldheted el a digitális szolgáltatásokról szóló rendelet (DSA) szerinti bejelentésedet a {product_name} felületén:\n\n{code}\n\nA kód lejárati időpontja: {expiresAt, date, full} {expiresAt, time, short}.\n\nHa nem te kérted ezt a kódot, figyelmen kívül hagyhatod ezt az e-mailt.\n\n– {product_name} biztonsági csapata"
	},
	"email_change_new": {
		"subject": "Erősítsd meg {product_name}-fiókod új e-mail-címét",
		"body": "Szia, {username}!\n\nAdd meg ezt a kódot az alkalmazásban {product_name}-fiókod új e-mail-címének megerősítéséhez:\n\n{code}\n\nEz a kód {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt, figyelmen kívül hagyhatod ezt az e-mailt.\n\n– {product_name} csapata"
	},
	"email_change_original": {
		"subject": "Erősítsd meg {product_name}-fiókod e-mail-címének módosítását",
		"body": "Szia, {username}!\n\nKérést kaptunk a {product_name}-fiókod e-mail-címének megváltoztatására.\n\nA változtatás megerősítéséhez add meg ezt a kódot az alkalmazásban:\n\n{code}\n\nEz a kód {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt, azonnal helyezd biztonságba a fiókodat.\n\n– {product_name} csapata"
	},
	"email_change_revert": {
		"subject": "Megváltozott {product_name}-fiókod e-mail-címe",
		"body": "Szia, {username}!\n\n{product_name}-fiókod e-mail-címe a következőre változott: {newEmail}.\n\nHa te módosítottad, nincs további teendőd. Ha nem te voltál, az alábbi linken visszavonhatod a módosítást, és biztonságba helyezheted a fiókodat:\n\n{revertUrl}\n\nEzzel visszaállítjuk a korábbi e-mail-címedet, mindenhol kijelentkeztetünk, eltávolítjuk a kapcsolt telefonszámokat, kikapcsoljuk a többfaktoros azonosítást, és új jelszót kell beállítanod.\n\n– {product_name} biztonsági csapata"
	},
	"email_verification": {
		"subject": "Erősítsd meg {product_name}-fiókod e-mail-címét",
		"body": "Szia, {username}!\n\nErősítsd meg a {product_name}-fiókod e-mail-címét az alábbi linkre kattintva:\n\n{verifyUrl}\n\nHa nem hoztál létre {product_name}-fiókot, nyugodtan figyelmen kívül hagyhatod ezt az e-mailt.\n\nEz a link 24 óráig érvényes.\n\n– {product_name} csapata"
	},
	"gift_chargeback_notification": {
		"subject": "Megszűntek a beváltott ajándékodhoz tartozó előnyök",
		"body": "Szia, {username}!\n\nAz általad beváltott ajándékkódot eredetileg valaki más fizette ki. Ezt a fizetést azóta visszaterhelték (chargeback).\n\nEmiatt megszüntettük azokat az előnyöket, amelyeket az ajándék beváltásakor kapott a fiókod.\n\nHa úgy gondolod, hogy ez tévedés, vedd fel a kapcsolatot ügyfélszolgálatunkkal, és add meg az ajándékkóddal és a beváltás időpontjával kapcsolatos ismert részleteket.\n\n– {product_name} csapata"
	},
	"harvest_completed": {
		"subject": "A {product_name} adatexportja elkészült, letöltheted az adataidat",
		"body": "Szia, {username}!\n\nAz adatexportod elkészült.\n\nLetöltési link:\n{downloadUrl}\n\nÜzenetek száma: {totalMessages, number}\nFájlméret: {fileSizeMB, number} MB\n\nEz a link {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt az exportálást, azonnal változtasd meg a jelszavadat, és vedd fel a kapcsolatot az ügyfélszolgálatunkkal.\n\n– {product_name} csapata"
	},
	"inactivity_warning": {
		"subject": "{product_name}-fiókodat inaktivitás miatt törölni fogjuk",
		"body": "Szia, {username}!\n\nEzen dátum óta nem észleltünk tevékenységet {product_name}-fiókodban: {lastActiveDate, date, full}.\n\nHa nem jelentkezel be az alábbi időpontig, inaktivitás miatt véglegesen töröljük a fiókodat: {deletionDate, date, full} {deletionDate, time, short}.\n\nItt jelentkezhetsz be:\n{loginUrl}\n\nHa mostanában használtad a {product_name} alkalmazást, azonnal vedd fel a kapcsolatot ügyfélszolgálatunkkal.\n\n– {product_name} csapata"
	},
	"ip_authorization": {
		"subject": "Új IP-címről való bejelentkezés engedélyezése",
		"body": "Szia, {username}!\n\nBejelentkezési kísérletet észleltünk a {product_name}-fiókodba egy új IP-címről:\n\nIP-cím: {ipAddress}\nHelyszín: {location}\n\nHa te voltál, engedélyezd ezt az IP-címet az alábbi linkre kattintva:\n\n{authUrl}\n\nHa nem te próbáltál bejelentkezni, azonnal változtasd meg a jelszavadat.\n\nEz a link 30 percig érvényes.\n\n– {product_name} csapata"
	},
	"mfa_backup_codes_view": {
		"subject": "Erősítsd meg {product_name}-fiókod biztonsági kódjainak megtekintését",
		"body": "Szia, {username}!\n\nKérést kaptunk a {product_name}-fiókod biztonsági kódjainak megtekintésére.\n\nA kérés megerősítéséhez add meg ezt a kódot az alkalmazásban:\n\n{code}\n\nEz a kód {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt, valaki hozzáférhet a fiókodhoz. Azonnal változtasd meg a jelszavadat.\n\n– {product_name} csapata"
	},
	"password_change_verification": {
		"subject": "Erősítsd meg {product_name}-fiókod jelszavának módosítását",
		"body": "Szia, {username}!\n\nKérést kaptunk {product_name}-fiókod jelszavának módosítására.\n\nA módosítás megerősítéséhez add meg ezt a kódot az alkalmazásban:\n\n{code}\n\nA kód lejárati időpontja: {expiresAt}.\n\nHa nem te kérted ezt, lehet, hogy valaki más is hozzáfér a fiókodhoz. Azonnal változtasd meg a jelszavadat, és kapcsold be a kétlépcsős azonosítást.\n\n– {product_name} csapata"
	},
	"password_reset": {
		"subject": "Állítsd vissza {product_name}-fiókod jelszavát",
		"body": "Szia, {username}!\n\n{product_name}-fiókod jelszavának visszaállítását kérted. Használd az alábbi linket új jelszó beállításához:\n\n{resetUrl}\n\nHa nem kérted ezt, nyugodtan figyelmen kívül hagyhatod ezt az e-mailt.\n\nEz a link 1 óráig érvényes.\n\n– {product_name} csapata"
	},
	"registration_approved": {
		"subject": "Jóváhagytuk a regisztrációdat a {product_name} felületén",
		"body": "Szia, {username}!\n\nJó hír: jóváhagytuk a regisztrációdat a {product_name} felületén.\n\nMost már bejelentkezhetsz az alkalmazásba:\n{channelsUrl}\n\nÜdvözlünk a {product_name} közösségében!\n\n– {product_name} csapata"
	},
	"report_resolved": {
		"subject": "Elbíráltuk a {product_name} felületén tett bejelentésedet",
		"body": "Szia, {username}!\n\nBiztonsági csapatunk elbírálta a bejelentésedet (azonosító: {reportId}).{hasComment, select, yes {\n\nA biztonsági csapat válasza:\n{publicComment}} other {}}\n\nKöszönjük, hogy segítesz mindenki számára biztonságossá tenni a {product_name} közösségét. Minden bejelentést komolyan veszünk, és nagyra értékeljük, hogy segíted a közösséget.\n\nHa kérdésed vagy aggályod van az eredménnyel kapcsolatban, írj erre a címre: {safety_email}.\n\n– {product_name} biztonsági csapata"
	},
	"scheduled_deletion_notification": {
		"subject": "{product_name}-fiókodat véglegesen törölni fogjuk",
		"body": "Szia, {username}!\n\nÜtemeztük {product_name}-fiókod végleges törlését.\n\nA törlés tervezett időpontja: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nEz súlyos moderációs intézkedés. Fiókod adatait a megadott időpontban véglegesen töröljük.\n\nHa úgy gondolod, hogy ez a döntés hibás volt, fellebbezhetsz. Erről az e-mail-címről írj a következő címre: {appeals_email}.\n\n– {product_name} biztonsági csapata"
	},
	"self_deletion_scheduled": {
		"subject": "Ütemeztük {product_name}-fiókod törlését",
		"body": "Szia, {username}!\n\nKérted a {product_name}-fiókod törlését. Fiókod végleges törlése a következő időpontra van ütemezve:\n\n{deletionDate, date, full} {deletionDate, time, short}\n\nHa nem te kérted ezt, jelentkezz be a fiókodba a törlés visszavonásához. Javasoljuk továbbá, hogy változtasd meg a jelszavadat a fiókod biztonságának megőrzése érdekében.\n\n– {product_name} csapata"
	},
	"unban_notification": {
		"subject": "Feloldottuk {product_name}-fiókod felfüggesztését",
		"body": "Szia, {username}!\n\nJó hír: feloldottuk {product_name}-fiókod felfüggesztését.\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nMost már újra bejelentkezhetsz, és a megszokott módon használhatod a {product_name} alkalmazást.\n\n– {product_name} biztonsági csapata"
	}
});

export default EMAIL_I18N_HU_MESSAGES;
