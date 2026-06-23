// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_HU_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "A {product_name} fiókodat ideiglenesen letiltottuk",
		"body": "Szia {username},\n\nIdeiglenesen letiltottuk a {product_name} fiókodat, mert gyanús tevékenységet észleltünk.\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nA fiókodhoz való hozzáférés visszaszerzéséhez vissza kell állítanod a jelszavadat:\n\n{forgotUrl}\n\nA jelszó visszaállítása után újra be tudsz jelentkezni.\n\nHa úgy gondolod, hogy ez tévedésből történt, vedd fel a kapcsolatot a támogatási csapatunkkal.\n\n– {product_name} Biztonsági Csapat"
	},
	"account_scheduled_deletion": {
		"subject": "{product_name} fiókod véglegesen törölve lesz",
		"body": "Szia {username},\n\n{product_name} fiókod végleges törlésre van ütemezve a Szolgáltatási feltételeink vagy a Közösségi irányelveink megsértése miatt.\n\nTervezett törlés: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nEz egy komoly végrehajtási intézkedés. Fiókod adatai a tervezett dátumon véglegesen törölve lesznek.\n\nTekintsd át:\n- Szolgáltatási feltételek: {termsUrl}\n- Közösségi irányelvek: {guidelinesUrl}\n\nFellebbezési eljárás:\nHa úgy gondolod, hogy ez a végrehajtási döntés helytelen vagy indokolatlan volt, 60 napod van fellebbezést benyújtani. Küldj e-mailt az {appeals_email} címre erről az e-mail címről.\n\nA fellebbezésedben:\n- Egyértelműen magyarázd el, miért gondolod, hogy a végrehajtási döntés helytelen vagy indokolatlan volt\n- Adj meg minden releváns bizonyítékot vagy kontextust\n\nA {product_name} Biztonsági Csapatának egy tagja felülvizsgálja a fellebbezésedet, és felfüggesztheti a függőben lévő törlést, amíg végleges döntés nem születik.\n\n– {product_name} Biztonsági Csapat"
	},
	"account_temp_banned": {
		"subject": "A {product_name} fiókodat ideiglenesen felfüggesztettük",
		"body": "Szia {username},\n\n{product_name} fiókodat ideiglenesen felfüggesztettük a Szolgáltatási feltételeink vagy a Közösségi irányelveink megsértése miatt.\n\nIdőtartam: {durationHours, plural,\n  =1 {1 óra}\n  other {# óra}\n}\nFelfüggesztve eddig: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nEz idő alatt nem fogsz tudni hozzáférni a fiókodhoz.\n\nTekintsd át:\n- Szolgáltatási feltételek: {termsUrl}\n- Közösségi irányelvek: {guidelinesUrl}\n\nHa úgy gondolod, hogy ez a végrehajtási döntés helytelen vagy indokolatlan volt, fellebbezést nyújthatsz be. Küldj e-mailt az {appeals_email} címre erről az e-mail címről, és egyértelműen magyarázd el, miért gondolod, hogy a döntés helytelen volt. Felülvizsgáljuk a fellebbezésedet, és válaszolunk a döntésünkről.\n\n– {product_name} Biztonsági Csapat"
	},
	"donation_confirmation": {
		"subject": "Köszönjük a {product_name} adományodat",
		"body": "Szia,\n\nKöszönjük a {product_name} adományodat! A {interval, select,\n  month {rendszeres adományod}\n  year {rendszeres adományod}\n  other {egyszeri adományod}\n} sikeresen {interval, select,\n  month {beállítottuk}\n  year {beállítottuk}\n  other {feldolgoztuk}\n}.\n\nAdomány részletei:\nÖsszeg: {amount} {currency} {interval, select,\n  month {havonta}\n  year {évente}\n  other {}\n}\n\nA Stripe hamarosan külön nyugtát küld neked e-mailben a számla PDF-jével. Ez tartalmazza az összes fizetési adatot, és adózási célokra is felhasználható.\n\nAdománytörténetedet, számláidat és {interval, select,\n  month {előfizetésedet}\n  year {előfizetésedet}\n  other {jövőbeli adományaidat}\n} bármikor kezelheted vagy lemondhatod ezen a linken:\n\n{manageUrl}\n\nA támogatásod segít a {product_name} működésében. Köszönjük!\n\n– {product_name} Csapat"
	},
	"donation_magic_link": {
		"subject": "A {product_name} adományaid kezelése",
		"body": "Szia,\n\nKattints az alábbi linkre az adományozói portálod eléréséhez:\n\n{manageUrl}\n\nA portálon kezelheted az előfizetéseket, letöltheted a számlákat, és megtekintheted az adománytörténetedet.\n\nEz a link {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt a linket, nyugodtan figyelmen kívül hagyhatod ezt az e-mailt.\n\n– {product_name} Csapat"
	},
	"dsa_report_verification": {
		"subject": "Erősítsd meg e-mail címedet a DSA jelentésedhez",
		"body": "Szia,\n\nHasználd az alábbi ellenőrző kódot a DSA jelentésed benyújtásához a {product_name} felületén:\n\n{code}\n\nEz a kód {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt, figyelmen kívül hagyhatod ezt az e-mailt.\n\n– {product_name} Biztonsági Csapat"
	},
	"email_change_new": {
		"subject": "Erősítsd meg új {product_name} e-mail címedet",
		"body": "Szia {username},\n\nAdd meg ezt a kódot az alkalmazásban az új {product_name} e-mail címed megerősítéséhez:\n\n{code}\n\nEz a kód {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt, figyelmen kívül hagyhatod ezt az e-mailt.\n\n– {product_name} Csapat"
	},
	"email_change_original": {
		"subject": "Erősítsd meg a {product_name} e-mail címed megváltoztatását",
		"body": "Szia {username},\n\nKérést kaptunk a {product_name} fiókod e-mail címének megváltoztatására.\n\nA változtatás megerősítéséhez add meg ezt a kódot az alkalmazásban:\n\n{code}\n\nEz a kód {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt, azonnal biztosítsd a fiókodat.\n\n– {product_name} Csapat"
	},
	"email_change_revert": {
		"subject": "A {product_name} e-mail címed megváltozott",
		"body": "Szia {username},\n\nA {product_name} fiókod e-mail címe a következőre változott: {newEmail}.\n\nHa te végezted ezt a változtatást, nincs szükség további intézkedésre. Ha nem te voltál, visszavonhatod a változtatást, és biztosíthatod a fiókodat ezen a linken:\n\n{revertUrl}\n\nEz visszaállítja az előző e-mail címedet, kijelentkeztet mindenhol, eltávolítja a kapcsolt telefonszámokat, letiltja az MFA-t, és új jelszó beállítására lesz szükséged.\n\n– {product_name} Biztonsági Csapat"
	},
	"email_verification": {
		"subject": "Erősítsd meg {product_name} e-mail címedet",
		"body": "Szia {username},\n\nErősítsd meg a {product_name} fiókod e-mail címét az alábbi linkre kattintva:\n\n{verifyUrl}\n\nHa nem hoztál létre {product_name} fiókot, nyugodtan figyelmen kívül hagyhatod ezt az e-mailt.\n\nEz a link 24 óráig érvényes.\n\n– {product_name} Csapat"
	},
	"gift_chargeback_notification": {
		"subject": "Eltávolítottuk a beváltott ajándékod előnyeit",
		"body": "Szia {username},\n\nAz általad beváltott ajándékkódot eredetileg valaki más fizette. Ez a fizetés azóta visszavonásra került (visszaterhelés).\n\nEmiatt eltávolítottuk azokat az előnyöket, amelyeket a fiókodhoz adtunk, amikor beváltottad az ajándékot.\n\nHa úgy gondolod, hogy ez hiba, vedd fel a kapcsolatot a támogatási csapatunkkal, és add meg az ajándékkóddal és a beváltás időpontjával kapcsolatos részleteket.\n\n– {product_name} Csapat"
	},
	"harvest_completed": {
		"subject": "A {product_name} adatexportod letölthető",
		"body": "Szia {username},\n\nAdataid exportálása elkészült.\n\nLetöltési link:\n{downloadUrl}\n\nÜzenetek száma: {totalMessages, number}\nFájlméret: {fileSizeMB, number} MB\n\nEz a link {expiresAt, date, full} {expiresAt, time, short} időpontban jár le.\n\nHa nem kérted ezt az exportálást, azonnal változtasd meg a jelszavadat, és vedd fel a kapcsolatot a támogatási csapatunkkal.\n\n– {product_name} Csapat"
	},
	"inactivity_warning": {
		"subject": "A {product_name} fiókodat inaktivitás miatt töröljük",
		"body": "Szia {username},\n\nNem észleltünk semmilyen tevékenységet a {product_name} fiókodon {lastActiveDate, date, full} óta.\n\nHa nem jelentkezel be {deletionDate, date, full} {deletionDate, time, short} időpontig, fiókod véglegesen törlésre kerül inaktivitás miatt.\n\nJelentkezz be itt:\n{loginUrl}\n\nHa mostanában használtad a {product_name} alkalmazást, azonnal vedd fel a kapcsolatot a támogatási csapatunkkal.\n\n– {product_name} Csapat"
	},
	"ip_authorization": {
		"subject": "Új IP-címről való bejelentkezés engedélyezése",
		"body": "Szia {username},\n\nBejelentkezési kísérletet észleltünk a {product_name} fiókodba egy új IP-címről:\n\nIP-cím: {ipAddress}\nHelyszín: {location}\n\nHa te voltál, engedélyezd ezt az IP-címet az alábbi linkre kattintva:\n\n{authUrl}\n\nHa nem te próbáltál bejelentkezni, azonnal változtasd meg a jelszavadat.\n\nEz a link 30 percig érvényes.\n\n– {product_name} Csapat"
	},
	"password_change_verification": {
		"subject": "Erősítsd meg a {product_name} jelszócserédet",
		"body": "Szia {username},\n\nKérést kaptunk a {product_name} fiókod jelszavának megváltoztatására.\n\nA változtatás megerősítéséhez add meg ezt a kódot az alkalmazásban:\n\n{code}\n\nEz a kód {expiresAt}-kor jár le.\n\nHa nem kérted ezt, valaki hozzáférhet a fiókodhoz. Azonnal változtasd meg a jelszavadat, és engedélyezd a kétfaktoros hitelesítést.\n\n– {product_name} Csapat"
	},
	"password_reset": {
		"subject": "A {product_name} jelszó visszaállítása",
		"body": "Szia {username},\n\n{product_name} jelszó visszaállítását kérted. Használd az alábbi linket új jelszó beállításához:\n\n{resetUrl}\n\nHa nem kérted ezt, nyugodtan figyelmen kívül hagyhatod ezt az e-mailt.\n\nEz a link 1 óráig érvényes.\n\n– {product_name} Csapat"
	},
	"registration_approved": {
		"subject": "A {product_name} regisztrációdat jóváhagytuk",
		"body": "Szia {username},\n\nJó hír: {product_name} regisztrációd jóváhagyásra került.\n\nMost már bejelentkezhetsz a {product_name} alkalmazásba itt:\n{channelsUrl}\n\nÜdvözlünk a {product_name} közösségben.\n\n– {product_name} Csapat"
	},
	"report_resolved": {
		"subject": "A {product_name} jelentésedet felülvizsgáltuk",
		"body": "Szia {username},\n\nJelentésedet (azonosító: {reportId}) felülvizsgálta Biztonsági Csapatunk.{hasComment, select, yes {\n\nVálasz a Biztonsági Csapattól:\n{publicComment}} other {}}\n\nKöszönjük, hogy segítesz a {product_name} biztonságos megőrzésében mindenki számára. Minden jelentést komolyan veszünk, és nagyra értékeljük a közösséghez való hozzájárulásodat.\n\nHa bármilyen kérdésed vagy aggályod van ezzel az eredménnyel kapcsolatban, vedd fel a kapcsolatot a {safety_email} címen.\n\n– {product_name} Biztonsági Csapat"
	},
	"scheduled_deletion_notification": {
		"subject": "{product_name} fiókod véglegesen törölve lesz",
		"body": "Szia {username},\n\nA {product_name} fiókod végleges törlésre van ütemezve.\n\nTervezett törlés: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nEz egy komoly végrehajtási intézkedés. Fiókod adatai a tervezett dátumon véglegesen törölve lesznek.\n\nHa úgy gondolod, hogy ez a végrehajtási döntés helytelen volt, fellebbezést nyújthatsz be. Küldj e-mailt az {appeals_email} címre erről az e-mail címről.\n\n– {product_name} Biztonsági Csapat"
	},
	"self_deletion_scheduled": {
		"subject": "A {product_name} fiókod törlését ütemeztük",
		"body": "Szia {username},\n\nKérted a {product_name} fiókod törlését. Fiókod végleges törlése a következő időpontra van ütemezve:\n\n{deletionDate, date, full} {deletionDate, time, short}\n\nHa nem te kérted ezt, jelentkezz be a fiókodba a törlés visszavonásához. Javasoljuk továbbá, hogy változtasd meg a jelszavadat a fiókod biztonságának megőrzése érdekében.\n\n– {product_name} Csapat"
	},
	"unban_notification": {
		"subject": "A {product_name} fiókod felfüggesztését feloldottuk",
		"body": "Szia {username},\n\nJó hír: {product_name} fiókod felfüggesztése feloldásra került.\n\n{reason, select,\n  null {}\n  other {Indok: {reason}}\n}\n\nMost már újra bejelentkezhetsz, és a szokásos módon használhatod a {product_name} alkalmazást.\n\n– {product_name} Biztonsági Csapat"
	}
});

export default EMAIL_I18N_HU_MESSAGES;
