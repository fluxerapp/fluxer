// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_CS_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Tvůj účet {product_name} byl dočasně deaktivován",
		"body": "Ahoj {username},\n\nDočasně jsme deaktivovali tvůj účet {product_name}, protože jsme zaznamenali podezřelou aktivitu.\n\n{reason, select,\n  null {}\n  other {Důvod: {reason}}\n}\n\nPro opětovný přístup k účtu si budeš muset resetovat heslo:\n\n{forgotUrl}\n\nPo resetování hesla se budeš moci znovu přihlásit.\n\nPokud se domníváš, že se jedná o chybu, kontaktuj prosím náš tým podpory.\n\n– Bezpečnostní tým {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Tvůj účet {product_name} bude trvale smazán",
		"body": "Ahoj {username},\n\nTvůj účet {product_name} byl naplánován k trvalému smazání z důvodu porušení našich Podmínek služby nebo Pokynů komunity.\n\nNaplánované smazání: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Důvod: {reason}}\n}\n\nJedná se o závažné opatření. Tvá data účtu budou trvale smazána v naplánovaný den.\n\nZkontroluj prosím:\n- Podmínky služby: {termsUrl}\n- Pokyny komunity: {guidelinesUrl}\n\nProces odvolání:\nPokud se domníváš, že toto rozhodnutí o vynucení bylo nesprávné nebo neoprávněné, máš 60 dní na podání odvolání. Odešli e-mail na {appeals_email} z této e-mailové adresy.\n\nVe svém odvolání:\n- Jasně vysvětli, proč se domníváš, že rozhodnutí o vynucení bylo nesprávné nebo neoprávněné\n- Poskytni veškeré relevantní důkazy nebo kontext\n\nČlen bezpečnostního týmu {product_name} tvé odvolání přezkoumá a může pozastavit probíhající smazání, dokud nebude dosaženo konečného rozhodnutí.\n\n– Bezpečnostní tým {product_name}"
	},
	"account_temp_banned": {
		"subject": "Tvůj účet {product_name} byl dočasně pozastaven",
		"body": "Ahoj {username},\n\nTvůj účet {product_name} byl dočasně pozastaven za porušení našich Podmínek služby nebo Pokynů komunity.\n\nDoba trvání: {durationHours, plural,\n  one {1 hodina} few {hodiny}\n  many {hodiny} other {# hodin}\n}\nPozastaveno do: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Důvod: {reason}}\n}\n\nBěhem této doby nebudeš mít přístup ke svému účtu.\n\nZkontroluj prosím:\n- Podmínky služby: {termsUrl}\n- Pokyny komunity: {guidelinesUrl}\n\nPokud se domníváš, že toto rozhodnutí o vynucení bylo nesprávné nebo neoprávněné, můžeš podat odvolání. Odešli e-mail na {appeals_email} z této e-mailové adresy a jasně vysvětli, proč se domníváš, že rozhodnutí bylo nesprávné. Tvé odvolání přezkoumáme a odpovíme ti s naším rozhodnutím.\n\n– Bezpečnostní tým {product_name}"
	},
	"donation_confirmation": {
		"subject": "Děkujeme za tvůj dar na {product_name}",
		"body": "Ahoj,\n\nDěkujeme za tvůj dar pro {product_name}! Tvůj {interval, select,\n  month {opakovaný dar}\n  year {opakovaný dar}\n  other {jednorázový dar}\n} byl {interval, select,\n  month {nastaven}\n  year {nastaven}\n  other {zpracován}\n} úspěšně.\n\nPodrobnosti o daru:\nČástka: {amount} {currency} {interval, select,\n  month {měsíčně}\n  year {ročně}\n  other {}\n}\n\nStripe ti brzy zašle samostatnou účtenku s PDF fakturou. Ta obsahuje všechny platební údaje a může být použita pro daňové účely.\n\nHistorii svých darů, stahování faktur {interval, select,\n  month {a správu nebo zrušení předplatného}\n  year {a správu nebo zrušení předplatného}\n  other {a správu budoucích darů}\n} můžeš kdykoli zobrazit pomocí tohoto odkazu:\n\n{manageUrl}\n\nTvá podpora pomáhá udržovat {product_name} v chodu. Děkujeme!\n\n– Tým {product_name}"
	},
	"donation_magic_link": {
		"subject": "Spravuj své dary pro {product_name}",
		"body": "Ahoj,\n\nKlikni na odkaz níže pro přístup k portálu pro dárce:\n\n{manageUrl}\n\nV portálu můžeš spravovat předplatná, stahovat faktury a prohlížet si historii svých darů.\n\nTento odkaz vyprší dne {expiresAt, date, full} v {expiresAt, time, short}.\n\nPokud jsi tento odkaz nepožadoval, můžeš tento e-mail klidně ignorovat.\n\n– Tým {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Ověř svůj e-mail pro nahlášení podle DSA",
		"body": "Ahoj,\n\nPoužij ověřovací kód níže k odeslání svého nahlášení podle zákona o digitálních službách na {product_name}:\n\n{code}\n\nTento kód vyprší dne {expiresAt, date, full} v {expiresAt, time, short}.\n\nPokud jsi o to nežádal, můžeš tento e-mail ignorovat.\n\n– Bezpečnostní tým {product_name}"
	},
	"email_change_new": {
		"subject": "Ověř svůj nový e-mail pro {product_name}",
		"body": "Ahoj {username},\n\nZadej tento kód do aplikace pro ověření tvého nového e-mailu pro {product_name}:\n\n{code}\n\nTento kód vyprší dne {expiresAt, date, full} v {expiresAt, time, short}.\n\nPokud jsi o to nežádal, můžeš tento e-mail ignorovat.\n\n– Tým {product_name}"
	},
	"email_change_original": {
		"subject": "Potvrď změnu e-mailu pro {product_name}",
		"body": "Ahoj {username},\n\nObdrželi jsme požadavek na změnu e-mailové adresy tvého účtu {product_name}.\n\nPro potvrzení této změny zadej tento kód do aplikace:\n\n{code}\n\nTento kód vyprší dne {expiresAt, date, full} v {expiresAt, time, short}.\n\nPokud jsi o to nežádal, zabezpeč si svůj účet ihned.\n\n– Tým {product_name}"
	},
	"email_change_revert": {
		"subject": "Tvůj e-mail pro {product_name} byl změněn",
		"body": "Ahoj {username},\n\nE-mailová adresa tvého účtu {product_name} byla změněna na {newEmail}.\n\nPokud jsi tuto změnu provedl ty, není potřeba žádná akce. Pokud ne, můžeš změnu vrátit zpět a zabezpečit svůj účet pomocí tohoto odkazu:\n\n{revertUrl}\n\nTím se obnoví tvůj předchozí e-mail, odhlásíš se všude, odstraní se propojená telefonní čísla, deaktivuje se MFA a budeš muset nastavit nové heslo.\n\n– Bezpečnostní tým {product_name}"
	},
	"email_verification": {
		"subject": "Ověř svou e-mailovou adresu pro {product_name}",
		"body": "Ahoj {username},\n\nOvěř e-mailovou adresu svého účtu {product_name} kliknutím na odkaz níže:\n\n{verifyUrl}\n\nPokud jsi si účet {product_name} nevytvořil, můžeš tento e-mail klidně ignorovat.\n\nTento odkaz je platný 24 hodin.\n\n– Tým {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Výhody z uplatněného dárku byly odstraněny",
		"body": "Ahoj {username},\n\nDárkový kód, který jsi uplatnil, byl původně zaplacen někým jiným. Tato platba byla mezitím zrušena (chargeback).\n\nZ tohoto důvodu jsme odstranili výhody, které byly přidány k tvému účtu, když jsi dárek uplatnil.\n\nPokud si myslíš, že se jedná o chybu, kontaktuj prosím náš tým podpory a uveď veškeré podrobnosti, které máš o dárkovém kódu a o tom, kdy jsi ho uplatnil.\n\n– Tým {product_name}"
	},
	"harvest_completed": {
		"subject": "Export tvých dat z {product_name} je připraven ke stažení",
		"body": "Ahoj {username},\n\nTvůj export dat je připraven.\n\nOdkaz ke stažení:\n{downloadUrl}\n\nZahrnuté zprávy: {totalMessages, number}\nVelikost souboru: {fileSizeMB, number} MB\n\nTento odkaz vyprší dne {expiresAt, date, full} v {expiresAt, time, short}.\n\nPokud jsi o tento export nežádal, okamžitě si změň heslo a kontaktuj náš tým podpory.\n\n– Tým {product_name}"
	},
	"inactivity_warning": {
		"subject": "Tvůj účet {product_name} bude smazán kvůli neaktivitě",
		"body": "Ahoj {username},\n\nNezaznamenali jsme žádnou aktivitu na tvém účtu {product_name} od {lastActiveDate, date, full}.\n\nPokud se nepřihlásíš do {deletionDate, date, full} v {deletionDate, time, short}, tvůj účet bude trvale smazán kvůli neaktivitě.\n\nPřihlas se zde:\n{loginUrl}\n\nPokud jsi {product_name} nedávno používal, kontaktuj náš tým podpory ihned.\n\n– Tým {product_name}"
	},
	"ip_authorization": {
		"subject": "Povolit přihlášení z nové IP adresy",
		"body": "Ahoj {username},\n\nZaznamenali jsme pokus o přihlášení k tvému účtu {product_name} z nové IP adresy:\n\nIP adresa: {ipAddress}\nPoloha: {location}\n\nPokud jsi to byl ty, autorizuj tuto IP adresu kliknutím na odkaz níže:\n\n{authUrl}\n\nPokud jsi se nepřihlašoval, okamžitě si změň heslo.\n\nTento odkaz je platný 30 minut.\n\n– Tým {product_name}"
	},
	"password_change_verification": {
		"subject": "Potvrď změnu hesla k {product_name}",
		"body": "Ahoj {username},\n\nObdrželi jsme požadavek na změnu hesla k tvému účtu pro {product_name}.\n\nPro potvrzení této změny zadej tento kód do aplikace:\n\n{code}\n\nTento kód vyprší v {expiresAt}.\n\nPokud jsi o to nežádal, někdo může mít přístup k tvému účtu. Okamžitě si změň heslo a povol dvoufaktorové ověřování.\n\n– Tým {product_name}"
	},
	"password_reset": {
		"subject": "Obnov své heslo k {product_name}",
		"body": "Ahoj {username},\n\nPožádal jsi o reset hesla pro {product_name}. Použij odkaz níže pro nastavení nového hesla:\n\n{resetUrl}\n\nPokud jsi o to nežádal, můžeš tento e-mail klidně ignorovat.\n\nTento odkaz je platný 1 hodinu.\n\n– Tým {product_name}"
	},
	"registration_approved": {
		"subject": "Tvá registrace k {product_name} byla schválena",
		"body": "Ahoj {username},\n\nDobré zprávy: tvá registrace do {product_name} byla schválena.\n\nNyní se můžeš přihlásit do aplikace {product_name} zde:\n{channelsUrl}\n\nVítej v komunitě {product_name}.\n\n– Tým {product_name}"
	},
	"report_resolved": {
		"subject": "Tvé nahlášení pro {product_name} bylo zkontrolováno",
		"body": "Ahoj {username},\n\nTvé nahlášení (ID: {reportId}) bylo zkontrolováno naším bezpečnostním týmem.{hasComment, select, yes {\n\nOdpověď od bezpečnostního týmu:\n{publicComment}} other {}}\n\nDěkujeme, že pomáháš udržovat {product_name} bezpečný pro všechny. Všechna nahlášení bereme vážně a oceňujeme tvůj příspěvek komunitě.\n\nPokud máš jakékoli dotazy nebo obavy ohledně tohoto výsledku, kontaktuj {safety_email}.\n\n– Bezpečnostní tým {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Tvůj účet {product_name} bude trvale smazán",
		"body": "Ahoj {username},\n\nTvůj účet {product_name} byl naplánován k trvalému smazání.\n\nNaplánované smazání: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Důvod: {reason}}\n}\n\nJedná se o závažné opatření. Tvá data účtu budou trvale smazána v naplánovaný den.\n\nPokud se domníváš, že toto rozhodnutí o vynucení bylo nesprávné, můžeš podat odvolání. Odešli e-mail na {appeals_email} z této e-mailové adresy.\n\n– Bezpečnostní tým {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "Smazání tvého účtu {product_name} je naplánováno",
		"body": "Ahoj {username},\n\nPožádal jsi o smazání svého účtu {product_name}. Tvůj účet je naplánován k trvalému smazání na:\n\n{deletionDate, date, full} v {deletionDate, time, short}\n\nPokud jsi to nepožadoval, přihlas se ke svému účtu a zruš smazání. Doporučujeme také změnit si heslo pro zabezpečení účtu.\n\n– Tým {product_name}"
	},
	"unban_notification": {
		"subject": "Pozastavení tvého účtu {product_name} bylo zrušeno",
		"body": "Ahoj {username},\n\nDobré zprávy: pozastavení tvého účtu {product_name} bylo zrušeno.\n\n{reason, select,\n  null {}\n  other {Důvod: {reason}}\n}\n\nNyní se můžeš znovu přihlásit a pokračovat v používání {product_name} jako obvykle.\n\n– Bezpečnostní tým {product_name}"
	}
});

export default EMAIL_I18N_CS_MESSAGES;
