// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_LT_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Tavo {product_name} paskyra laikinai išjungta",
		"body": "Sveikas, {username},\n\nLaikinai išjungėme tavo {product_name} paskyrą, nes aptikome įtartiną veiklą.\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}}\n}\n\nNorėdamas atgauti prieigą prie paskyros, turėsi iš naujo nustatyti slaptažodį:\n\n{forgotUrl}\n\nNustatęs slaptažodį iš naujo, galėsi vėl prisijungti.\n\nJei manai, kad tai įvyko per klaidą, susisiek su mūsų palaikymo komanda.\n\n– {product_name} saugos komanda"
	},
	"account_scheduled_deletion": {
		"subject": "Tavo {product_name} paskyra bus visam laikui ištrinta",
		"body": "Sveikas, {username},\n\nTavo {product_name} paskyra numatyta visam laikui ištrinti dėl mūsų paslaugų teikimo sąlygų arba bendruomenės gairių pažeidimų.\n\nNumatytas ištrynimas: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}}\n}\n\nTai yra rimtas vykdymo veiksmas. Tavo paskyros duomenys bus visam laikui ištrinti numatytą dieną.\n\nPeržiūrėk:\n- Paslaugų teikimo sąlygos: {termsUrl}\n- Bendruomenės gairės: {guidelinesUrl}\n\nApeliacijos procesas:\nJei manai, kad šis vykdymo sprendimas buvo neteisingas arba nepagrįstas, turi 60 dienų pateikti apeliaciją. Iš šio el. pašto adreso siųsk el. laišką adresu {appeals_email}.\n\nSavo apeliacijoje:\n- Aiškiai paaiškink, kodėl manai, kad vykdymo sprendimas buvo neteisingas arba nepagrįstas\n- Pateik visus susijusius įrodymus ar kontekstą\n\n{product_name} saugos komandos narys peržiūrės tavo apeliaciją ir gali sustabdyti laukiantį ištrynimą, kol bus priimtas galutinis sprendimas.\n\n– {product_name} saugos komanda"
	},
	"account_temp_banned": {
		"subject": "Tavo {product_name} paskyra laikinai sustabdyta",
		"body": "Sveikas, {username},\n\nTavo {product_name} paskyra laikinai sustabdyta dėl mūsų paslaugų teikimo sąlygų arba bendruomenės gairių pažeidimo.\n\nTrukmė: {durationHours, plural,\n  one {valandÄ} few {valandas}\n  many {valandas} other {# valandų}\n}\nSustabdyta iki: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}}\n}\n\nŠiuo laikotarpiu negalėsi prisijungti prie savo paskyros.\n\nPeržiūrėk:\n- Paslaugų teikimo sąlygos: {termsUrl}\n- Bendruomenės gairės: {guidelinesUrl}\n\nJei manai, kad šis vykdymo sprendimas buvo neteisingas arba nepagrįstas, gali pateikti apeliaciją. Iš šio el. pašto adreso siųsk el. laišką adresu {appeals_email} ir aiškiai paaiškink, kodėl manai, kad sprendimas buvo neteisingas. Peržiūrėsime tavo apeliaciją ir atsakysime su savo sprendimu.\n\n– {product_name} saugos komanda saugos komanda"
	},
	"donation_confirmation": {
		"subject": "Dėkojame už tavo {product_name} paramą",
		"body": "Sveikas,\n\nDėkojame už tavo paramą {product_name}! Tavo {interval, select,\n  month {pasikartojanti parama}\n  year {pasikartojanti parama}\n  other {vienkartinė parama}\n} buvo {interval, select,\n  month {nustatyta}\n  year {nustatyta}\n  other {apdorota}\n} sėkmingai.\n\nParamos detalės:\nSuma: {amount} {currency} {interval, select,\n  month {per mėnesį}\n  year {per metus}\n  other {}\n}\n\nStripe netrukus atsiųs tau atskirą kvitą su sąskaitos faktūros PDF. Tai apima visą mokėjimo informaciją ir gali būti naudojama mokesčių tikslais.\n\nGali peržiūrėti savo paramos istoriją, atsisiųsti sąskaitas faktūras, {interval, select,\n  month {ir valdyti arba atšaukti savo prenumeratą}\n  year {ir valdyti arba atšaukti savo prenumeratą}\n  other {ir valdyti būsimą paramą}\n} bet kuriuo metu naudodamasis šia nuoroda:\n\n{manageUrl}\n\nTavo parama padeda {product_name} veikti. Ačiū!\n\n– {product_name} komanda"
	},
	"donation_magic_link": {
		"subject": "Tvarkyk savo {product_name} paramą",
		"body": "Sveikas,\n\nSpustelėk žemiau esančią nuorodą, kad pasiektum savo paramos portalą:\n\n{manageUrl}\n\nPortale gali valdyti prenumeratas, atsisiųsti sąskaitas faktūras ir peržiūrėti savo paramos istoriją.\n\nŠi nuoroda galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei neprašei šios nuorodos, gali saugiai ignoruoti šį el. laišką.\n\n– {product_name} komanda"
	},
	"dsa_report_verification": {
		"subject": "Patvirtink savo el. paštą dėl DSA ataskaitos",
		"body": "Sveikas,\n\nNaudok žemiau esantį patvirtinimo kodą, kad pateiktum savo Skaitmeninių paslaugų akto ataskaitą {product_name} platformoje:\n\n{code}\n\nŠis kodas galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei to neprašei, gali ignoruoti šį el. laišką.\n\n– {product_name} saugos komanda"
	},
	"email_change_new": {
		"subject": "Patvirtink savo naują {product_name} el. paštą",
		"body": "Sveikas, {username},\n\nĮvesk šį kodą programėlėje, kad patvirtintum savo naują {product_name} el. paštą:\n\n{code}\n\nŠis kodas galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei to neprašei, gali ignoruoti šį el. laišką.\n\n– {product_name} komanda"
	},
	"email_change_original": {
		"subject": "Patvirtink savo {product_name} el. pašto keitimą",
		"body": "Sveikas, {username},\n\nGavome prašymą pakeisti tavo {product_name} paskyros el. pašto adresą.\n\nNorėdamas patvirtinti šį pakeitimą, įvesk šį kodą programėlėje:\n\n{code}\n\nŠis kodas galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei to neprašei, nedelsdamas apsaugok savo paskyrą.\n\n– {product_name} komanda"
	},
	"email_change_revert": {
		"subject": "Tavo {product_name} el. paštas buvo pakeistas",
		"body": "Sveikas, {username},\n\nTavo {product_name} paskyros el. pašto adresas buvo pakeistas į {newEmail}.\n\nJei atlikai šį pakeitimą, jokių veiksmų nereikia. Jei ne, gali atšaukti pakeitimą ir apsaugoti savo paskyrą naudodamasis šia nuoroda:\n\n{revertUrl}\n\nTai atkurs tavo ankstesnį el. paštą, atsijungsi visur, pašalinsi susietus telefono numerius, išjungsi MFA ir reikalaus nustatyti naują slaptažodį.\n\n– {product_name} saugos komanda"
	},
	"email_verification": {
		"subject": "Patvirtink savo {product_name} el. pašto adresą",
		"body": "Sveikas, {username},\n\nPatvirtink savo {product_name} paskyros el. pašto adresą spustelėdamas žemiau esančią nuorodą:\n\n{verifyUrl}\n\nJei nesukūrei {product_name} paskyros, gali saugiai ignoruoti šį el. laišką.\n\nŠi nuoroda galioja 24 valandas.\n\n– {product_name} komanda"
	},
	"gift_chargeback_notification": {
		"subject": "Privalumai iš tavo išpirktos dovanos buvo pašalinti",
		"body": "Sveikas, {username},\n\nDovanos kodas, kurį išpirkai, iš pradžių buvo apmokėtas kito asmens. Tas mokėjimas vėliau buvo atšauktas (grąžinimas).\n\nDėl to pašalinome privalumus, kurie buvo pridėti prie tavo paskyros, kai išpirkai dovaną.\n\nJei manai, kad tai klaida, susisiek su mūsų palaikymo komanda ir pateik visą turimą informaciją apie dovanos kodą ir kada jį išpirkai.\n\n– {product_name} komanda"
	},
	"harvest_completed": {
		"subject": "Tavo {product_name} duomenų eksportas paruoštas atsisiųsti",
		"body": "Sveikas, {username},\n\nTavo duomenų eksportas paruoštas.\n\nAtsisiuntimo nuoroda:\n{downloadUrl}\n\nŽinutės: {totalMessages, number}\nFailo dydis: {fileSizeMB, number} MB\n\nŠi nuoroda galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei neprašei šio eksporto, nedelsdamas pakeisk slaptažodį ir susisiek su mūsų palaikymo komanda.\n\n– {product_name} komanda"
	},
	"inactivity_warning": {
		"subject": "Tavo {product_name} paskyra bus ištrinta dėl neveiklumo",
		"body": "Sveikas, {username},\n\nNematėme jokios veiklos tavo {product_name} paskyroje nuo {lastActiveDate, date, full}.\n\nJei neprisijungsi iki {deletionDate, date, full} {deletionDate, time, short}, tavo paskyra bus visam laikui ištrinta dėl neveiklumo.\n\nPrisijunk čia:\n{loginUrl}\n\nJei neseniai naudojai {product_name}, nedelsdamas susisiek su mūsų palaikymo komanda.\n\n– {product_name} komanda"
	},
	"ip_authorization": {
		"subject": "Leisti prisijungimą iš naujo IP adreso",
		"body": "Sveikas, {username},\n\nAptikome bandymą prisijungti prie tavo {product_name} paskyros iš naujo IP adreso:\n\nIP adresas: {ipAddress}\nVieta: {location}\n\nJei tai buvai tu, patvirtink šį IP adresą spustelėdamas žemiau esančią nuorodą:\n\n{authUrl}\n\nJei nebandei prisijungti, nedelsdamas pakeisk slaptažodį.\n\nŠi nuoroda galioja 30 minučių.\n\n– {product_name} komanda"
	},
	"password_change_verification": {
		"subject": "Patvirtink savo {product_name} slaptažodžio keitimą",
		"body": "Sveikas, {username},\n\nGavome prašymą pakeisti tavo {product_name} paskyros slaptažodį.\n\nNorėdamas patvirtinti šį pakeitimą, įvesk šį kodą programėlėje:\n\n{code}\n\nŠis kodas galioja iki {expiresAt}.\n\nJei to neprašei, kažkas gali turėti prieigą prie tavo paskyros. Nedelsdamas pakeisk slaptažodį ir įjunk dviejų veiksnių autentifikavimą.\n\n– {product_name} komanda"
	},
	"password_reset": {
		"subject": "Nustatyk iš naujo savo {product_name} slaptažodį",
		"body": "Sveikas, {username},\n\nTu paprašei iš naujo nustatyti {product_name} slaptažodį. Naudok žemiau esančią nuorodą, kad nustatytum naują slaptažodį:\n\n{resetUrl}\n\nJei to neprašei, gali saugiai ignoruoti šį el. laišką.\n\nŠi nuoroda galioja 1 valandą.\n\n– {product_name} komanda"
	},
	"registration_approved": {
		"subject": "Tavo {product_name} registracija patvirtinta",
		"body": "Sveikas, {username},\n\nGeros naujienos: tavo {product_name} registracija patvirtinta.\n\nDabar gali prisijungti prie {product_name} programėlės čia:\n{channelsUrl}\n\nSveikas atvykęs į {product_name} bendruomenę.\n\n– {product_name} komanda"
	},
	"report_resolved": {
		"subject": "Tavo {product_name} ataskaita buvo peržiūrėta",
		"body": "Sveikas, {username},\n\nTavo ataskaitą (ID: {reportId}) peržiūrėjo mūsų saugos komanda.{hasComment, select, yes {\n\nSaugos komandos atsakymas:\n{publicComment}} other {}}\n\nDėkojame, kad padedi užtikrinti {product_name} saugumą visiems. Visas ataskaitas vertiname rimtai ir vertiname tavo indėlį į bendruomenę.\n\nJei turi klausimų ar abejonių dėl šio rezultato, susisiek su {safety_email}.\n\n– {product_name} saugos komanda"
	},
	"scheduled_deletion_notification": {
		"subject": "Tavo {product_name} paskyra bus visam laikui ištrinta",
		"body": "Sveikas, {username},\n\nTavo {product_name} paskyra numatyta visam laikui ištrinti.\n\nNumatytas ištrynimas: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}}\n}\n\nTai yra rimtas vykdymo veiksmas. Tavo paskyros duomenys bus visam laikui ištrinti numatytą dieną.\n\nJei manai, kad šis vykdymo sprendimas buvo neteisingas, gali pateikti apeliaciją. Iš šio el. pašto adreso siųsk el. laišką adresu {appeals_email}.\n\n– {product_name} saugos komanda"
	},
	"self_deletion_scheduled": {
		"subject": "Tavo {product_name} paskyros ištrynimas suplanuotas",
		"body": "Sveikas, {username},\n\nTu paprašei ištrinti savo {product_name} paskyrą. Tavo paskyra numatyta visam laikui ištrinti:\n\n{deletionDate, date, full} {deletionDate, time, short}\n\nJei to neprašei, prisijunk prie savo paskyros, kad atšauktum ištrynimą. Taip pat rekomenduojame pakeisti slaptažodį, kad apsaugotum savo paskyrą.\n\n– {product_name} komanda"
	},
	"unban_notification": {
		"subject": "Tavo {product_name} paskyros sustabdymas panaikintas",
		"body": "Sveikas, {username},\n\nGeros naujienos: tavo {product_name} paskyros sustabdymas panaikintas.\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}}\n}\n\nDabar gali vėl prisijungti ir toliau naudotis {product_name} kaip įprasta.\n\n– {product_name} saugos komanda"
	}
});

export default EMAIL_I18N_LT_MESSAGES;
