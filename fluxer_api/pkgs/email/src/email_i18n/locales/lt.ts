// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '@pkgs/email/src/email_i18n/EmailI18nMessages';

const EMAIL_I18N_LT_MESSAGES = defineEmailI18nLocaleMessages({
	"account_deletion_cancelled": {
		"subject": "Tavo {product_name} paskyros ištrynimas atšauktas",
		"body": "Sveiki, {username},\n\nNumatytas tavo {product_name} paskyros ištrynimas atšauktas. Tavo paskyra nebus ištrinta.\n\n{safety_email, select, null {Jei turi klausimų, susisiek su šios instancijos administratoriais.} other {Jei turi klausimų, rašyk adresu {safety_email}.}}\n\n– {product_name} komanda"
	},
	"account_deletion_scheduled_inactivity": {
		"subject": "Tavo {product_name} paskyra bus ištrinta dėl neaktyvumo",
		"body": "Sveiki, {username},\n\nTavo {product_name} paskyra ilgą laiką buvo neaktyvi, todėl ją numatyta visam laikui ištrinti:\n\n{deletionDate, date, full} {deletionDate, time, short}{reason, select, null {} other {\n\nPriežastis: {reason}}}\n\n{safety_email, select, null {Jei nori išsaugoti paskyrą, iki šios datos susisiek su šios instancijos administratoriais.} other {Jei nori išsaugoti paskyrą, iki šios datos parašyk adresu {safety_email} iš šio el. pašto adreso.}}\n\n– {product_name} komanda"
	},
	"account_deletion_scheduled_requested": {
		"subject": "Tavo {product_name} paskyros ištrynimas numatytas",
		"body": "Sveiki, {username},\n\nTavo prašymu {product_name} paskyrą numatyta visam laikui ištrinti:\n\n{deletionDate, date, full} {deletionDate, time, short}{reason, select, null {} other {\n\nPriežastis: {reason}}}\n\nIki tol tavo paskyra užrakinta. {safety_email, select, null {Jei to neprašei arba nori išsaugoti paskyrą, iki šios datos susisiek su šios instancijos administratoriais.} other {Jei to neprašei arba nori išsaugoti paskyrą, iki šios datos parašyk adresu {safety_email} iš šio el. pašto adreso.}}\n\n– {product_name} komanda"
	},
	"account_scheduled_deletion": {
		"subject": "Tavo {product_name} paskyra bus visam laikui ištrinta",
		"body": "Sveiki, {username},\n\nTavo {product_name} paskyrą numatyta visam laikui ištrinti dėl mūsų paslaugų teikimo sąlygų arba bendruomenės gairių pažeidimų.\n\nNumatytas ištrynimas: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}\n\n}\n}Tai rimta priemonė. Tavo paskyros duomenys numatytą dieną bus visam laikui ištrinti.\n\n{legalLinks, select,\n  both {Peržiūrėk:\n- Paslaugų teikimo sąlygos: {termsUrl}\n- Bendruomenės gairės: {guidelinesUrl}\n\n}\n  terms {Peržiūrėk:\n- Paslaugų teikimo sąlygos: {termsUrl}\n\n}\n  guidelines {Peržiūrėk:\n- Bendruomenės gairės: {guidelinesUrl}\n\n}\n  other {}\n}Apeliacijos procesas:\nJei manai, kad šis sprendimas neteisingas arba nepagrįstas, apeliaciją gali pateikti per 60 dienų. {appeals_email, select, null {Susisiek su šios instancijos administratoriais.} other {Iš šio el. pašto adreso siųsk el. laišką adresu {appeals_email}.}}\n\nSavo apeliacijoje:\n- Aiškiai paaiškink, kodėl manai, kad sprendimas neteisingas arba nepagrįstas\n- Pateik visus susijusius įrodymus ar kontekstą\n\n{product_name} saugos komandos narys peržiūrės tavo apeliaciją ir gali sustabdyti numatytą ištrynimą, kol bus priimtas galutinis sprendimas.\n\n– {product_name} saugos komanda"
	},
	"account_temp_banned": {
		"subject": "Tavo {product_name} paskyra laikinai sustabdyta",
		"body": "Sveiki, {username},\n\nTavo {product_name} paskyra laikinai sustabdyta dėl mūsų paslaugų teikimo sąlygų arba bendruomenės gairių pažeidimo.\n\nTrukmė: {durationHours, plural,\n  one {# valanda} few {# valandos}\n  many {# valandos} other {# valandų}\n}\nSustabdyta iki: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}\n\n}\n}Šiuo laikotarpiu negalėsi pasiekti savo paskyros.\n\n{legalLinks, select,\n  both {Peržiūrėk:\n- Paslaugų teikimo sąlygos: {termsUrl}\n- Bendruomenės gairės: {guidelinesUrl}\n\n}\n  terms {Peržiūrėk:\n- Paslaugų teikimo sąlygos: {termsUrl}\n\n}\n  guidelines {Peržiūrėk:\n- Bendruomenės gairės: {guidelinesUrl}\n\n}\n  other {}\n}Jei manai, kad šis sprendimas neteisingas arba nepagrįstas, gali pateikti apeliaciją. {appeals_email, select, null {Susisiek su šios instancijos administratoriais ir aiškiai paaiškink, kodėl manai, kad sprendimas neteisingas.} other {Iš šio el. pašto adreso siųsk el. laišką adresu {appeals_email} ir aiškiai paaiškink, kodėl manai, kad sprendimas neteisingas.}} Peržiūrėsime tavo apeliaciją ir pranešime savo sprendimą.\n\n– {product_name} saugos komanda"
	},
	"donation_confirmation": {
		"subject": "Dėkojame už tavo {product_name} paramą",
		"body": "Sveiki,\n\nDėkojame už tavo paramą {product_name}! Tavo {interval, select,\n  month {pasikartojanti parama}\n  year {pasikartojanti parama}\n  other {vienkartinė parama}\n} sėkmingai {interval, select,\n  month {nustatyta}\n  year {nustatyta}\n  other {apdorota}\n}.\n\nParamos informacija:\nSuma: {amount} {interval, select,\n  month {per mėnesį}\n  year {per metus}\n  other {}\n}\n\nStripe netrukus atsiųs tau atskirą kvitą su sąskaitos faktūros PDF. Jame nurodyta visa mokėjimo informacija ir jį galima naudoti mokesčių tikslais.\n\nBet kuriuo metu šia nuoroda gali peržiūrėti savo paramos istoriją, atsisiųsti sąskaitas faktūras {interval, select,\n  month {ir valdyti arba atšaukti savo prenumeratą}\n  year {ir valdyti arba atšaukti savo prenumeratą}\n  other {ir valdyti būsimą paramą}\n}:\n\n{manageUrl}\n\nTavo parama padeda {product_name} veikti. Ačiū!\n\n– {product_name} komanda"
	},
	"donation_magic_link": {
		"subject": "Tvarkyk savo {product_name} paramą",
		"body": "Sveiki,\n\nSpustelėk žemiau esančią nuorodą, kad pasiektum savo rėmėjo portalą:\n\n{manageUrl}\n\nPortale gali valdyti prenumeratas, atsisiųsti sąskaitas faktūras ir peržiūrėti savo paramos istoriją.\n\nŠi nuoroda galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei neprašei šios nuorodos, gali saugiai ignoruoti šį el. laišką.\n\n– {product_name} komanda"
	},
	"dsa_report_resolved": {
		"subject": "Priėmėme sprendimą dėl tavo {product_name} pranešimo",
		"body": "Sveiki,\n\nMūsų saugos komanda peržiūrėjo tavo pranešimą pagal Skaitmeninių paslaugų aktą (ID: {reportId}) ir priėmė sprendimą.{hasComment, select, yes {\n\nSaugos komandos atsakymas:\n{publicComment}} other {}}\n\nNesidalijame informacija apie veiksmus, kurių imtasi dėl kito asmens paskyros, nes tai yra jo asmens duomenys.\n\nJei nesutinki su šiuo sprendimu, per 60 dienų gali nemokamai pateikti apeliaciją. {appeals_email, select, null {Susisiek su šios instancijos administratoriais, nurodyk savo pranešimo ID ir paaiškink, kodėl manai, kad sprendimas neteisingas.} other {Iš šio el. pašto adreso siųsk el. laišką adresu {appeals_email}, nurodyk savo pranešimo ID ir paaiškink, kodėl manai, kad sprendimas neteisingas.}} Jei esi ES, ginčą taip pat gali perduoti sertifikuotai neteisminio ginčų sprendimo įstaigai. Visa tai neturi įtakos tavo teisei kreiptis į teismą.\n\n– {product_name} saugos komanda"
	},
	"dsa_report_verification": {
		"subject": "Patvirtink savo el. paštą DSA pranešimui pateikti",
		"body": "Sveiki,\n\nNaudok žemiau esantį patvirtinimo kodą, kad {product_name} platformoje pateiktum pranešimą pagal Skaitmeninių paslaugų aktą:\n\n{code}\n\nŠis kodas galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei to neprašei, gali ignoruoti šį el. laišką.\n\n– {product_name} saugos komanda"
	},
	"email_change_new": {
		"subject": "Patvirtink savo naują {product_name} el. paštą",
		"body": "Sveiki, {username},\n\nĮvesk šį kodą programėlėje, kad patvirtintum savo naują {product_name} el. paštą:\n\n{code}\n\nŠis kodas galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei to neprašei, gali ignoruoti šį el. laišką.\n\n– {product_name} komanda"
	},
	"email_change_original": {
		"subject": "Patvirtink savo {product_name} el. pašto keitimą",
		"body": "Sveiki, {username},\n\nGavome prašymą pakeisti tavo {product_name} paskyros el. pašto adresą.\n\nKad patvirtintum šį pakeitimą, įvesk šį kodą programėlėje:\n\n{code}\n\nŠis kodas galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei to neprašei, nedelsiant apsaugok savo paskyrą.\n\n– {product_name} komanda"
	},
	"email_change_revert": {
		"subject": "Tavo {product_name} el. paštas buvo pakeistas",
		"body": "Sveiki, {username},\n\nTavo {product_name} paskyros el. pašto adresas pakeistas į {newEmail}.\n\nJei šį pakeitimą atlikai tu, nieko daryti nereikia. Jei ne, šia nuoroda gali atšaukti pakeitimą ir apsaugoti savo paskyrą:\n\n{revertUrl}\n\nTai atkurs ankstesnį el. pašto adresą, atjungs tave visuose įrenginiuose, išjungs MFA ir pareikalaus nustatyti naują slaptažodį.\n\n– {product_name} saugos komanda"
	},
	"email_verification": {
		"subject": "Patvirtink savo {product_name} el. pašto adresą",
		"body": "Sveiki, {username},\n\nSpustelėk žemiau esančią nuorodą ir patvirtink savo {product_name} paskyros el. pašto adresą:\n\n{verifyUrl}\n\nJei {product_name} paskyros nekūrei, gali saugiai ignoruoti šį el. laišką.\n\nŠi nuoroda galioja 24 valandas.\n\n– {product_name} komanda"
	},
	"gift_chargeback_notification": {
		"subject": "Privilegijos, gautos už išpirktą dovaną, pašalintos",
		"body": "Sveiki, {username},\n\nDovanos kodą, kurį išpirkai, iš pradžių apmokėjo kitas asmuo. Tas mokėjimas vėliau buvo atšauktas (lėšų grąžinimas).\n\nDėl to pašalinome privilegijas, kurios buvo pridėtos prie tavo paskyros, kai išpirkai dovaną.\n\n{support_email, select, null {Jei manai, kad tai klaida, susisiek su šios instancijos administratoriais ir nurodyk visą turimą informaciją apie dovanos kodą ir apie tai, kada jį išpirkai.} other {Jei manai, kad tai klaida, susisiek su mūsų palaikymo komanda ir nurodyk visą turimą informaciją apie dovanos kodą ir apie tai, kada jį išpirkai.}}\n\n– {product_name} komanda"
	},
	"harvest_completed": {
		"subject": "Tavo {product_name} duomenų eksportas paruoštas atsisiųsti",
		"body": "Sveiki, {username},\n\nTavo duomenų eksportas paruoštas.\n\nAtsisiuntimo nuoroda:\n{downloadUrl}\n\nĮtrauktos žinutės: {totalMessages, number}\nFailo dydis: {fileSizeMB, number} MB\n\nŠi nuoroda galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\n{support_email, select, null {Jei šio eksporto neprašei, nedelsiant pakeisk slaptažodį ir susisiek su šios instancijos administratoriais.} other {Jei šio eksporto neprašei, nedelsiant pakeisk slaptažodį ir susisiek su mūsų palaikymo komanda.}}\n\n– {product_name} komanda"
	},
	"inactivity_warning": {
		"subject": "Tavo {product_name} paskyra bus ištrinta dėl neaktyvumo",
		"body": "Sveiki, {username},\n\nNuo {lastActiveDate, date, full} tavo {product_name} paskyroje nematėme jokios veiklos.\n\nJei neprisijungsi iki {deletionDate, date, full} {deletionDate, time, short}, tavo paskyra bus visam laikui ištrinta dėl neaktyvumo.\n\nPrisijunk čia:\n{loginUrl}\n\n{support_email, select, null {Jei neseniai naudojaisi {product_name}, nedelsiant susisiek su šios instancijos administratoriais.} other {Jei neseniai naudojaisi {product_name}, nedelsiant susisiek su mūsų palaikymo komanda.}}\n\n– {product_name} komanda"
	},
	"ip_authorization": {
		"subject": "Patvirtink prisijungimą iš naujo IP adreso",
		"body": "Sveiki, {username},\n\nAptikome bandymą prisijungti prie tavo {product_name} paskyros iš naujo IP adreso:\n\nIP adresas: {ipAddress}\nVieta: {location}\n\nJei tai buvai tu, spustelėk žemiau esančią nuorodą ir patvirtink šį IP adresą:\n\n{authUrl}\n\nJei prisijungti nebandei, nedelsiant pakeisk slaptažodį.\n\nŠi nuoroda galioja 30 minučių.\n\n– {product_name} komanda"
	},
	"mfa_backup_codes_view": {
		"subject": "Patvirtink prieigą prie savo {product_name} atsarginių kodų",
		"body": "Sveiki, {username},\n\nGavome prašymą peržiūrėti tavo {product_name} paskyros atsarginius kodus.\n\nKad patvirtintum šį prašymą, įvesk šį kodą programėlėje:\n\n{code}\n\nŠis kodas galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei to neprašei, kažkas gali turėti prieigą prie tavo paskyros. Nedelsiant pakeisk slaptažodį.\n\n– {product_name} komanda"
	},
	"password_change_verification": {
		"subject": "Patvirtink savo {product_name} slaptažodžio keitimą",
		"body": "Sveiki, {username},\n\nGavome prašymą pakeisti tavo {product_name} paskyros slaptažodį.\n\nKad patvirtintum šį pakeitimą, įvesk šį kodą programėlėje:\n\n{code}\n\nŠis kodas galioja iki {expiresAt, date, full} {expiresAt, time, short}.\n\nJei to neprašei, kažkas gali turėti prieigą prie tavo paskyros. Nedelsiant pakeisk slaptažodį ir įjunk dviejų veiksnių autentifikavimą.\n\n– {product_name} komanda"
	},
	"password_reset": {
		"subject": "Iš naujo nustatyk savo {product_name} slaptažodį",
		"body": "Sveiki, {username},\n\nPaprašei iš naujo nustatyti {product_name} slaptažodį. Naudok žemiau esančią nuorodą, kad nustatytum naują slaptažodį:\n\n{resetUrl}\n\nJei to neprašei, gali saugiai ignoruoti šį el. laišką.\n\nŠi nuoroda galioja 1 valandą.\n\n– {product_name} komanda"
	},
	"registration_approved": {
		"subject": "Tavo {product_name} registracija patvirtinta",
		"body": "Sveiki, {username},\n\nGeros naujienos: tavo {product_name} registracija patvirtinta.\n\nDabar gali prisijungti prie {product_name} programėlės čia:\n{channelsUrl}\n\nSveiki atvykę į {product_name} bendruomenę.\n\n– {product_name} komanda"
	},
	"report_received": {
		"subject": "Gavome tavo {product_name} pranešimą",
		"body": "Sveiki,\n\nGavome tavo pranešimą pagal Skaitmeninių paslaugų aktą apie {targetKind, select, message {žinutę} user {paskyrą} guild {bendruomenę} other {turinį}} {product_name} platformoje.\n\nPranešimo ID: {reportId}\n\nMūsų saugos komanda peržiūrės tavo pranešimą, o priėmę sprendimą parašysime tau šiuo adresu. Išsaugok šį el. laišką, kad turėtum jį ateičiai.\n\n– {product_name} saugos komanda"
	},
	"report_resolved": {
		"subject": "Tavo {product_name} pranešimas peržiūrėtas",
		"body": "Sveiki, {username},\n\nTavo pranešimą (ID: {reportId}) peržiūrėjo mūsų saugos komanda.{hasComment, select, yes {\n\nSaugos komandos atsakymas:\n{publicComment}} other {}}\n\nDėkojame, kad padedi užtikrinti {product_name} saugumą visiems. Visus pranešimus vertiname rimtai ir esame dėkingi už tavo indėlį į bendruomenę.\n\n{safety_email, select, null {Jei turi klausimų ar abejonių dėl šio sprendimo, susisiek su šios instancijos administratoriais.} other {Jei turi klausimų ar abejonių dėl šio sprendimo, rašyk adresu {safety_email}.}}\n\n– {product_name} saugos komanda"
	},
	"scheduled_deletion_notification": {
		"subject": "Tavo {product_name} paskyra bus visam laikui ištrinta",
		"body": "Sveiki, {username},\n\nTavo {product_name} paskyrą numatyta visam laikui ištrinti.\n\nNumatytas ištrynimas: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}\n\n}\n}Tai rimta priemonė. Tavo paskyros duomenys numatytą dieną bus visam laikui ištrinti.\n\nJei manai, kad šis sprendimas neteisingas, gali pateikti apeliaciją. {appeals_email, select, null {Susisiek su šios instancijos administratoriais.} other {Iš šio el. pašto adreso siųsk el. laišką adresu {appeals_email}.}}\n\n– {product_name} saugos komanda"
	},
	"self_deletion_scheduled": {
		"subject": "Tavo {product_name} paskyros ištrynimas numatytas",
		"body": "Sveiki, {username},\n\nPaprašei ištrinti savo {product_name} paskyrą. Tavo paskyrą numatyta visam laikui ištrinti:\n\n{deletionDate, date, full} {deletionDate, time, short}\n\nJei to neprašei, prisijunk prie savo paskyros ir atšauk ištrynimą. Taip pat rekomenduojame pakeisti slaptažodį, kad apsaugotum savo paskyrą.\n\n– {product_name} komanda"
	},
	"unban_notification": {
		"subject": "Tavo {product_name} paskyros sustabdymas panaikintas",
		"body": "Sveiki, {username},\n\nGeros naujienos: tavo {product_name} paskyros sustabdymas panaikintas.\n\n{reason, select,\n  null {}\n  other {Priežastis: {reason}\n\n}\n}Dabar gali vėl prisijungti ir toliau naudotis {product_name} kaip įprasta.\n\n– {product_name} saugos komanda"
	}
});

export default EMAIL_I18N_LT_MESSAGES;
