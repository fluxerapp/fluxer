// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_HR_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Tvoj {product_name} račun je privremeno onemogućen",
		"body": "Pozdrav {username},\n\nPrivremeno smo onemogućili tvoj {product_name} račun jer smo otkrili sumnjivu aktivnost.\n\n{reason, select,\n  null {}\n  other {Razlog: {reason}}\n}\n\nDa bi ponovno pristupio svom računu, morat ćeš poništiti lozinku:\n\n{forgotUrl}\n\nNakon što poništiš lozinku, moći ćeš se ponovno prijaviti.\n\nAko misliš da je ovo pogreška, kontaktiraj naš tim za podršku.\n\n– {product_name} Sigurnosni Tim"
	},
	"account_scheduled_deletion": {
		"subject": "Tvoj {product_name} račun bit će trajno izbrisan",
		"body": "Pozdrav {username},\n\nTvoj {product_name} račun je zakazan za trajno brisanje zbog kršenja naših Uvjeta korištenja ili Smjernica zajednice.\n\nZakazano brisanje: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Razlog: {reason}}\n}\n\nOvo je ozbiljna mjera. Podaci tvog računa bit će trajno izbrisani na zakazani datum.\n\nPregledaj:\n- Uvjeti korištenja: {termsUrl}\n- Smjernice zajednice: {guidelinesUrl}\n\nPostupak žalbe:\nAko smatraš da je ova odluka bila netočna ili neopravdana, imaš 60 dana za podnošenje žalbe. Pošalji e-poštu na {appeals_email} s ove adrese e-pošte.\n\nU svojoj žalbi:\n- Jasno objasni zašto smatraš da je odluka bila netočna ili neopravdana\n- Pruži sve relevantne dokaze ili kontekst\n\nČlan {product_name} sigurnosnog tima pregledat će tvoju žalbu i može zaustaviti brisanje dok se ne donese konačna odluka.\n\n– {product_name} Sigurnosni Tim"
	},
	"account_temp_banned": {
		"subject": "Tvoj {product_name} račun je privremeno suspendiran",
		"body": "Pozdrav {username},\n\nTvoj {product_name} račun je privremeno suspendiran zbog kršenja naših Uvjeta korištenja ili Smjernica zajednice.\n\nTrajanje: {durationHours, plural,\n  =1 {1 sat} few {sata}\n  other {# sati}\n}\nSuspendiran do: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Razlog: {reason}}\n}\n\nTijekom tog vremena nećeš moći pristupiti svom računu.\n\nPregledaj:\n- Uvjeti korištenja: {termsUrl}\n- Smjernice zajednice: {guidelinesUrl}\n\nAko smatraš da je ova odluka bila netočna ili neopravdana, možeš podnijeti žalbu. Pošalji e-poštu na {appeals_email} s ove adrese e-pošte i jasno objasni zašto smatraš da je odluka bila netočna. Pregledat ćemo tvoju žalbu i odgovoriti s našom odlukom.\n\n– {product_name} Sigurnosni Tim"
	},
	"donation_confirmation": {
		"subject": "Hvala ti na donaciji za {product_name}",
		"body": "Pozdrav,\n\nHvala ti na donaciji za {product_name}! Tvoja {interval, select,\n  month {redovita donacija}\n  year {redovita donacija}\n  other {jednokratna donacija}\n} je uspješno {interval, select,\n  month {postavljena}\n  year {postavljena}\n  other {obrađena}\n}.\n\nDetalji donacije:\nIznos: {amount} {currency} {interval, select,\n  month {mjesečno}\n  year {godišnje}\n  other {}\n}\n\nStripe će ti uskoro poslati zasebnu potvrdu s PDF-om računa. Ovo uključuje sve detalje plaćanja i može se koristiti u porezne svrhe.\n\nMožeš pregledati povijest donacija, preuzeti račune, {interval, select,\n  month {i upravljati ili otkazati svoju pretplatu}\n  year {i upravljati ili otkazati svoju pretplatu}\n  other {i upravljati budućim donacijama}\n} u bilo kojem trenutku koristeći ovu poveznicu:\n\n{manageUrl}\n\nTvoja podrška pomaže {product_name} da nastavi s radom. Hvala ti!\n\n– {product_name} Tim"
	},
	"donation_magic_link": {
		"subject": "Upravljanje donacijama za {product_name}",
		"body": "Pozdrav,\n\nKlikni na poveznicu ispod za pristup svom portalu za donatore:\n\n{manageUrl}\n\nNa portalu možeš upravljati pretplatama, preuzimati račune i pregledavati povijest svojih donacija.\n\nOva poveznica istječe {expiresAt, date, full} u {expiresAt, time, short}.\n\nAko nisi zatražio ovu poveznicu, možeš sigurno zanemariti ovu e-poštu.\n\n– {product_name} Tim"
	},
	"dsa_report_verification": {
		"subject": "Potvrdi svoju e-poštu za izvješće o Zakonu o digitalnim uslugama",
		"body": "Pozdrav,\n\nKoristi kod za provjeru u nastavku za podnošenje izvješća o Zakonu o digitalnim uslugama na {product_name}:\n\n{code}\n\nOvaj kod istječe {expiresAt, date, full} u {expiresAt, time, short}.\n\nAko ovo nisi zatražio, možeš zanemariti ovu e-poštu.\n\n– {product_name} Sigurnosni Tim"
	},
	"email_change_new": {
		"subject": "Potvrdi svoju novu e-poštu za {product_name}",
		"body": "Pozdrav {username},\n\nUnesi ovaj kod u aplikaciju kako bi potvrdio svoju novu {product_name} e-poštu:\n\n{code}\n\nOvaj kod istječe {expiresAt, date, full} u {expiresAt, time, short}.\n\nAko ovo nisi zatražio, možeš zanemariti ovu e-poštu.\n\n– {product_name} Tim"
	},
	"email_change_original": {
		"subject": "Potvrdi promjenu e-pošte za {product_name}",
		"body": "Pozdrav {username},\n\nPrimili smo zahtjev za promjenu adrese e-pošte na tvom {product_name} računu.\n\nDa bi potvrdio ovu promjenu, unesi ovaj kod u aplikaciju:\n\n{code}\n\nOvaj kod istječe {expiresAt, date, full} u {expiresAt, time, short}.\n\nAko ovo nisi zatražio, odmah zaštiti svoj račun.\n\n– {product_name} Tim"
	},
	"email_change_revert": {
		"subject": "Tvoja e-pošta za {product_name} je promijenjena",
		"body": "Pozdrav {username},\n\nAdresa e-pošte na tvom {product_name} računu promijenjena je u {newEmail}.\n\nAko si napravio ovu promjenu, nije potrebna nikakva radnja. Ako nisi, možeš poništiti promjenu i zaštititi svoj račun koristeći ovu poveznicu:\n\n{revertUrl}\n\nOvo će vratiti tvoju prethodnu e-poštu, odjaviti te svugdje, ukloniti povezane telefonske brojeve, onemogućiti višefaktorsku autentifikaciju i zahtijevati postavljanje nove lozinke.\n\n– {product_name} Sigurnosni Tim"
	},
	"email_verification": {
		"subject": "Potvrdi svoju e-poštu za {product_name}",
		"body": "Pozdrav {username},\n\nPotvrdi adresu e-pošte za svoj {product_name} račun klikom na poveznicu ispod:\n\n{verifyUrl}\n\nAko nisi stvorio {product_name} račun, možeš zanemariti ovu e-poštu.\n\nOva poveznica vrijedi 24 sata.\n\n– {product_name} Tim"
	},
	"gift_chargeback_notification": {
		"subject": "Pogodnosti tvog iskorištenog poklona su uklonjene",
		"body": "Pozdrav {username},\n\nPoklon kod koji si iskoristio izvorno je platio netko drugi. To plaćanje je u međuvremenu poništeno (povrat sredstava).\n\nZbog toga smo uklonili pogodnosti koje su dodane tvom računu kada si iskoristio poklon.\n\nAko misliš da je ovo pogreška, kontaktiraj naš tim za podršku i uključi sve detalje o poklon kodu i kada si ga iskoristio.\n\n– {product_name} Tim"
	},
	"harvest_completed": {
		"subject": "Tvoj izvoz podataka za {product_name} je spreman za preuzimanje",
		"body": "Pozdrav {username},\n\nTvoj izvoz podataka je spreman.\n\nPoveznica za preuzimanje:\n{downloadUrl}\n\nUključene poruke: {totalMessages, number}\nVeličina datoteke: {fileSizeMB, number} MB\n\nOva poveznica istječe {expiresAt, date, full} u {expiresAt, time, short}.\n\nAko nisi zatražio ovaj izvoz, odmah promijeni lozinku i kontaktiraj naš tim za podršku.\n\n– {product_name} Tim"
	},
	"inactivity_warning": {
		"subject": "Tvoj {product_name} račun bit će izbrisan zbog neaktivnosti",
		"body": "Pozdrav {username},\n\nNismo primijetili nikakvu aktivnost na tvom {product_name} računu od {lastActiveDate, date, full}.\n\nAko se ne prijaviš do {deletionDate, date, full} u {deletionDate, time, short}, tvoj račun bit će trajno izbrisan zbog neaktivnosti.\n\nPrijavi se ovdje:\n{loginUrl}\n\nAko si nedavno koristio {product_name}, odmah kontaktiraj naš tim za podršku.\n\n– {product_name} Tim"
	},
	"ip_authorization": {
		"subject": "Autoriziraj prijavu s nove IP adrese",
		"body": "Pozdrav {username},\n\nOtkrili smo pokušaj prijave na tvoj {product_name} račun s nove IP adrese:\n\nIP adresa: {ipAddress}\nLokacija: {location}\n\nAko si to bio ti, potvrdi ovu IP adresu klikom na poveznicu ispod:\n\n{authUrl}\n\nAko se nisi ti pokušao prijaviti, odmah promijeni lozinku.\n\nOva poveznica vrijedi 30 minuta.\n\n– {product_name} Tim"
	},
	"password_change_verification": {
		"subject": "Potvrdi promjenu lozinke za {product_name}",
		"body": "Pozdrav {username},\n\nPrimili smo zahtjev za promjenu lozinke na tvom {product_name} računu.\n\nDa bi potvrdio ovu promjenu, unesi ovaj kod u aplikaciju:\n\n{code}\n\nOvaj kod vrijedi do {expiresAt}.\n\nAko ovo nisi zatražio, netko je možda dobio pristup tvom računu. Odmah promijeni lozinku i omogući dvofaktorsku autentifikaciju.\n\n– {product_name} Tim"
	},
	"password_reset": {
		"subject": "Resetiraj svoju lozinku za {product_name}",
		"body": "Pozdrav {username},\n\nZatražio si poništavanje {product_name} lozinke. Koristi poveznicu ispod za postavljanje nove lozinke:\n\n{resetUrl}\n\nAko ovo nisi zatražio, možeš zanemariti ovu e-poštu.\n\nOva poveznica vrijedi 1 sat.\n\n– {product_name} Tim"
	},
	"registration_approved": {
		"subject": "Tvoja registracija za {product_name} je odobrena",
		"body": "Pozdrav {username},\n\nDobre vijesti: tvoja {product_name} registracija je odobrena.\n\nSada se možeš prijaviti u {product_name} aplikaciju ovdje:\n{channelsUrl}\n\nDobrodošao u {product_name} zajednicu.\n\n– {product_name} Tim"
	},
	"report_resolved": {
		"subject": "Tvoja prijava na {product_name} je pregledana",
		"body": "Pozdrav {username},\n\nTvoje izvješće (ID: {reportId}) pregledao je naš Sigurnosni tim.{hasComment, select, yes {\n\nOdgovor Sigurnosnog tima:\n{publicComment}} other {}}\n\nHvala ti što pomažeš da {product_name} bude siguran za sve. Sve prijave shvaćamo ozbiljno i cijenimo tvoj doprinos zajednici.\n\nAko imaš bilo kakvih pitanja ili nedoumica u vezi s ovim rezultatom, kontaktiraj {safety_email}.\n\n– {product_name} Sigurnosni Tim"
	},
	"scheduled_deletion_notification": {
		"subject": "Tvoj {product_name} račun bit će trajno izbrisan",
		"body": "Pozdrav {username},\n\nTvoj {product_name} račun je zakazan za trajno brisanje.\n\nZakazano brisanje: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Razlog: {reason}}\n}\n\nOvo je ozbiljna mjera. Podaci tvog računa bit će trajno izbrisani na zakazani datum.\n\nAko smatraš da je ova odluka o provedbi bila netočna, možeš podnijeti žalbu. Pošalji e-poštu na {appeals_email} s ove adrese e-pošte.\n\n– {product_name} Sigurnosni Tim"
	},
	"self_deletion_scheduled": {
		"subject": "Brisanje tvog računa za {product_name} je zakazano",
		"body": "Pozdrav {username},\n\nZatražio si brisanje svog {product_name} računa. Tvoj račun je zakazan za trajno brisanje:\n\n{deletionDate, date, full} u {deletionDate, time, short}\n\nAko ovo nisi zatražio, prijavi se na svoj račun kako bi otkazao brisanje. Također preporučujemo da promijeniš lozinku kako bi osigurao svoj račun.\n\n– {product_name} Tim"
	},
	"unban_notification": {
		"subject": "Suspenzija tvog računa za {product_name} je ukinuta",
		"body": "Pozdrav {username},\n\nDobre vijesti: suspenzija tvog {product_name} računa je ukinuta.\n\n{reason, select,\n  null {}\n  other {Razlog: {reason}}\n}\n\nSada se možeš ponovno prijaviti i nastaviti koristiti {product_name} kao i obično.\n\n– {product_name} Sigurnosni Tim"
	}
});

export default EMAIL_I18N_HR_MESSAGES;
