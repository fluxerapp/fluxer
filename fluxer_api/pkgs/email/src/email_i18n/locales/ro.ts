// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_RO_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Contul tău {product_name} a fost dezactivat temporar",
		"body": "Salut {username},\n\nAm dezactivat temporar contul tău {product_name} deoarece am detectat activitate suspectă.\n\n{reason, select,\n  null {}\n  other {Motiv: {reason}}\n}\n\nPentru a recăpăta accesul la contul tău, va trebui să-ți resetezi parola:\n\n{forgotUrl}\n\nDupă ce îți resetezi parola, te vei putea conecta din nou.\n\nDacă crezi că aceasta a fost o eroare, te rugăm să contactezi echipa noastră de suport.\n\n– {product_name} Safety Team"
	},
	"account_scheduled_deletion": {
		"subject": "Contul tău de {product_name} va fi șters permanent",
		"body": "Salut {username},\n\nContul tău {product_name} a fost programat pentru ștergere permanentă din cauza încălcării Termenilor și Condițiilor sau a Ghidului Comunității noastre.\n\nȘtergere programată: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Motiv: {reason}}\n}\n\nAceasta este o măsură disciplinară serioasă. Datele contului tău vor fi șterse permanent la data programată.\n\nTe rugăm să revizuiești:\n- Termeni și Condiții: {termsUrl}\n- Ghidul Comunității: {guidelinesUrl}\n\nProcesul de apel:\nDacă crezi că această decizie de aplicare a fost incorectă sau nejustificată, ai la dispoziție 60 de zile pentru a depune un apel. Trimite un e-mail la {appeals_email} de la această adresă de e-mail.\n\nÎn apelul tău:\n- Explică clar de ce crezi că decizia de aplicare a fost incorectă sau nejustificată\n- Furnizează orice dovezi sau context relevante\n\nUn membru al Echipei de Siguranță {product_name} va examina apelul tău și poate întrerupe ștergerea în așteptare până la luarea unei decizii finale.\n\n– {product_name} Safety Team"
	},
	"account_temp_banned": {
		"subject": "Contul tău {product_name} a fost suspendat temporar",
		"body": "Salut {username},\n\nContul tău {product_name} a fost suspendat temporar pentru încălcarea Termenilor și Condițiilor sau a Ghidului Comunității noastre.\n\nDurată: {durationHours, plural,\n  one {1 oră} few {ore}\n  other {# de ore}\n}\nSuspendat până la: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Motiv: {reason}}\n}\n\nÎn acest timp, nu vei putea accesa contul tău.\n\nTe rugăm să revizuiești:\n- Termeni și Condiții: {termsUrl}\n- Ghidul Comunității: {guidelinesUrl}\n\nDacă crezi că această decizie a fost incorectă sau nejustificată, poți depune un apel. Trimite un e-mail la {appeals_email} de la această adresă de e-mail și explică clar de ce crezi că decizia a fost incorectă. Vom examina apelul tău și vom răspunde cu decizia noastră.\n\n– {product_name} Safety Team Safety Team"
	},
	"donation_confirmation": {
		"subject": "Îți mulțumim pentru donația ta pentru {product_name}",
		"body": "Salut,\n\nÎți mulțumim pentru donația ta către {product_name}! {interval, select,\n  month {Donația ta recurentă}\n  year {Donația ta recurentă}\n  other {Donația ta unică}\n} a fost {interval, select,\n  month {configurată}\n  year {configurată}\n  other {procesată}\n} cu succes.\n\nDetalii donație:\nSumă: {amount} {currency} {interval, select,\n  month {pe lună}\n  year {pe an}\n  other {}\n}\n\nStripe îți va trimite în scurt timp un e-mail separat cu chitanța ta în format PDF. Aceasta include toate detaliile plății și poate fi utilizată în scopuri fiscale.\n\nPoți vizualiza istoricul donațiilor tale, descărca facturi, {interval, select,\n  month {și gestiona sau anula abonamentul tău}\n  year {și gestiona sau anula abonamentul tău}\n  other {și gestiona donațiile viitoare}\n} în orice moment folosind acest link:\n\n{manageUrl}\n\nSuportul tău ajută la menținerea {product_name} în funcțiune. Mulțumim!\n\n– Echipa {product_name}"
	},
	"donation_magic_link": {
		"subject": "Gestionează-ți donațiile pentru {product_name}",
		"body": "Salut,\n\nApasă pe linkul de mai jos pentru a accesa portalul tău de donator:\n\n{manageUrl}\n\nÎn portal, poți gestiona abonamentele, descărca facturi și vizualiza istoricul donațiilor tale.\n\nAcest link expiră la data de {expiresAt, date, full} la ora {expiresAt, time, short}.\n\nDacă nu ai solicitat acest link, poți ignora în siguranță acest e-mail.\n\n– Echipa {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Verifică-ți e-mailul pentru un raport DSA",
		"body": "Salut,\n\nFolosește codul de verificare de mai jos pentru a trimite raportul tău privind Legea Serviciilor Digitale pe {product_name}:\n\n{code}\n\nAcest cod expiră la data de {expiresAt, date, full} la ora {expiresAt, time, short}.\n\nDacă nu ai solicitat acest lucru, poți ignora acest e-mail.\n\n– Echipa de Siguranță a {product_name}"
	},
	"email_change_new": {
		"subject": "Verifică-ți noul e-mail pentru {product_name}",
		"body": "Salut {username},\n\nIntrodu acest cod în aplicație pentru a-ți verifica noul e-mail pentru {product_name}:\n\n{code}\n\nAcest cod expiră la data de {expiresAt, date, full} la ora {expiresAt, time, short}.\n\nDacă nu ai solicitat acest lucru, poți ignora acest e-mail.\n\n– Echipa {product_name}"
	},
	"email_change_original": {
		"subject": "Confirmă schimbarea adresei de e-mail pentru {product_name}",
		"body": "Salut {username},\n\nAm primit o solicitare de schimbare a adresei de e-mail a contului tău {product_name}.\n\nPentru a confirma această modificare, introdu acest cod în aplicație:\n\n{code}\n\nAcest cod expiră la data de {expiresAt, date, full} la ora {expiresAt, time, short}.\n\nDacă nu ai solicitat acest lucru, te rugăm să-ți securizezi contul imediat.\n\n– Echipa {product_name}"
	},
	"email_change_revert": {
		"subject": "Adresa de e-mail pentru {product_name} a fost schimbată",
		"body": "Salut {username},\n\nAdresa de e-mail a contului tău {product_name} a fost schimbată în {newEmail}.\n\nDacă tu ai făcut această modificare, nu este necesară nicio acțiune. Dacă nu tu ai făcut-o, poți anula modificarea și securiza contul tău folosind acest link:\n\n{revertUrl}\n\nAceasta va restabili e-mailul tău anterior, te va deconecta de peste tot, va elimina numerele de telefon conectate, va dezactiva MFA și îți va cere să setezi o nouă parolă.\n\n– Echipa de Siguranță a {product_name}"
	},
	"email_verification": {
		"subject": "Verifică-ți adresa de e-mail pentru {product_name}",
		"body": "Salut {username},\n\nTe rugăm să verifici adresa de e-mail a contului tău {product_name} apăsând pe linkul de mai jos:\n\n{verifyUrl}\n\nDacă nu ai creat un cont {product_name}, poți ignora în siguranță acest e-mail.\n\nAcest link este valabil 24 de ore.\n\n– Echipa {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Beneficiile obținute prin cadoul tău răscumpărat au fost eliminate",
		"body": "Salut {username},\n\nUn cod cadou pe care l-ai răscumpărat a fost plătit inițial de altcineva. Acea plată a fost ulterior anulată (o rambursare).\n\nDin această cauză, am eliminat beneficiile care au fost adăugate contului tău atunci când ai răscumpărat cadoul.\n\nDacă crezi că aceasta este o greșeală, te rugăm să contactezi echipa noastră de suport și să incluzi orice detalii ai despre codul cadou și când l-ai răscumpărat.\n\n– Echipa {product_name}"
	},
	"harvest_completed": {
		"subject": "Exportul datelor din {product_name} este gata pentru descărcare",
		"body": "Salut {username},\n\nExportul datelor tale este gata.\n\nLink de descărcare:\n{downloadUrl}\n\nMesaje incluse: {totalMessages, number}\nDimensiune fișier: {fileSizeMB, number} MB\n\nAcest link expiră la data de {expiresAt, date, full} la ora {expiresAt, time, short}.\n\nDacă nu ai solicitat acest export, te rugăm să-ți schimbi parola imediat și să contactezi echipa noastră de suport.\n\n– Echipa {product_name}"
	},
	"inactivity_warning": {
		"subject": "Contul tău {product_name} va fi șters din cauza inactivității",
		"body": "Salut {username},\n\nNu am înregistrat nicio activitate pe contul tău {product_name} de la data de {lastActiveDate, date, full}.\n\nDacă nu te conectezi până la data de {deletionDate, date, full} la ora {deletionDate, time, short}, contul tău va fi șters permanent din cauza inactivității.\n\nConectează-te aici:\n{loginUrl}\n\nDacă ai folosit {product_name} recent, te rugăm să contactezi imediat echipa noastră de suport.\n\n– Echipa {product_name}"
	},
	"ip_authorization": {
		"subject": "Autorizează conectarea de la o nouă adresă IP",
		"body": "Salut {username},\n\nAm detectat o tentativă de conectare la contul tău {product_name} de la o nouă adresă IP:\n\nAdresă IP: {ipAddress}\nLocație: {location}\n\nDacă ai fost tu, te rugăm să autorizezi această adresă IP apăsând pe linkul de mai jos:\n\n{authUrl}\n\nDacă nu ai încercat să te conectezi, te rugăm să-ți schimbi parola imediat.\n\nAcest link este valabil 30 de minute.\n\n– Echipa {product_name}"
	},
	"password_change_verification": {
		"subject": "Confirmă schimbarea parolei pentru {product_name}",
		"body": "Salut {username},\n\nAm primit o solicitare de schimbare a parolei contului tău {product_name}.\n\nPentru a confirma această modificare, introdu acest cod în aplicație:\n\n{code}\n\nAcest cod expiră la {expiresAt}.\n\nDacă nu ai solicitat acest lucru, cineva ar putea avea acces la contul tău. Schimbă-ți parola imediat și activează autentificarea cu doi factori.\n\n– Echipa {product_name}"
	},
	"password_reset": {
		"subject": "Resetează-ți parola pentru {product_name}",
		"body": "Salut {username},\n\nAi solicitat o resetare a parolei pentru {product_name}. Folosește linkul de mai jos pentru a seta o nouă parolă:\n\n{resetUrl}\n\nDacă nu ai solicitat acest lucru, poți ignora în siguranță acest e-mail.\n\nAcest link este valabil 1 oră.\n\n– Echipa {product_name}"
	},
	"registration_approved": {
		"subject": "Înregistrarea ta pentru {product_name} a fost aprobată",
		"body": "Salut {username},\n\nVești bune: înregistrarea ta pentru {product_name} a fost aprobată.\n\nAcum te poți conecta la aplicația {product_name} aici:\n{channelsUrl}\n\nBun venit în comunitatea {product_name}.\n\n– Echipa {product_name}"
	},
	"report_resolved": {
		"subject": "Raportul tău privind {product_name} a fost revizuit",
		"body": "Salut {username},\n\nRaportul tău (ID: {reportId}) a fost revizuit de Echipa noastră de Siguranță.{hasComment, select, yes {\n\nRăspuns de la Echipa de Siguranță:\n{publicComment}} other {}}\n\nMulțumim că ajuți la menținerea siguranței {product_name} pentru toată lumea. Luăm în serios toate rapoartele și apreciem contribuția ta la comunitate.\n\nDacă ai întrebări sau nelămuriri cu privire la acest rezultat, te rugăm să contactezi {safety_email}.\n\n– Echipa de Siguranță a {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Contul tău de {product_name} va fi șters permanent",
		"body": "Salut {username},\n\nContul tău {product_name} a fost programat pentru ștergere permanentă.\n\nȘtergere programată: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Motiv: {reason}}\n}\n\nAceasta este o măsură disciplinară serioasă. Datele contului tău vor fi șterse permanent la data programată.\n\nDacă crezi că această decizie de aplicare a fost incorectă, poți depune un apel. Trimite un e-mail la {appeals_email} de la această adresă de e-mail.\n\n– {product_name} Safety Team"
	},
	"self_deletion_scheduled": {
		"subject": "Ștergerea contului {product_name} a fost programată",
		"body": "Salut {username},\n\nAi solicitat ștergerea contului tău {product_name}. Contul tău este programat pentru ștergere permanentă la:\n\n{deletionDate, date, full} la ora {deletionDate, time, short}\n\nDacă nu ai solicitat acest lucru, conectează-te la contul tău pentru a anula ștergerea. De asemenea, îți recomandăm să-ți schimbi parola pentru a-ți securiza contul.\n\n– Echipa {product_name}"
	},
	"unban_notification": {
		"subject": "Suspendarea contului {product_name} a fost ridicată",
		"body": "Salut {username},\n\nVești bune: suspendarea contului tău {product_name} a fost ridicată.\n\n{reason, select,\n  null {}\n  other {Motiv: {reason}}\n}\n\nAcum te poți conecta din nou și poți continua să folosești {product_name} în mod normal.\n\n– {product_name} Safety Team"
	}
});

export default EMAIL_I18N_RO_MESSAGES;
