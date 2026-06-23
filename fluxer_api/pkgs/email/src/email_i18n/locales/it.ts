// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_IT_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Il tuo account {product_name} è stato temporaneamente disabilitato",
		"body": "Ciao {username},\n\nAbbiamo temporaneamente disabilitato il tuo account {product_name} perché abbiamo rilevato attività sospette.\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nPer riottenere l'accesso al tuo account, dovrai reimpostare la password:\n\n{forgotUrl}\n\nDopo aver reimpostato la password, potrai accedere di nuovo.\n\nSe ritieni che ciò sia stato fatto per errore, contatta il nostro team di supporto.\n\n– Team di Sicurezza {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Il tuo account {product_name} verrà eliminato definitivamente",
		"body": "Ciao {username},\n\nIl tuo account {product_name} è stato programmato per l'eliminazione permanente a causa di violazioni dei nostri Termini di Servizio o delle Linee Guida della Comunità.\n\nEliminazione programmata: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nSi tratta di un provvedimento grave. I dati del tuo account verranno eliminati definitivamente alla data programmata.\n\nConsulta:\n- Termini di Servizio: {termsUrl}\n- Linee Guida della Comunità: {guidelinesUrl}\n\nProcesso di ricorso:\nSe ritieni che questa decisione sia scorretta o ingiustificata, hai 60 giorni per presentare un ricorso. Invia un'e-mail a {appeals_email} da questo indirizzo e-mail.\n\nNel tuo ricorso:\n- Spiega chiaramente perché ritieni che la decisione sia scorretta o ingiustificata\n- Fornisci qualsiasi prova o contesto pertinente\n\nUn membro del Team di Sicurezza {product_name} esaminerà il tuo ricorso e potrebbe sospendere l'eliminazione in corso fino a quando non sarà stata raggiunta una decisione finale.\n\n– Team di Sicurezza {product_name}"
	},
	"account_temp_banned": {
		"subject": "Il tuo account {product_name} è stato temporaneamente sospeso",
		"body": "Ciao {username},\n\nIl tuo account {product_name} è stato temporaneamente sospeso per aver violato i nostri Termini di Servizio o le Linee Guida della Comunità.\n\nDurata: {durationHours, plural,\n  =1 {1 ora}\n  other {# ore}\n}\nSospeso fino a: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nDurante questo periodo, non potrai accedere al tuo account.\n\nConsulta:\n- Termini di Servizio: {termsUrl}\n- Linee Guida della Comunità: {guidelinesUrl}\n\nSe ritieni che questa decisione sia scorretta o ingiustificata, puoi presentare un ricorso. Invia un'e-mail a {appeals_email} da questo indirizzo e-mail e spiega chiaramente perché ritieni che la decisione sia scorretta. Esamineremo il tuo ricorso e risponderemo con la nostra decisione.\n\n– Team di Sicurezza {product_name}"
	},
	"donation_confirmation": {
		"subject": "Grazie per la tua donazione a {product_name}",
		"body": "Ciao,\n\nGrazie per la tua donazione a {product_name}! La tua {interval, select,\n  month {donazione ricorrente}\n  year {donazione ricorrente}\n  other {donazione una tantum}\n} è stata {interval, select,\n  month {impostata}\n  year {impostata}\n  other {elaborata}\n} con successo.\n\nDettagli della donazione:\nImporto: {amount} {currency} {interval, select,\n  month {al mese}\n  year {all'anno}\n  other {}\n}\n\nStripe ti invierà a breve una ricevuta separata con la tua fattura in PDF. Questa include tutti i dettagli del pagamento e può essere utilizzata a fini fiscali.\n\nPuoi visualizzare la cronologia delle tue donazioni, scaricare le fatture, {interval, select,\n  month {e gestire o annullare il tuo abbonamento}\n  year {e gestire o annullare il tuo abbonamento}\n  other {e gestire le future donazioni}\n} in qualsiasi momento utilizzando questo link:\n\n{manageUrl}\n\nIl tuo supporto aiuta a mantenere {product_name} in funzione. Grazie!\n\n– Team {product_name}"
	},
	"donation_magic_link": {
		"subject": "Gestisci le tue donazioni a {product_name}",
		"body": "Ciao,\n\nClicca sul link qui sotto per accedere al tuo portale donatori:\n\n{manageUrl}\n\nNel portale, puoi gestire gli abbonamenti, scaricare le fatture e visualizzare la cronologia delle tue donazioni.\n\nQuesto link scade il {expiresAt, date, full} alle {expiresAt, time, short}.\n\nSe non hai richiesto questo link, puoi ignorare tranquillamente questa email.\n\n– Team {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Verifica la tua e-mail per una segnalazione DSA",
		"body": "Ciao,\n\nUsa il codice di verifica qui sotto per inviare la tua segnalazione DSA su {product_name}:\n\n{code}\n\nQuesto codice scade il {expiresAt, date, full} alle {expiresAt, time, short}.\n\nSe non hai richiesto questa operazione, puoi ignorare questa email.\n\n– Team di Sicurezza {product_name}"
	},
	"email_change_new": {
		"subject": "Verifica la tua nuova e-mail di {product_name}",
		"body": "Ciao {username},\n\nInserisci questo codice nell'app per verificare la tua nuova e-mail di {product_name}:\n\n{code}\n\nQuesto codice scade il {expiresAt, date, full} alle {expiresAt, time, short}.\n\nSe non hai richiesto questa operazione, puoi ignorare questa e-mail.\n\n– Team {product_name}"
	},
	"email_change_original": {
		"subject": "Conferma la modifica della tua e-mail di {product_name}",
		"body": "Ciao {username},\n\nAbbiamo ricevuto una richiesta di modifica dell'indirizzo e-mail del tuo account {product_name}.\n\nPer confermare questa modifica, inserisci questo codice nell'app:\n\n{code}\n\nQuesto codice scade il {expiresAt, date, full} alle {expiresAt, time, short}.\n\nSe non hai richiesto questa operazione, metti subito in sicurezza il tuo account.\n\n– Team {product_name}"
	},
	"email_change_revert": {
		"subject": "La tua e-mail {product_name} è stata modificata",
		"body": "Ciao {username},\n\nL'indirizzo e-mail del tuo account {product_name} è stato modificato in {newEmail}.\n\nSe hai effettuato questa modifica, non è necessaria alcuna azione. In caso contrario, puoi annullare la modifica e mettere in sicurezza il tuo account utilizzando questo link:\n\n{revertUrl}\n\nQuesto ripristinerà la tua e-mail precedente, ti disconnetterà da tutti i dispositivi, rimuoverà i numeri di telefono collegati, disabiliterà l'MFA e ti richiederà di impostare una nuova password.\n\n– Team di Sicurezza {product_name}"
	},
	"email_verification": {
		"subject": "Verifica il tuo indirizzo e-mail di {product_name}",
		"body": "Ciao {username},\n\nVerifica l'indirizzo e-mail del tuo account {product_name} cliccando sul link qui sotto:\n\n{verifyUrl}\n\nSe non hai creato un account {product_name}, puoi ignorare tranquillamente questa e-mail.\n\nQuesto link è valido per 24 ore.\n\n– Team {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "I vantaggi del tuo regalo riscattato sono stati rimossi",
		"body": "Ciao {username},\n\nUn codice regalo che hai riscattato era stato originariamente pagato da qualcun altro. Tale pagamento è stato successivamente annullato (un chargeback).\n\nPer questo motivo, abbiamo rimosso i benefici che erano stati aggiunti al tuo account quando hai riscattato il regalo.\n\nSe ritieni che si tratti di un errore, contatta il nostro team di supporto e includi tutti i dettagli che hai sul codice regalo e su quando lo hai riscattato.\n\n– Team {product_name}"
	},
	"harvest_completed": {
		"subject": "La tua esportazione dati {product_name} è pronta per il download",
		"body": "Ciao {username},\n\nLa tua esportazione dei dati è pronta.\n\nLink per il download:\n{downloadUrl}\n\nMessaggi inclusi: {totalMessages, number}\nDimensione file: {fileSizeMB, number} MB\n\nQuesto link scade il {expiresAt, date, full} alle {expiresAt, time, short}.\n\nSe non hai richiesto questa esportazione, cambia immediatamente la password e contatta il nostro team di supporto.\n\n– Team {product_name}"
	},
	"inactivity_warning": {
		"subject": "Il tuo account {product_name} verrà eliminato per inattività",
		"body": "Ciao {username},\n\nNon abbiamo rilevato alcuna attività sul tuo account {product_name} dal {lastActiveDate, date, full}.\n\nSe non effettui l'accesso entro il {deletionDate, date, full} alle {deletionDate, time, short}, il tuo account verrà eliminato definitivamente a causa di inattività.\n\nAccedi qui:\n{loginUrl}\n\nSe hai usato {product_name} di recente, contatta subito il nostro team di supporto.\n\n– Team {product_name}"
	},
	"ip_authorization": {
		"subject": "Autorizza l'accesso da un nuovo indirizzo IP",
		"body": "Ciao {username},\n\nAbbiamo rilevato un tentativo di accesso al tuo account {product_name} da un nuovo indirizzo IP:\n\nIndirizzo IP: {ipAddress}\nPosizione: {location}\n\nSe sei stato tu, autorizza questo indirizzo IP cliccando sul link qui sotto:\n\n{authUrl}\n\nSe non hai tentato di accedere, cambia subito la password.\n\nQuesto link è valido per 30 minuti.\n\n– Team {product_name}"
	},
	"password_change_verification": {
		"subject": "Conferma la modifica della password di {product_name}",
		"body": "Ciao {username},\n\nAbbiamo ricevuto una richiesta di modifica della password del tuo account {product_name}.\n\nPer confermare questa modifica, inserisci questo codice nell'app:\n\n{code}\n\nQuesto codice scade alle {expiresAt}.\n\nSe non hai richiesto questa modifica, qualcuno potrebbe avere accesso al tuo account. Cambia immediatamente la password e abilita l'autenticazione a due fattori.\n\n– Team {product_name}"
	},
	"password_reset": {
		"subject": "Reimposta la password di {product_name}",
		"body": "Ciao {username},\n\nHai richiesto il ripristino della tua password {product_name}. Usa il link qui sotto per impostare una nuova password:\n\n{resetUrl}\n\nSe non hai richiesto questa operazione, puoi ignorare tranquillamente questa email.\n\nQuesto link è valido per 1 ora.\n\n– Team {product_name}"
	},
	"registration_approved": {
		"subject": "La tua registrazione a {product_name} è stata approvata",
		"body": "Ciao {username},\n\nBuone notizie: la tua registrazione a {product_name} è stata approvata.\n\nOra puoi accedere all'app {product_name} qui:\n{channelsUrl}\n\nBenvenuto nella comunità di {product_name}.\n\n– Team {product_name}"
	},
	"report_resolved": {
		"subject": "La tua segnalazione {product_name} è stata esaminata",
		"body": "Ciao {username},\n\nLa tua segnalazione (ID: {reportId}) è stata esaminata dal nostro Team di Sicurezza.{hasComment, select, yes {\n\nRisposta dal Team di Sicurezza:\n{publicComment}} other {}}\n\nGrazie per aver contribuito a mantenere {product_name} sicuro per tutti. Prendiamo sul serio tutte le segnalazioni e apprezziamo il tuo contributo alla comunità.\n\nSe hai domande o preoccupazioni riguardo a questo esito, contatta {safety_email}.\n\n– Team di Sicurezza {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Il tuo account {product_name} verrà eliminato definitivamente",
		"body": "Ciao {username},\n\nIl tuo account {product_name} è stato programmato per l'eliminazione permanente.\n\nEliminazione programmata: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nSi tratta di un provvedimento grave. I dati del tuo account verranno eliminati definitivamente alla data programmata.\n\nSe ritieni che questa decisione sia scorretta, puoi presentare un ricorso. Invia un'e-mail a {appeals_email} da questo indirizzo e-mail.\n\n– Team di Sicurezza {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "L'eliminazione del tuo account {product_name} è programmata",
		"body": "Ciao {username},\n\nHai richiesto di eliminare il tuo account {product_name}. Il tuo account è programmato per l'eliminazione permanente il:\n\n{deletionDate, date, full} alle {deletionDate, time, short}\n\nSe non hai richiesto l'eliminazione, accedi al tuo account per annullarla. Ti consigliamo anche di cambiare la password per mettere in sicurezza il tuo account.\n\n– Team {product_name}"
	},
	"unban_notification": {
		"subject": "La sospensione del tuo account {product_name} è stata revocata",
		"body": "Ciao {username},\n\nBuone notizie: la sospensione del tuo account {product_name} è stata revocata.\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nOra puoi accedere di nuovo e continuare a usare {product_name} normalmente.\n\n– Team di Sicurezza {product_name}"
	}
});

export default EMAIL_I18N_IT_MESSAGES;
