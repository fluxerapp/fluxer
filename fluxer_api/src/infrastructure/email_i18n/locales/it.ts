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

export const it: EmailTranslations = {
	passwordReset: {
		subject: 'Reimposta la tua password Fluxer',
		body: `Ciao {username},

Hai richiesto di reimpostare la password del tuo account Fluxer. Segui il link qui sotto per impostare una nuova password:

{resetUrl}

Se non hai richiesto tu questa reimpostazione, puoi ignorare questa email in sicurezza.

Questo link scadrà tra 1 ora.

- Il team Fluxer`,
	},
	emailVerification: {
		subject: 'Verifica il tuo indirizzo email Fluxer',
		body: `Ciao {username},

Per favore verifica l'indirizzo email associato al tuo account Fluxer cliccando sul link qui sotto:

{verifyUrl}

Se non hai creato un account Fluxer, puoi ignorare questa email.

Questo link scadrà tra 24 ore.

- Il team Fluxer`,
	},
	ipAuthorization: {
		subject: "Autorizza l'accesso da un nuovo indirizzo IP",
		body: `Ciao {username},

Abbiamo rilevato un tentativo di accesso al tuo account Fluxer da un nuovo indirizzo IP:

Indirizzo IP: {ipAddress}
Località: {location}

Se sei stato tu, autorizza questo indirizzo IP cliccando sul link:

{authUrl}

Se non hai tentato di accedere, modifica subito la tua password.

Questo link di autorizzazione scadrà tra 30 minuti.

- Il team Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Il tuo account Fluxer è stato temporaneamente disabilitato',
		body: `Ciao {username},

Il tuo account Fluxer è stato temporaneamente disabilitato a causa di attività sospette.

{reason, select,
	null {}
	other {Motivo: {reason}

}}Per riottenere l'accesso al tuo account, devi reimpostare la password:

{forgotUrl}

Dopo aver reimpostato la password, potrai accedere nuovamente.

Se ritieni che questa misura sia stata presa per errore, contatta il nostro team di supporto.

- Il team Sicurezza Fluxer`,
	},
	accountTempBanned: {
		subject: 'Il tuo account Fluxer è stato temporaneamente sospeso',
		body: `Ciao {username},

Il tuo account Fluxer è stato temporaneamente sospeso per violazione dei nostri Termini di servizio o Linee guida della community.

Durata: {durationHours, plural,
	=1 {1 ora}
	other {# ore}
}
Sospeso fino al: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Motivo: {reason}}
}

Durante questo periodo non potrai accedere al tuo account.

Ti invitiamo a consultare:
- Termini di servizio: {termsUrl}
- Linee guida della community: {guidelinesUrl}

Se ritieni che questa decisione sia errata o ingiustificata, puoi inviare un ricorso a appeals@fluxer.app da questo indirizzo email. Ti chiediamo di spiegare chiaramente perché ritieni che la decisione sia sbagliata. Esamineremo il ricorso e risponderemo con la nostra valutazione.

- Il team Sicurezza Fluxer`,
	},
	accountScheduledDeletion: {
		subject: "Il tuo account Fluxer è programmato per l'eliminazione",
		body: `Ciao {username},

Il tuo account Fluxer è stato programmato per l'eliminazione permanente a causa della violazione dei nostri Termini di servizio o Linee guida della community.

Data di eliminazione programmata: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Motivo: {reason}}
}

Questa è una misura disciplinare seria. I dati del tuo account verranno eliminati definitivamente alla data indicata.

Ti invitiamo a consultare:
- Termini di servizio: {termsUrl}
- Linee guida della community: {guidelinesUrl}

PROCESSO DI RICORSO:
Se ritieni che la decisione sia errata o ingiustificata, hai 30 giorni di tempo per inviare un ricorso a appeals@fluxer.app da questo indirizzo email.

Nel tuo ricorso:
- Spiega chiaramente perché ritieni che la decisione sia sbagliata
- Fornisci eventuali prove o contesto rilevante

Un membro del team Sicurezza Fluxer esaminerà il ricorso e potrà sospendere l'eliminazione fino alla decisione finale.

- Il team Sicurezza Fluxer`,
	},
	selfDeletionScheduled: {
		subject: "L'eliminazione del tuo account Fluxer è stata programmata",
		body: `Ciao {username},

Ci dispiace vederti andare via! L'eliminazione del tuo account Fluxer è stata programmata.

Data di eliminazione programmata: {deletionDate, date, full} {deletionDate, time, short}

IMPORTANTE: Puoi annullare questa eliminazione in qualsiasi momento prima del {deletionDate, date, full} {deletionDate, time, short} semplicemente accedendo di nuovo al tuo account.

PRIMA DI ANDARE:
La tua Dashboard della privacy nelle Impostazioni utente ti consente di:
- Eliminare i tuoi messaggi sulla piattaforma
- Estrarre i tuoi dati importanti prima di lasciare

Nota: Una volta eliminato l'account, non sarà più possibile eliminare i tuoi messaggi. Se desideri farlo, fallo prima della data di eliminazione.

Se cambi idea, effettua nuovamente l'accesso per annullare l'eliminazione.

- Il team Fluxer`,
	},
	inactivityWarning: {
		subject: 'Il tuo account Fluxer verrà eliminato per inattività',
		body: `Ciao {username},

Abbiamo notato che non accedi al tuo account Fluxer da oltre 2 anni.

Ultimo accesso: {lastActiveDate, date, full} {lastActiveDate, time, short}

In base alla nostra politica di conservazione dei dati, gli account inattivi vengono automaticamente programmati per l'eliminazione. Il tuo account verrà eliminato definitivamente il:

Data di eliminazione programmata: {deletionDate, date, full} {deletionDate, time, short}

COME MANTENERE IL TUO ACCOUNT:
È sufficiente accedere al tuo account all'indirizzo {loginUrl} prima della data di eliminazione per annullare questo processo automatico.

SE NON ACCEDI:
- Il tuo account e tutti i dati associati verranno eliminati in modo permanente
- I tuoi messaggi verranno anonimizzati (attribuiti a “Utente eliminato”)
- Questa azione è irreversibile

VUOI ELIMINARE I TUOI MESSAGGI?
Se desideri eliminare i tuoi messaggi prima che il tuo account venga rimosso, accedi e utilizza la Dashboard della privacy.

Speriamo di rivederti presto su Fluxer!

- Il team Fluxer`,
	},
	harvestCompleted: {
		subject: 'La tua esportazione dei dati Fluxer è pronta',
		body: `Ciao {username},

La tua esportazione dei dati è stata completata ed è pronta per il download!

Riepilogo dell'esportazione:
- Numero totale di messaggi: {totalMessages, number}
- Dimensione del file: {fileSizeMB} MB
- Formato: Archivio ZIP con file JSON

Scarica i tuoi dati: {downloadUrl}

IMPORTANTE: Questo link per il download scadrà il {expiresAt, date, full} {expiresAt, time, short}

Cosa è incluso nell'esportazione:
- Tutti i tuoi messaggi organizzati per canale
- Metadati dei canali
- Il tuo profilo utente e informazioni sull'account
- Impostazioni e appartenenze ai server (guild)
- Sessioni di autenticazione e informazioni sulla sicurezza

I dati sono forniti in formato JSON per facilitare l'analisi.

Se hai domande sulla tua esportazione, contatta support@fluxer.app

- Il team Fluxer`,
	},
	unbanNotification: {
		subject: 'La sospensione del tuo account Fluxer è stata revocata',
		body: `Ciao {username},

Buone notizie! La sospensione del tuo account Fluxer è stata revocata.

Motivo: {reason}

Ora puoi accedere nuovamente e continuare a utilizzare Fluxer.

- Il team Sicurezza Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: "Il tuo account Fluxer è programmato per l'eliminazione",
		body: `Ciao {username},

Il tuo account Fluxer è stato programmato per l'eliminazione permanente.

Data di eliminazione programmata: {deletionDate, date, full} {deletionDate, time, short}
Motivo: {reason}

Questa è una misura disciplinare seria. I dati del tuo account verranno eliminati definitivamente.

Se ritieni che questa decisione sia incorretta, puoi inviare un ricorso a appeals@fluxer.app.

- Il team Sicurezza Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Il tuo regalo Fluxer Premium è stato revocato',
		body: `Ciao {username},

Ti informiamo che il regalo Fluxer Premium che hai riscattato è stato revocato a causa di una contestazione di pagamento (chargeback) presentata dall'acquirente originale.

I tuoi vantaggi Premium sono stati rimossi dal tuo account. Ciò è avvenuto perché il pagamento è stato annullato.

Per eventuali domande, contatta support@fluxer.app.

- Il team Fluxer`,
	},
	reportResolved: {
		subject: 'La tua segnalazione Fluxer è stata esaminata',
		body: `Ciao {username},

La tua segnalazione (ID: {reportId}) è stata esaminata dal nostro Team Sicurezza.

Risposta del Team Sicurezza:
{publicComment}

Grazie per aver contribuito a mantenere Fluxer un ambiente sicuro. Apprezziamo il tuo contributo alla nostra community.

Per qualsiasi domanda o dubbio, contatta safety@fluxer.app.

- Il team Sicurezza Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Verifica la tua email per una segnalazione DSA',
		body: `Salve,

Utilizza il seguente codice di verifica per inviare la tua segnalazione ai sensi del Digital Services Act su Fluxer:

{code}

Questo codice scade il {expiresAt, date, full} {expiresAt, time, short}.

Se non hai richiesto questa verifica, ignora questa email.

- Il team Sicurezza Fluxer`,
	},
	registrationApproved: {
		subject: 'La tua registrazione Fluxer è stata approvata',
		body: `Ciao {username},

Ottime notizie! La tua registrazione a Fluxer è stata approvata.

Ora puoi accedere all'app Fluxer qui:
{channelsUrl}

Benvenuto nella community Fluxer!

- Il team Fluxer`,
	},
	emailChangeRevert: {
		subject: 'La tua email Fluxer è stata modificata',
		body: `Ciao {username},

L'email del tuo account Fluxer è stata cambiata in {newEmail}.

Se hai effettuato tu questa modifica, non devi fare altro. In caso contrario, puoi annullarla e mettere al sicuro il tuo account con questo link:

{revertUrl}

Questo ripristinerà la tua email precedente, ti disconnetterà ovunque, rimuoverà i numeri di telefono associati, disabiliterà l'MFA e richiederà una nuova password.

- Team Sicurezza Fluxer`,
	},
};
