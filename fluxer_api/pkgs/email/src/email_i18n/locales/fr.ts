// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '@pkgs/email/src/email_i18n/EmailI18nMessages';

const EMAIL_I18N_FR_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Votre compte {product_name} a été temporairement désactivé",
		"body": "Bonjour {username},\n\nNous avons temporairement désactivé votre compte {product_name} après avoir détecté une activité suspecte.\n\n{reason, select, null {} other {Motif : {reason}}}\n\nPour retrouver l’accès à votre compte, réinitialisez votre mot de passe :\n\n{forgotUrl}\n\nVous pourrez ensuite vous connecter à nouveau.\n\nSi vous pensez qu’il s’agit d’une erreur, contactez notre équipe d’assistance.\n\n– L’équipe de sécurité de {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Votre compte {product_name} sera définitivement supprimé",
		"body": "Bonjour {username},\n\nLa suppression définitive de votre compte {product_name} a été programmée en raison de violations de nos conditions d’utilisation ou des règles de la communauté.\n\nSuppression prévue : {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select, null {} other {Motif : {reason}}}\n\nIl s’agit d’une sanction importante. Les données de votre compte seront définitivement supprimées à la date prévue.\n\nVeuillez consulter :\n- Conditions d’utilisation : {termsUrl}\n- Règles de la communauté : {guidelinesUrl}\n\nProcédure de recours :\nSi vous estimez que cette décision est incorrecte ou injustifiée, vous disposez de 60 jours pour déposer un recours. Écrivez à {appeals_email} depuis cette adresse e-mail.\n\nDans votre recours :\n- Expliquez clairement pourquoi vous estimez la décision incorrecte ou injustifiée\n- Fournissez tout élément de preuve ou de contexte pertinent\n\nUn membre de l’équipe de sécurité de {product_name} examinera votre recours et pourra suspendre la suppression prévue jusqu’à la décision finale.\n\n– L’équipe de sécurité de {product_name}"
	},
	"account_temp_banned": {
		"subject": "Votre compte {product_name} a été temporairement suspendu",
		"body": "Bonjour {username},\n\nVotre compte {product_name} a été temporairement suspendu pour violation de nos conditions d’utilisation ou des règles de la communauté.\n\nDurée : {durationHours, plural, =1 {1 heure} other {# heures}}\nSuspension jusqu’au : {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select, null {} other {Motif : {reason}}}\n\nVous ne pourrez pas accéder à votre compte pendant cette période.\n\nVeuillez consulter :\n- Conditions d’utilisation : {termsUrl}\n- Règles de la communauté : {guidelinesUrl}\n\nSi vous estimez que cette décision est incorrecte ou injustifiée, vous pouvez déposer un recours. Écrivez à {appeals_email} depuis cette adresse e-mail et expliquez clairement pourquoi vous contestez cette décision. Nous examinerons votre recours et vous communiquerons notre décision.\n\n– L’équipe de sécurité de {product_name}"
	},
	"donation_confirmation": {
		"subject": "Merci pour votre don à {product_name}",
		"body": "Bonjour,\n\nMerci pour votre don à {product_name} ! {interval, select, month {Votre don récurrent a bien été mis en place.} year {Votre don récurrent a bien été mis en place.} other {Votre don ponctuel a bien été traité.}}\n\nDétails du don :\nMontant : {amount} {interval, select, month {par mois} year {par an} other {}}\n\nStripe vous enverra bientôt un reçu séparé accompagné de votre facture PDF. Ce document contient tous les détails du paiement et peut servir de justificatif fiscal.\n\nVous pouvez consulter l’historique de vos dons, télécharger des factures {interval, select, month {et gérer ou résilier votre abonnement} year {et gérer ou résilier votre abonnement} other {et gérer vos futurs dons}} à tout moment avec ce lien :\n\n{manageUrl}\n\nVotre soutien contribue au fonctionnement de {product_name}. Merci !\n\n– L’équipe {product_name}"
	},
	"donation_magic_link": {
		"subject": "Gérer vos dons à {product_name}",
		"body": "Bonjour,\n\nCliquez sur le lien ci-dessous pour accéder à votre portail de donateur :\n\n{manageUrl}\n\nVous pouvez y gérer vos abonnements, télécharger des factures et consulter l’historique de vos dons.\n\nCe lien expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi vous n’avez pas demandé ce lien, vous pouvez ignorer cet e-mail.\n\n– L’équipe {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Vérifiez votre e-mail pour un signalement DSA",
		"body": "Bonjour,\n\nUtilisez le code ci-dessous pour envoyer votre signalement au titre du règlement sur les services numériques (DSA) sur {product_name} :\n\n{code}\n\nCe code expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi vous n’êtes pas à l’origine de cette demande, vous pouvez ignorer cet e-mail.\n\n– L’équipe de sécurité de {product_name}"
	},
	"email_change_new": {
		"subject": "Vérifiez votre nouvelle adresse e-mail {product_name}",
		"body": "Bonjour {username},\n\nSaisissez ce code dans l’application pour vérifier votre nouvelle adresse e-mail {product_name} :\n\n{code}\n\nCe code expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi vous n’êtes pas à l’origine de cette demande, vous pouvez ignorer cet e-mail.\n\n– L’équipe {product_name}"
	},
	"email_change_original": {
		"subject": "Confirmez votre changement d’adresse e-mail {product_name}",
		"body": "Bonjour {username},\n\nNous avons reçu une demande de modification de l’adresse e-mail de votre compte {product_name}.\n\nPour confirmer ce changement, saisissez ce code dans l’application :\n\n{code}\n\nCe code expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi vous n’êtes pas à l’origine de cette demande, sécurisez votre compte immédiatement.\n\n– L’équipe {product_name}"
	},
	"email_change_revert": {
		"subject": "Votre adresse e-mail {product_name} a été modifiée",
		"body": "Bonjour {username},\n\nL’adresse e-mail de votre compte {product_name} a été remplacée par {newEmail}.\n\nSi vous avez effectué ce changement, vous n’avez rien à faire. Sinon, vous pouvez l’annuler et sécuriser votre compte avec ce lien :\n\n{revertUrl}\n\nVotre ancienne adresse e-mail sera rétablie, toutes vos sessions seront fermées, les numéros de téléphone associés seront supprimés et l’authentification multifacteur sera désactivée. Vous devrez aussi définir un nouveau mot de passe.\n\n– L’équipe de sécurité de {product_name}"
	},
	"email_verification": {
		"subject": "Vérifiez votre adresse e-mail {product_name}",
		"body": "Bonjour {username},\n\nVérifiez l’adresse e-mail de votre compte {product_name} en cliquant sur le lien ci-dessous :\n\n{verifyUrl}\n\nSi vous n’avez pas créé de compte {product_name}, vous pouvez ignorer cet e-mail.\n\nCe lien est valable pendant 24 heures.\n\n– L’équipe {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Les avantages de votre cadeau activé ont été retirés",
		"body": "Bonjour {username},\n\nUn code cadeau que vous avez utilisé avait été payé par une autre personne. Ce paiement a depuis été annulé à la suite d’une contestation bancaire.\n\nNous avons donc retiré les avantages ajoutés à votre compte lors de l’activation du cadeau.\n\nSi vous pensez qu’il s’agit d’une erreur, contactez notre équipe d’assistance et indiquez les informations dont vous disposez sur le code cadeau et sa date d’utilisation.\n\n– L’équipe {product_name}"
	},
	"harvest_completed": {
		"subject": "Votre export de données {product_name} est prêt à être téléchargé",
		"body": "Bonjour {username},\n\nVotre export de données est prêt.\n\nLien de téléchargement :\n{downloadUrl}\n\nMessages inclus : {totalMessages, number}\nTaille du fichier : {fileSizeMB, number} Mo\n\nCe lien expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi vous n’avez pas demandé cet export, changez immédiatement votre mot de passe et contactez notre équipe d’assistance.\n\n– L’équipe {product_name}"
	},
	"inactivity_warning": {
		"subject": "Votre compte {product_name} sera supprimé pour inactivité",
		"body": "Bonjour {username},\n\nNous n’avons détecté aucune activité sur votre compte {product_name} depuis le {lastActiveDate, date, full}.\n\nSi vous ne vous connectez pas avant le {deletionDate, date, full} à {deletionDate, time, short}, votre compte sera définitivement supprimé pour inactivité.\n\nConnectez-vous ici :\n{loginUrl}\n\nSi vous avez utilisé {product_name} récemment, contactez immédiatement notre équipe d’assistance.\n\n– L’équipe {product_name}"
	},
	"ip_authorization": {
		"subject": "Autoriser la connexion depuis une nouvelle adresse IP",
		"body": "Bonjour {username},\n\nNous avons détecté une tentative de connexion à votre compte {product_name} depuis une nouvelle adresse IP :\n\nAdresse IP : {ipAddress}\nLocalisation : {location}\n\nSi vous êtes à l’origine de cette tentative, autorisez cette adresse IP en cliquant sur le lien ci-dessous :\n\n{authUrl}\n\nSinon, changez immédiatement votre mot de passe.\n\nCe lien est valable pendant 30 minutes.\n\n– L’équipe {product_name}"
	},
	"mfa_backup_codes_view": {
		"subject": "Confirmez l’accès à vos codes de secours {product_name}",
		"body": "Bonjour {username},\n\nNous avons reçu une demande de consultation des codes de secours de votre compte {product_name}.\n\nPour confirmer cette demande, saisissez ce code dans l’application :\n\n{code}\n\nCe code expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi vous n’êtes pas à l’origine de cette demande, quelqu’un a peut-être accès à votre compte. Changez immédiatement votre mot de passe.\n\n– L’équipe {product_name}"
	},
	"password_change_verification": {
		"subject": "Confirmez la modification de votre mot de passe {product_name}",
		"body": "Bonjour {username},\n\nNous avons reçu une demande de modification du mot de passe de votre compte {product_name}.\n\nPour confirmer ce changement, saisissez ce code dans l’application :\n\n{code}\n\nCe code expire à {expiresAt}.\n\nSi vous n’êtes pas à l’origine de cette demande, quelqu’un a peut-être accès à votre compte. Changez immédiatement votre mot de passe et activez l’authentification à deux facteurs.\n\n– L’équipe {product_name}"
	},
	"password_reset": {
		"subject": "Réinitialiser votre mot de passe {product_name}",
		"body": "Bonjour {username},\n\nVous avez demandé la réinitialisation de votre mot de passe {product_name}. Utilisez le lien ci-dessous pour en définir un nouveau :\n\n{resetUrl}\n\nSi vous n’êtes pas à l’origine de cette demande, vous pouvez ignorer cet e-mail.\n\nCe lien est valable pendant 1 heure.\n\n– L’équipe {product_name}"
	},
	"registration_approved": {
		"subject": "Votre inscription à {product_name} a été approuvée",
		"body": "Bonjour {username},\n\nBonne nouvelle : votre inscription à {product_name} a été approuvée.\n\nVous pouvez maintenant vous connecter à l’application {product_name} ici :\n{channelsUrl}\n\nBienvenue dans la communauté {product_name}.\n\n– L’équipe {product_name}"
	},
	"report_resolved": {
		"subject": "Votre signalement sur {product_name} a été examiné",
		"body": "Bonjour {username},\n\nVotre signalement (ID : {reportId}) a été examiné par notre équipe de sécurité.{hasComment, select, yes {\n\nRéponse de l’équipe de sécurité :\n{publicComment}} other {}}\n\nMerci de nous aider à préserver la sécurité de {product_name} pour tous. Nous prenons tous les signalements au sérieux et apprécions votre contribution à la communauté.\n\nPour toute question ou préoccupation concernant cette décision, contactez {safety_email}.\n\n– L’équipe de sécurité de {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Votre compte {product_name} sera définitivement supprimé",
		"body": "Bonjour {username},\n\nLa suppression définitive de votre compte {product_name} a été programmée.\n\nSuppression prévue : {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select, null {} other {Motif : {reason}}}\n\nIl s’agit d’une sanction importante. Les données de votre compte seront définitivement supprimées à la date prévue.\n\nSi vous estimez que cette décision est incorrecte, vous pouvez déposer un recours. Écrivez à {appeals_email} depuis cette adresse e-mail.\n\n– L’équipe de sécurité de {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "La suppression de votre compte {product_name} est programmée",
		"body": "Bonjour {username},\n\nVous avez demandé la suppression de votre compte {product_name}. Sa suppression définitive est prévue le :\n\n{deletionDate, date, full} à {deletionDate, time, short}\n\nSi vous n’êtes pas à l’origine de cette demande, connectez-vous à votre compte pour annuler la suppression. Nous vous recommandons également de changer votre mot de passe pour sécuriser votre compte.\n\n– L’équipe {product_name}"
	},
	"unban_notification": {
		"subject": "La suspension de votre compte {product_name} a été levée",
		"body": "Bonjour {username},\n\nBonne nouvelle : la suspension de votre compte {product_name} a été levée.\n\n{reason, select, null {} other {Motif : {reason}}}\n\nVous pouvez maintenant vous reconnecter et utiliser {product_name} normalement.\n\n– L’équipe de sécurité de {product_name}"
	}
});

export default EMAIL_I18N_FR_MESSAGES;
