// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_FR_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Ton compte {product_name} a été temporairement désactivé",
		"body": "Bonjour {username},\n\nNous avons temporairement désactivé ton compte {product_name} car nous avons détecté une activité suspecte.\n\n{reason, select,\n  null {}\n  other {Raison : {reason}}\n}\n\nPour retrouver l'accès à ton compte, tu devras réinitialiser ton mot de passe :\n\n{forgotUrl}\n\nAprès avoir réinitialisé ton mot de passe, tu pourras te connecter à nouveau.\n\nSi tu penses qu'il s'agit d'une erreur, contacte notre équipe d'assistance.\n\n– Équipe de sécurité de {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Ton compte {product_name} sera définitivement supprimé",
		"body": "Bonjour {username},\n\nLa suppression définitive de ton compte {product_name} a été programmée en raison de violations de nos Conditions d'utilisation ou de nos Règles de la communauté.\n\nSuppression programmée : {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Raison : {reason}}\n}\n\nIl s'agit d'une mesure sérieuse. Les données de ton compte seront définitivement supprimées à la date prévue.\n\nConsulte :\n- Conditions d'utilisation : {termsUrl}\n- Règles de la communauté : {guidelinesUrl}\n\nProcédure d'appel :\nSi tu penses que cette décision était incorrecte ou injustifiée, tu as 60 jours pour soumettre un appel. Envoie un e-mail à {appeals_email} depuis cette adresse e-mail.\n\nDans ton appel :\n- Explique clairement pourquoi tu penses que la décision était incorrecte ou injustifiée\n- Fournis toute preuve ou contexte pertinent\n\nUn membre de l'équipe de sécurité de {product_name} examinera ton appel et pourra suspendre la suppression en attente jusqu'à ce qu'une décision finale soit prise.\n\n– Équipe de sécurité de {product_name}"
	},
	"account_temp_banned": {
		"subject": "Ton compte {product_name} a été temporairement suspendu",
		"body": "Bonjour {username},\n\nTon compte {product_name} a été temporairement suspendu pour avoir enfreint nos Conditions d'utilisation ou nos Règles de la communauté.\n\nDurée : {durationHours, plural,\n  =1 {1 heure}\n  other {# heures}\n}\nSuspendu jusqu'au : {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Raison : {reason}}\n}\n\nPendant cette période, tu ne pourras pas accéder à ton compte.\n\nConsulte :\n- Conditions d'utilisation : {termsUrl}\n- Règles de la communauté : {guidelinesUrl}\n\nSi tu penses que cette décision était incorrecte ou injustifiée, tu peux faire appel. Envoie un e-mail à {appeals_email} depuis cette adresse e-mail et explique clairement pourquoi tu penses que la décision était incorrecte. Nous examinerons ton appel et te répondrons avec notre décision.\n\n– Équipe de sécurité de {product_name}"
	},
	"donation_confirmation": {
		"subject": "Merci pour ton don à {product_name}",
		"body": "Bonjour,\n\nMerci pour ton don à {product_name} ! Ton {interval, select,\n  month {don récurrent}\n  year {don récurrent}\n  other {don unique}\n} a été {interval, select,\n  month {mis en place}\n  year {mis en place}\n  other {traité}\n} avec succès.\n\nDétails du don :\nMontant : {amount} {currency} {interval, select,\n  month {par mois}\n  year {par an}\n  other {}\n}\n\nStripe t'enverra sous peu un reçu séparé avec ta facture PDF. Cela inclut tous les détails de paiement et peut être utilisé à des fins fiscales.\n\nTu peux consulter l'historique de tes dons, télécharger des factures, {interval, select,\n  month {et gérer ou annuler ton abonnement}\n  year {et gérer ou annuler ton abonnement}\n  other {et gérer les futurs dons}\n} à tout moment en utilisant ce lien :\n\n{manageUrl}\n\nTon soutien aide à maintenir {product_name} en fonctionnement. Merci !\n\n– L'équipe {product_name}"
	},
	"donation_magic_link": {
		"subject": "Gérer tes dons {product_name}",
		"body": "Bonjour,\n\nClique sur le lien ci-dessous pour accéder à ton portail de donateur :\n\n{manageUrl}\n\nDans le portail, tu peux gérer tes abonnements, télécharger des factures et consulter l'historique de tes dons.\n\nCe lien expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi tu n'as pas demandé ce lien, tu peux ignorer cet e-mail en toute sécurité.\n\n– L'équipe {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Vérifie ton e-mail pour un signalement DSA",
		"body": "Bonjour,\n\nUtilise le code de vérification ci-dessous pour soumettre ton signalement DSA sur {product_name} :\n\n{code}\n\nCe code expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi tu n'as pas demandé cela, tu peux ignorer cet e-mail.\n\n– Équipe de sécurité de {product_name}"
	},
	"email_change_new": {
		"subject": "Vérifie ta nouvelle adresse e-mail {product_name}",
		"body": "Bonjour {username},\n\nSaisis ce code dans l'application pour vérifier ta nouvelle adresse e-mail {product_name} :\n\n{code}\n\nCe code expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi tu n'as pas demandé cela, tu peux ignorer cet e-mail.\n\n– L'équipe {product_name}"
	},
	"email_change_original": {
		"subject": "Confirme ton changement d'e-mail {product_name}",
		"body": "Bonjour {username},\n\nNous avons reçu une demande de modification de l'adresse e-mail de ton compte {product_name}.\n\nPour confirmer ce changement, saisis ce code dans l'application :\n\n{code}\n\nCe code expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi tu n'as pas demandé cela, sécurise ton compte immédiatement.\n\n– L'équipe {product_name}"
	},
	"email_change_revert": {
		"subject": "Ton e-mail {product_name} a été modifié",
		"body": "Bonjour {username},\n\nL'adresse e-mail de ton compte {product_name} a été modifiée pour {newEmail}.\n\nSi tu as effectué ce changement, aucune action n'est nécessaire. Si ce n'est pas le cas, tu peux annuler le changement et sécuriser ton compte en utilisant ce lien :\n\n{revertUrl}\n\nCela restaurera ton e-mail précédent, te déconnectera de toutes les sessions, supprimera les numéros de téléphone liés, désactivera l'authentification multifacteur et t'obligera à définir un nouveau mot de passe.\n\n– Équipe de sécurité de {product_name}"
	},
	"email_verification": {
		"subject": "Vérifie ton adresse e-mail {product_name}",
		"body": "Bonjour {username},\n\nVérifie l'adresse e-mail de ton compte {product_name} en cliquant sur le lien ci-dessous :\n\n{verifyUrl}\n\nSi tu n'as pas créé de compte {product_name}, tu peux ignorer cet e-mail en toute sécurité.\n\nCe lien est valide pendant 24 heures.\n\n– L'équipe {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Les avantages de ton cadeau échangé ont été supprimés",
		"body": "Bonjour {username},\n\nUn code cadeau que tu as échangé a été initialement payé par quelqu'un d'autre. Ce paiement a depuis été annulé (une rétrofacturation).\n\nPour cette raison, nous avons supprimé les avantages qui ont été ajoutés à ton compte lorsque tu as échangé le cadeau.\n\nSi tu penses qu'il s'agit d'une erreur, contacte notre équipe d'assistance et inclus tous les détails que tu as concernant le code cadeau et le moment où tu l'as échangé.\n\n– L'équipe {product_name}"
	},
	"harvest_completed": {
		"subject": "Ton export de données {product_name} est prêt à être téléchargé",
		"body": "Bonjour {username},\n\nTon export de données est prêt.\n\nLien de téléchargement :\n{downloadUrl}\n\nMessages inclus : {totalMessages, number}\nTaille du fichier : {fileSizeMB, number} Mo\n\nCe lien expire le {expiresAt, date, full} à {expiresAt, time, short}.\n\nSi tu n'as pas demandé cet export, change ton mot de passe immédiatement et contacte notre équipe d'assistance.\n\n– L'équipe {product_name}"
	},
	"inactivity_warning": {
		"subject": "Ton compte {product_name} sera supprimé en raison d'inactivité",
		"body": "Bonjour {username},\n\nNous n'avons détecté aucune activité sur ton compte {product_name} depuis le {lastActiveDate, date, full}.\n\nSi tu ne te connectes pas avant le {deletionDate, date, full} à {deletionDate, time, short}, ton compte sera définitivement supprimé en raison d'inactivité.\n\nConnecte-toi ici :\n{loginUrl}\n\nSi tu as utilisé {product_name} récemment, contacte notre équipe d'assistance immédiatement.\n\n– L'équipe {product_name}"
	},
	"ip_authorization": {
		"subject": "Autoriser la connexion depuis une nouvelle adresse IP",
		"body": "Bonjour {username},\n\nNous avons détecté une tentative de connexion à ton compte {product_name} depuis une nouvelle adresse IP :\n\nAdresse IP : {ipAddress}\nLocalisation : {location}\n\nSi c'était toi, autorise cette adresse IP en cliquant sur le lien ci-dessous :\n\n{authUrl}\n\nSi tu n'as pas tenté de te connecter, change ton mot de passe immédiatement.\n\nCe lien est valide pendant 30 minutes.\n\n– L'équipe {product_name}"
	},
	"password_change_verification": {
		"subject": "Confirme la modification de ton mot de passe {product_name}",
		"body": "Bonjour {username},\n\nNous avons reçu une demande de modification du mot de passe de ton compte {product_name}.\n\nPour confirmer cette modification, saisis ce code dans l'application :\n\n{code}\n\nCe code expire à {expiresAt}.\n\nSi tu n'as pas demandé cela, quelqu'un a peut-être eu accès à ton compte. Change ton mot de passe immédiatement et active l'authentification à deux facteurs.\n\n– L'équipe {product_name}"
	},
	"password_reset": {
		"subject": "Réinitialiser ton mot de passe {product_name}",
		"body": "Bonjour {username},\n\nTu as demandé une réinitialisation de ton mot de passe {product_name}. Utilise le lien ci-dessous pour définir un nouveau mot de passe :\n\n{resetUrl}\n\nSi tu n'as pas demandé cela, tu peux ignorer cet e-mail en toute sécurité.\n\nCe lien est valide pendant 1 heure.\n\n– L'équipe {product_name}"
	},
	"registration_approved": {
		"subject": "Ton inscription {product_name} a été approuvée",
		"body": "Bonjour {username},\n\nBonne nouvelle : ton inscription {product_name} a été approuvée.\n\nTu peux maintenant te connecter à l'application {product_name} ici :\n{channelsUrl}\n\nBienvenue dans la communauté {product_name}.\n\n– L'équipe {product_name}"
	},
	"report_resolved": {
		"subject": "Ton signalement {product_name} a été examiné",
		"body": "Bonjour {username},\n\nTon signalement (ID : {reportId}) a été examiné par notre équipe de sécurité.{hasComment, select, yes {\n\nRéponse de l'équipe de sécurité :\n{publicComment}} other {}}\n\nMerci de nous aider à préserver la sécurité de {product_name} pour tous. Nous prenons tous les signalements au sérieux et apprécions ta contribution à la communauté.\n\nSi tu as des questions ou des préoccupations concernant ce résultat, contacte {safety_email}.\n\n– Équipe de sécurité de {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Ton compte {product_name} sera définitivement supprimé",
		"body": "Bonjour {username},\n\nLa suppression définitive de ton compte {product_name} a été programmée.\n\nSuppression programmée : {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Raison : {reason}}\n}\n\nIl s'agit d'une mesure sérieuse. Les données de ton compte seront définitivement supprimées à la date prévue.\n\nSi tu penses que cette décision était incorrecte, tu peux soumettre un appel. Envoie un e-mail à {appeals_email} depuis cette adresse e-mail.\n\n– Équipe de sécurité de {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "La suppression de ton compte {product_name} est programmée",
		"body": "Bonjour {username},\n\nTu as demandé la suppression de ton compte {product_name}. Ton compte est programmé pour une suppression définitive le :\n\n{deletionDate, date, full} à {deletionDate, time, short}\n\nSi tu n'as pas demandé cela, connecte-toi à ton compte pour annuler la suppression. Nous te recommandons également de changer ton mot de passe pour sécuriser ton compte.\n\n– L'équipe {product_name}"
	},
	"unban_notification": {
		"subject": "La suspension de ton compte {product_name} a été levée",
		"body": "Bonjour {username},\n\nBonne nouvelle : la suspension de ton compte {product_name} a été levée.\n\n{reason, select,\n  null {}\n  other {Raison : {reason}}\n}\n\nTu peux maintenant te reconnecter et continuer à utiliser {product_name} normalement.\n\n– Équipe de sécurité de {product_name}"
	}
});

export default EMAIL_I18N_FR_MESSAGES;
