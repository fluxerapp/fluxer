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

export const fr: EmailTranslations = {
	passwordReset: {
		subject: 'Réinitialisez votre mot de passe Fluxer',
		body: `Bonjour {username},

Vous avez demandé à réinitialiser le mot de passe de votre compte Fluxer. Veuillez suivre le lien ci-dessous pour définir un nouveau mot de passe :

{resetUrl}

Si vous n'êtes pas à l'origine de cette demande, vous pouvez ignorer cet e-mail en toute sécurité.

Ce lien expirera dans 1 heure.

- L'équipe Fluxer`,
	},
	emailVerification: {
		subject: 'Vérifiez votre adresse e-mail Fluxer',
		body: `Bonjour {username},

Veuillez vérifier l'adresse e-mail associée à votre compte Fluxer en cliquant sur le lien ci-dessous :

{verifyUrl}

Si vous n'avez pas créé de compte Fluxer, vous pouvez ignorer cet e-mail en toute sécurité.

Ce lien expirera dans 24 heures.

- L'équipe Fluxer`,
	},
	ipAuthorization: {
		subject: 'Autorisez une connexion depuis une nouvelle adresse IP',
		body: `Bonjour {username},

Nous avons détecté une tentative de connexion à votre compte Fluxer depuis une nouvelle adresse IP :

Adresse IP : {ipAddress}
Localisation : {location}

Si c'était bien vous, veuillez autoriser cette adresse IP en cliquant sur le lien ci-dessous :

{authUrl}

Si vous n'avez pas tenté de vous connecter, veuillez modifier votre mot de passe immédiatement.

Ce lien d'autorisation expirera dans 30 minutes.

- L'équipe Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Votre compte Fluxer a été temporairement désactivé',
		body: `Bonjour {username},

Votre compte Fluxer a été temporairement désactivé en raison d'une activité suspecte.

{reason, select,
	null {}
	other {Raison : {reason}

}}Pour retrouver l'accès à votre compte, vous devez réinitialiser votre mot de passe :

{forgotUrl}

Après avoir réinitialisé votre mot de passe, vous pourrez vous reconnecter.

Si vous pensez qu'il s'agit d'une erreur, veuillez contacter notre équipe d'assistance.

- L'équipe Sécurité Fluxer`,
	},
	accountTempBanned: {
		subject: 'Votre compte Fluxer a été temporairement suspendu',
		body: `Bonjour {username},

Votre compte Fluxer a été temporairement suspendu pour non-respect de nos Conditions d'utilisation ou de nos Règles communautaires.

Durée : {durationHours, plural,
	=1 {1 heure}
	other {# heures}
}
Suspendu jusqu'au : {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Raison : {reason}}
}

Pendant cette période, vous ne pourrez pas accéder à votre compte.

Nous vous invitons à consulter :
- Conditions d'utilisation : {termsUrl}
- Règles communautaires : {guidelinesUrl}

Si vous estimez que cette décision est incorrecte ou injustifiée, vous pouvez envoyer une demande d'appel à appeals@fluxer.app depuis cette adresse e-mail. Veuillez expliquer clairement pourquoi vous pensez que la décision est erronée. Nous étudierons votre appel et reviendrons vers vous avec notre décision.

- L'équipe Sécurité Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Votre compte Fluxer est programmé pour suppression',
		body: `Bonjour {username},

Votre compte Fluxer a été programmé pour suppression définitive en raison d'infractions à nos Conditions d'utilisation ou Règles communautaires.

Date de suppression prévue : {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Raison : {reason}}
}

Ceci est une mesure disciplinaire sérieuse. Vos données seront supprimées définitivement à la date prévue.

Nous vous invitons à consulter :
- Conditions d'utilisation : {termsUrl}
- Règles communautaires : {guidelinesUrl}

PROCÉDURE D'APPEL :
Si vous estimez que cette décision est incorrecte ou injustifiée, vous disposez de 30 jours pour envoyer un appel à appeals@fluxer.app depuis cette adresse.

Dans votre appel :
- Expliquez clairement pourquoi vous estimez que la décision est incorrecte
- Fournissez tout élément pertinent ou tout contexte utile

Un membre de l'équipe Sécurité Fluxer examinera votre demande et pourra suspendre la suppression jusqu'à ce qu'une décision finale soit prise.

- L'équipe Sécurité Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'La suppression de votre compte Fluxer a été planifiée',
		body: `Bonjour {username},

Nous sommes désolés de vous voir partir ! La suppression de votre compte Fluxer a été programmée.

Date prévue de suppression : {deletionDate, date, full} {deletionDate, time, short}

IMPORTANT : Vous pouvez annuler cette suppression à tout moment avant le {deletionDate, date, full} {deletionDate, time, short} en vous reconnectant simplement à votre compte.

AVANT DE PARTIR :
Votre tableau de bord de confidentialité dans les paramètres utilisateur vous permet de :
- Supprimer vos messages sur la plateforme
- Exporter vos données importantes avant de partir

Veuillez noter : une fois votre compte supprimé, il ne sera plus possible de supprimer vos messages. Si vous souhaitez les effacer, faites-le via le tableau de bord de confidentialité avant la suppression définitive.

Si vous changez d'avis, reconnectez-vous simplement pour annuler la suppression.

- L'équipe Fluxer`,
	},
	inactivityWarning: {
		subject: 'Votre compte Fluxer sera supprimé pour inactivité',
		body: `Bonjour {username},

Nous avons remarqué que vous ne vous êtes pas connecté à votre compte Fluxer depuis plus de 2 ans.

Dernière connexion : {lastActiveDate, date, full} {lastActiveDate, time, short}

Dans le cadre de notre politique de conservation des données, les comptes inactifs sont automatiquement programmés pour suppression. Votre compte sera supprimé définitivement le :

Date prévue de suppression : {deletionDate, date, full} {deletionDate, time, short}

COMMENT GARDER VOTRE COMPTE :
Il vous suffit de vous connecter à votre compte à {loginUrl} avant la date de suppression pour annuler cette suppression automatique. Aucune autre action n'est nécessaire.

SI VOUS NE VOUS CONNECTEZ PAS :
- Votre compte et toutes les données associées seront supprimés définitivement
- Vos messages seront rendus anonymes (attribués à « Utilisateur supprimé »)
- Cette action est irréversible

VOUS SOUHAITEZ SUPPRIMER VOS MESSAGES ?
Si vous souhaitez effacer vos messages avant la suppression du compte, veuillez vous connecter et utiliser le tableau de bord de confidentialité.

Nous espérons vous revoir bientôt sur Fluxer !

- L'équipe Fluxer`,
	},
	harvestCompleted: {
		subject: 'Votre exportation de données Fluxer est prête',
		body: `Bonjour {username},

Votre exportation de données est terminée et prête à être téléchargée !

Résumé de l'export :
- Nombre total de messages : {totalMessages, number}
- Taille du fichier : {fileSizeMB} Mo
- Format : Archive ZIP contenant des fichiers JSON

Téléchargez vos données : {downloadUrl}

IMPORTANT : Ce lien expirera le {expiresAt, date, full} {expiresAt, time, short}

Contenu de l'export :
- Tous vos messages organisés par canal
- Métadonnées des canaux
- Votre profil utilisateur et informations de compte
- Vos appartenances et paramètres de serveurs (guildes)
- Vos sessions d'authentification et informations de sécurité

Les données sont fournies au format JSON pour faciliter l'analyse.

Pour toute question, veuillez contacter support@fluxer.app

- L'équipe Fluxer`,
	},
	unbanNotification: {
		subject: 'La suspension de votre compte Fluxer a été levée',
		body: `Bonjour {username},

Bonne nouvelle ! La suspension de votre compte Fluxer a été levée.

Raison : {reason}

Vous pouvez désormais vous reconnecter et continuer à utiliser Fluxer.

- L'équipe Sécurité Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Votre compte Fluxer est programmé pour suppression',
		body: `Bonjour {username},

Votre compte Fluxer a été programmé pour suppression définitive.

Date prévue de suppression : {deletionDate, date, full} {deletionDate, time, short}
Raison : {reason}

Il s'agit d'une mesure disciplinaire sérieuse. Vos données de compte seront supprimées définitivement à la date indiquée.

Si vous pensez que cette décision est incorrecte, vous pouvez envoyer un appel à appeals@fluxer.app depuis cette adresse e-mail.

- L'équipe Sécurité Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Votre cadeau Fluxer Premium a été révoqué',
		body: `Bonjour {username},

Nous vous informons que le cadeau Fluxer Premium que vous avez utilisé a été révoqué à la suite d'un litige de paiement (chargeback) déposé par l'acheteur initial.

Vos avantages Premium ont été retirés de votre compte. Cette action a été effectuée car le paiement a été contesté et annulé.

Si vous avez des questions, veuillez contacter support@fluxer.app.

- L'équipe Fluxer`,
	},
	reportResolved: {
		subject: 'Votre signalement Fluxer a été examiné',
		body: `Bonjour {username},

Votre signalement (ID : {reportId}) a été examiné par notre équipe Sécurité.

Réponse de l'équipe :
{publicComment}

Merci d'aider à faire de Fluxer un espace sûr pour tous. Nous prenons tous les signalements au sérieux et apprécions votre contribution.

Si vous avez des questions ou des préoccupations concernant cette décision, veuillez contacter safety@fluxer.app.

- L'équipe Sécurité Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Vérifiez votre e-mail pour un signalement DSA',
		body: `Bonjour,

Utilisez le code de vérification suivant pour soumettre votre signalement conformément à la loi sur les services numériques (Digital Services Act) sur Fluxer :

{code}

Ce code expire le {expiresAt, date, full} {expiresAt, time, short}.

Si vous n'avez pas demandé cela, veuillez ignorer cet e-mail.

- L'équipe Sécurité Fluxer`,
	},
	registrationApproved: {
		subject: 'Votre inscription Fluxer a été approuvée',
		body: `Bonjour {username},

Bonne nouvelle ! Votre inscription à Fluxer a été approuvée.

Vous pouvez maintenant vous connecter à l'application Fluxer à l'adresse :
{channelsUrl}

Bienvenue dans la communauté Fluxer !

- L'équipe Fluxer`,
	},
	emailChangeRevert: {
		subject: 'Votre e-mail Fluxer a été modifié',
		body: `Bonjour {username},

L'adresse e-mail de votre compte Fluxer a été modifiée en {newEmail}.

Si vous êtes à l'origine de ce changement, vous n'avez rien à faire. Sinon, vous pouvez l'annuler et sécuriser votre compte avec ce lien :

{revertUrl}

Cela restaurera votre ancienne adresse e-mail, vous déconnectera partout, supprimera les numéros de téléphone associés, désactivera la MFA et exigera un nouveau mot de passe.

- Équipe Sécurité Fluxer`,
	},
};
