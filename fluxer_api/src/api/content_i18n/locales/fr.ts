// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_FR_MESSAGES = defineContentI18nLocaleMessages({
	"billing.eu_withdrawal_waiver_checkout": "En tant que consommateur UE/EEE, je consens expressément à ce que le contenu numérique {product_name} {premium_tier_name} soit fourni immédiatement et reconnais perdre mon droit de rétractation légal dès l'accès fourni. Cela n'affecte pas les autres droits impératifs des consommateurs. Voir les [Conditions d'utilisation]({terms_url}).",
	"bulk_message_deletion.complete": "Nous avons terminé la suppression de tes messages. Nous avons retiré {message_count, plural, =0 {0 message} one {# message} other {# messages}} dans {channel_count, plural, =0 {0 endroit} one {# endroit} other {# endroits}}.",
	"content.virus_detected": "Ce fichier a été signalé comme potentiellement dangereux et a été supprimé.",
	"guild.bulk_create.emoji_limit": "Nombre maximal d’émojis atteint ({limit}).",
	"guild.bulk_create.sticker_limit": "Nombre maximal d’autocollants atteint ({limit}).",
	"guild.bulk_create.unknown_error": "Erreur inconnue.",
	"guild.default_category_text": "Salons textuels",
	"guild.default_category_voice": "Salons vocaux",
	"guild.default_channel_text": "général",
	"guild.default_channel_voice": "Général"
});

export default CONTENT_I18N_FR_MESSAGES;
