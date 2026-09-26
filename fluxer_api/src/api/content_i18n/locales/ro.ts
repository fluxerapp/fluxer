// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_RO_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "Locație necunoscută",
	"billing.donation_description_monthly": "Donație lunară pentru susținerea {product_name}",
	"billing.donation_description_one_time": "Donație unică pentru susținerea {product_name}",
	"billing.donation_description_yearly": "Donație anuală pentru susținerea {product_name}",
	"billing.donation_name_one_time": "Donație pentru {product_name}",
	"billing.donation_name_recurring": "Donație recurentă pentru {product_name}",
	"billing.eu_withdrawal_waiver_checkout": "Dacă sunt un consumator din UE/SEE, îmi exprim consimțământul expres ca conținutul digital {product_name} {premium_tier_name} să fie furnizat imediat și recunosc că pierd dreptul legal de retragere odată ce accesul este acordat. Acest lucru nu afectează alte drepturi obligatorii ale consumatorilor. Vezi [Termenii și condițiile]({terms_url}).",
	"bulk_message_deletion.complete": "Am terminat de șters mesajele tale. Am eliminat {message_count, plural, =0 {0 mesaje} one {# mesaj} few {# mesaje} other {# de mesaje}} din {channel_count, plural, =0 {0 locuri} one {# loc} few {# locuri} other {# de locuri}}.",
	"content.virus_detected": "Acest fișier a fost marcat ca potențial nesigur și a fost eliminat.",
	"guild.bulk_create.emoji_limit": "S-a atins numărul maxim de emojiuri ({limit}).",
	"guild.bulk_create.sticker_limit": "S-a atins numărul maxim de stickere ({limit}).",
	"guild.bulk_create.unknown_error": "A apărut o eroare necunoscută.",
	"guild.default_category_text": "Canale text",
	"guild.default_category_voice": "Canale de voce",
	"guild.default_channel_text": "general",
	"guild.default_channel_voice": "General"
});

export default CONTENT_I18N_RO_MESSAGES;
