// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_SV_SE_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "Okänd plats",
	"billing.donation_description_monthly": "Månadsdonation till stöd för {product_name}",
	"billing.donation_description_one_time": "Engångsdonation till stöd för {product_name}",
	"billing.donation_description_yearly": "Årsdonation till stöd för {product_name}",
	"billing.donation_name_one_time": "Donation till {product_name}",
	"billing.donation_name_recurring": "Återkommande donation till {product_name}",
	"billing.eu_withdrawal_waiver_checkout": "Om jag är konsument i EU/EES samtycker jag uttryckligen till att det digitala innehållet i {product_name} {premium_tier_name} tillhandahålls omedelbart och bekräftar att jag förlorar min lagstadgade ångerrätt när jag får tillgång till innehållet. Detta påverkar inte andra tvingande konsumenträttigheter. Se [användarvillkoren]({terms_url}).",
	"bulk_message_deletion.complete": "Vi har raderat dina meddelanden. Vi tog bort {message_count, plural, =0 {0 meddelanden} one {# meddelande} other {# meddelanden}} från {channel_count, plural, =0 {0 platser} one {# plats} other {# platser}}.",
	"content.virus_detected": "Filen flaggades som potentiellt osäker och har tagits bort.",
	"guild.bulk_create.emoji_limit": "Maximalt antal emojis nått ({limit}).",
	"guild.bulk_create.sticker_limit": "Maximalt antal klistermärken nått ({limit}).",
	"guild.bulk_create.unknown_error": "Okänt fel.",
	"guild.default_category_text": "Textkanaler",
	"guild.default_category_voice": "Röstkanaler",
	"guild.default_channel_text": "allmänt",
	"guild.default_channel_voice": "Allmänt"
});

export default CONTENT_I18N_SV_SE_MESSAGES;
