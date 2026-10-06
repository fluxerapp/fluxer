// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_CS_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "Neznámá poloha",
	"billing.donation_description_monthly": "Měsíční dar na podporu {product_name}",
	"billing.donation_description_one_time": "Jednorázový dar na podporu {product_name}",
	"billing.donation_description_yearly": "Roční dar na podporu {product_name}",
	"billing.donation_name_one_time": "Dar pro {product_name}",
	"billing.donation_name_recurring": "Opakovaný dar pro {product_name}",
	"billing.eu_withdrawal_waiver_checkout": "Pokud jsem spotřebitelem z EU/EHP, výslovně souhlasím s tím, že digitální obsah {product_name} {premium_tier_name} bude poskytnut okamžitě, a beru na vědomí, že po poskytnutí přístupu ztrácím zákonné právo na odstoupení od smlouvy. Tím nejsou dotčena další povinná práva spotřebitele. Viz [Podmínky služby]({terms_url}).",
	"bulk_message_deletion.complete": "Dokončili jsme mazání tvých zpráv. Odstranili jsme {message_count, plural, =0 {0 zpráv} one {# zprávu} few {# zprávy} many {# zpráv} other {# zpráv}} z {channel_count, plural, =0 {0 míst} one {# místa} few {# míst} many {# míst} other {# míst}}.",
	"content.virus_detected": "Tento soubor byl označen jako potenciálně nebezpečný a byl odstraněn.",
	"guild.bulk_create.emoji_limit": "Byl dosažen maximální počet emoji ({limit}).",
	"guild.bulk_create.sticker_limit": "Byl dosažen maximální počet nálepek ({limit}).",
	"guild.bulk_create.unknown_error": "Nastala neznámá chyba.",
	"guild.default_category_text": "Textové kanály",
	"guild.default_category_voice": "Hlasové kanály",
	"guild.default_channel_text": "obecný",
	"guild.default_channel_voice": "Obecný"
});

export default CONTENT_I18N_CS_MESSAGES;
