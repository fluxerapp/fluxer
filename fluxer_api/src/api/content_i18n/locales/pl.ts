// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_PL_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "Nieznana lokalizacja",
	"billing.donation_description_monthly": "Miesięczna darowizna na wsparcie {product_name}",
	"billing.donation_description_one_time": "Jednorazowa darowizna na wsparcie {product_name}",
	"billing.donation_description_yearly": "Roczna darowizna na wsparcie {product_name}",
	"billing.donation_name_one_time": "Darowizna na rzecz {product_name}",
	"billing.donation_name_recurring": "Cykliczna darowizna na rzecz {product_name}",
	"billing.eu_withdrawal_waiver_checkout": "Jeśli jestem konsumentem z UE/EOG, wyraźnie zgadzam się na natychmiastowe dostarczenie treści cyfrowych {product_name} {premium_tier_name} i przyjmuję do wiadomości, że po uzyskaniu dostępu tracę ustawowe prawo do odstąpienia od umowy. Nie wpływa to na inne prawa konsumenta wynikające z bezwzględnie obowiązujących przepisów. Zobacz [Warunki użytkowania]({terms_url}).",
	"bulk_message_deletion.complete": "Zakończyliśmy usuwanie Twoich wiadomości. Usunęliśmy {message_count, plural, =0 {0 wiadomości} one {# wiadomość} few {# wiadomości} many {# wiadomości} other {# wiadomości}} z {channel_count, plural, =0 {0 miejsc} one {# miejsca} few {# miejsc} many {# miejsc} other {# miejsc}}.",
	"content.virus_detected": "Ten plik został oznaczony jako potencjalnie niebezpieczny i został usunięty.",
	"guild.bulk_create.emoji_limit": "Osiągnięto maksymalną liczbę emoji ({limit}).",
	"guild.bulk_create.sticker_limit": "Osiągnięto maksymalną liczbę naklejek ({limit}).",
	"guild.bulk_create.unknown_error": "Wystąpił nieznany błąd.",
	"guild.default_category_text": "Kanały tekstowe",
	"guild.default_category_voice": "Kanały głosowe",
	"guild.default_channel_text": "ogólny",
	"guild.default_channel_voice": "Ogólny"
});

export default CONTENT_I18N_PL_MESSAGES;
