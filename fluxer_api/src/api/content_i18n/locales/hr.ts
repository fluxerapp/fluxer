// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_HR_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "Nepoznata lokacija",
	"billing.donation_description_monthly": "Mjesečna donacija za podršku {product_name}u",
	"billing.donation_description_one_time": "Jednokratna donacija za podršku {product_name}u",
	"billing.donation_description_yearly": "Godišnja donacija za podršku {product_name}u",
	"billing.donation_name_one_time": "Donacija za {product_name}",
	"billing.donation_name_recurring": "Ponavljajuća donacija za {product_name}",
	"billing.eu_withdrawal_waiver_checkout": "Ako sam potrošač iz EU-a/EGP-a, izričito pristajem na trenutačnu isporuku digitalnog sadržaja {product_name} pretplate {premium_tier_name} i potvrđujem da gubim zakonsko pravo na odustanak čim mi se omogući pristup. To ne utječe na ostala obvezna potrošačka prava. Pogledaj [Uvjete pružanja usluge]({terms_url}).",
	"bulk_message_deletion.complete": "Završili smo s brisanjem tvojih poruka. Uklonili smo {message_count, plural, =0 {0 poruka} one {# poruku} few {# poruke} other {# poruka}} iz {channel_count, plural, =0 {0 mjesta} one {# mjesta} few {# mjesta} other {# mjesta}}.",
	"content.virus_detected": "Ta je datoteka označena kao potencijalno nesigurna i uklonjena je.",
	"guild.bulk_create.emoji_limit": "Dostignut maksimalan broj emojija ({limit}).",
	"guild.bulk_create.sticker_limit": "Dostignut maksimalan broj naljepnica ({limit}).",
	"guild.bulk_create.unknown_error": "Nepoznata pogreška.",
	"guild.default_category_text": "Tekstualni kanali",
	"guild.default_category_voice": "Glasovni kanali",
	"guild.default_channel_text": "općenito",
	"guild.default_channel_voice": "Općenito"
});

export default CONTENT_I18N_HR_MESSAGES;
