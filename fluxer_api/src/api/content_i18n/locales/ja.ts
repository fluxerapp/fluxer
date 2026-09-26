// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_JA_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "不明な場所",
	"billing.donation_description_monthly": "{product_name}を支援する毎月の寄付",
	"billing.donation_description_one_time": "{product_name}を支援する1回のみの寄付",
	"billing.donation_description_yearly": "{product_name}を支援する毎年の寄付",
	"billing.donation_name_one_time": "{product_name}への寄付",
	"billing.donation_name_recurring": "{product_name}への定期寄付",
	"billing.eu_withdrawal_waiver_checkout": "私がEU/EEAの消費者である場合、{product_name} {premium_tier_name} のデジタルコンテンツが直ちに提供されることに明示的に同意し、アクセスが提供された時点で法定の撤回権を失うことを認めます。これは、法律によって保障されるその他の消費者の権利に影響を与えません。[利用規約]({terms_url})をご確認ください。",
	"bulk_message_deletion.complete": "メッセージの削除が完了しました。{channel_count, plural, =0 {0 件の場所から} other {# 件の場所から}}{message_count, plural, =0 {0 件のメッセージを削除しました} other {# 件のメッセージを削除しました}}。",
	"content.virus_detected": "このファイルは安全でない可能性があると判断されたため、削除されました。",
	"guild.bulk_create.emoji_limit": "絵文字の上限に達しました（{limit}）。",
	"guild.bulk_create.sticker_limit": "ステッカーの上限に達しました（{limit}）。",
	"guild.bulk_create.unknown_error": "不明なエラーが発生しました。",
	"guild.default_category_text": "テキストチャンネル",
	"guild.default_category_voice": "ボイスチャンネル",
	"guild.default_channel_text": "一般",
	"guild.default_channel_voice": "一般"
});

export default CONTENT_I18N_JA_MESSAGES;
