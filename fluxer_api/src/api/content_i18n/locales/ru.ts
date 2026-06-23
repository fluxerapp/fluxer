// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_RU_MESSAGES = defineContentI18nLocaleMessages({
	"billing.eu_withdrawal_waiver_checkout": "Если ты являешься потребителем из ЕС/ЕЭЗ, ты даёшь явное согласие на немедленное предоставление цифрового контента {product_name} {premium_tier_name} и понимаешь, что теряешь законное право на отмены после получения доступа. Это не затрагивает другие обязательные права потребителя. См. [Условия предоставления услуг]({terms_url}).",
	"bulk_message_deletion.complete": "Мы завершили удаление ваших сообщений. Удалено {message_count, plural, =0 {0 сообщений} one {# сообщение} few {# сообщения} many {# сообщений} other {# сообщений}} из {channel_count, plural, =0 {0 мест} one {# места} few {# мест} many {# мест} other {# мест}}.",
	"content.virus_detected": "Этот файл был отмечен как потенциально опасный и удалён.",
	"guild.bulk_create.emoji_limit": "Достигнут максимальный лимит эмодзи ({limit}).",
	"guild.bulk_create.sticker_limit": "Достигнут максимальный лимит стикеров ({limit}).",
	"guild.bulk_create.unknown_error": "Произошла неизвестная ошибка.",
	"guild.default_category_text": "Текстовые каналы",
	"guild.default_category_voice": "Голосовые каналы",
	"guild.default_channel_text": "общий",
	"guild.default_channel_voice": "Общий"
});

export default CONTENT_I18N_RU_MESSAGES;
