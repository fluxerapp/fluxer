// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_UK_MESSAGES = defineContentI18nLocaleMessages({
	"billing.eu_withdrawal_waiver_checkout": "Якщо я є споживачем з ЄС/ЄЕЗ, я надаю явну згоду на негайне надання цифрового контенту {product_name} {premium_tier_name} і визнаю, що втрачаю своє законне право на відмову після отримання доступу. Це не впливає на інші обов'язкові права споживача. Див. [Умови надання послуг]({terms_url}).",
	"bulk_message_deletion.complete": "Ми завершили видалення твоїх повідомлень. Видалено {message_count, plural, =0 {0 повідомлень} one {# повідомлення} few {# повідомлення} many {# повідомлень} other {# повідомлень}} з {channel_count, plural, =0 {0 місць} one {# місця} few {# місць} many {# місць} other {# місць}}.",
	"content.virus_detected": "Цей файл позначено як потенційно небезпечний, і його вилучено.",
	"guild.bulk_create.emoji_limit": "Досягнуто максимальної кількості емодзі ({limit}).",
	"guild.bulk_create.sticker_limit": "Досягнуто максимальну кількість стікерів ({limit}).",
	"guild.bulk_create.unknown_error": "Невідома помилка.",
	"guild.default_category_text": "Текстові канали",
	"guild.default_category_voice": "Голосові канали",
	"guild.default_channel_text": "загальний",
	"guild.default_channel_voice": "Загальний"
});

export default CONTENT_I18N_UK_MESSAGES;
