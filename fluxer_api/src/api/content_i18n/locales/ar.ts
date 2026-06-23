// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_AR_MESSAGES = defineContentI18nLocaleMessages({
	"billing.eu_withdrawal_waiver_checkout": "إذا كنت مستهلكًا في الاتحاد الأوروبي/المنطقة الاقتصادية الأوروبية، فإنني أوافق صراحةً على تقديم المحتوى الرقمي لـ {product_name} {premium_tier_name} فورًا، وأقرّ بأنني أفقد حقي القانوني في الانسحاب بمجرد تقديم الوصول. لا يؤثر هذا على حقوق المستهلك الإلزامية الأخرى. اطّلع على [شروط الخدمة]({terms_url}).",
	"bulk_message_deletion.complete": "انتهينا من حذف رسائلك. أزلنا {message_count, plural, =0 {0 رسالة} one {رسالة واحدة} two {رسالتين} few {# رسائل} many {# رسالة} other {# رسالة}} من {channel_count, plural, =0 {0 مكان} one {مكان واحد} two {مكانين} few {# أماكن} many {# مكانًا} other {# مكان}}.",
	"content.virus_detected": "تم الإبلاغ عن هذا الملف على أنه قد يكون غير آمن وتمت إزالته.",
	"guild.bulk_create.emoji_limit": "تم الوصول إلى الحد الأقصى لعدد الرموز التعبيرية ({limit}).",
	"guild.bulk_create.sticker_limit": "تم الوصول إلى الحد الأقصى لعدد الملصقات ({limit}).",
	"guild.bulk_create.unknown_error": "حدث خطأ غير معروف.",
	"guild.default_category_text": "القنوات النصية",
	"guild.default_category_voice": "القنوات الصوتية",
	"guild.default_channel_text": "عام",
	"guild.default_channel_voice": "عام"
});

export default CONTENT_I18N_AR_MESSAGES;
