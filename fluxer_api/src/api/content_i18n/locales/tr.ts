// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_TR_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "Bilinmeyen konum",
	"billing.donation_description_monthly": "{product_name} için aylık destek bağışı",
	"billing.donation_description_one_time": "{product_name} için tek seferlik destek bağışı",
	"billing.donation_description_yearly": "{product_name} için yıllık destek bağışı",
	"billing.donation_name_one_time": "{product_name} bağışı",
	"billing.donation_name_recurring": "Yinelenen {product_name} bağışı",
	"billing.eu_withdrawal_waiver_checkout": "AB/AEA tüketicisiysem, {product_name} {premium_tier_name} dijital içeriğinin hemen sağlanmasına açıkça onay veriyorum ve erişim sağlandıktan sonra yasal cayma hakkımı kaybettiğimi kabul ediyorum. Bu, diğer zorunlu tüketici haklarını etkilemez. [Hizmet şartları]({terms_url}) sayfasına bakın.",
	"bulk_message_deletion.complete": "Mesajlarını silme işlemini tamamladık. {channel_count, plural, =0 {0 yerden} one {# yerden} other {# yerden}} {message_count, plural, =0 {0 mesaj} one {# mesaj} other {# mesaj}} kaldırdık.",
	"content.virus_detected": "Bu dosya potansiyel olarak güvensiz olarak işaretlendi ve kaldırıldı.",
	"guild.bulk_create.emoji_limit": "Maksimum emoji sayısına ulaşıldı ({limit}).",
	"guild.bulk_create.sticker_limit": "Maksimum çıkartma sayısına ulaşıldı ({limit}).",
	"guild.bulk_create.unknown_error": "Bilinmeyen bir hata oluştu.",
	"guild.default_category_text": "Metin kanalları",
	"guild.default_category_voice": "Sesli kanallar",
	"guild.default_channel_text": "genel",
	"guild.default_channel_voice": "Genel"
});

export default CONTENT_I18N_TR_MESSAGES;
