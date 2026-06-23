// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_TH_MESSAGES = defineContentI18nLocaleMessages({
	"billing.eu_withdrawal_waiver_checkout": "หากฉันเป็นผู้บริโภคใน EU/EEA ฉันยินยอมให้มีการให้บริการเนื้อหาดิจิทัล {product_name} {premium_tier_name} ทันที และรับทราบว่าฉันสูญเสียสิทธิ์ในการถอนตัวตามกฎหมายเมื่อมีการให้สิทธิ์การเข้าถึงแล้ว ซึ่งไม่กระทบต่อสิทธิ์ผู้บริโภคอื่นๆ ที่จำเป็น ดู[ข้อกำหนดในการให้บริการ]({terms_url})",
	"bulk_message_deletion.complete": "ลบข้อความของคุณเรียบร้อยแล้ว เราลบ {message_count, plural, =0 {0 ข้อความ} other {# ข้อความ}} จาก {channel_count, plural, =0 {0 ที่} other {# ที่}}",
	"content.virus_detected": "ไฟล์นั้นถูกตรวจพบว่าอาจไม่ปลอดภัยและถูกลบออกแล้ว",
	"guild.bulk_create.emoji_limit": "จำนวนอีโมจิสูงสุดที่อนุญาตคือ {limit}",
	"guild.bulk_create.sticker_limit": "จำนวนสติกเกอร์สูงสุดที่สามารถเพิ่มได้คือ {limit}",
	"guild.bulk_create.unknown_error": "เกิดข้อผิดพลาดที่ไม่ทราบสาเหตุ",
	"guild.default_category_text": "ช่องข้อความ",
	"guild.default_category_voice": "ช่องเสียง",
	"guild.default_channel_text": "ทั่วไป",
	"guild.default_channel_voice": "ทั่วไป"
});

export default CONTENT_I18N_TH_MESSAGES;
