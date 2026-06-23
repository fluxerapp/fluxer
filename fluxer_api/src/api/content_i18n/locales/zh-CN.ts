// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_ZH_CN_MESSAGES = defineContentI18nLocaleMessages({
	"billing.eu_withdrawal_waiver_checkout": "如果我是欧盟/欧洲经济区消费者，我明确同意立即提供 {product_name} {premium_tier_name} 数字内容，并知晓一旦获得访问权限，我将失去法定撤销权。这不影响其他强制性消费者权利。请参阅[服务条款]({terms_url})。",
	"bulk_message_deletion.complete": "我们已删除你的消息。从 {channel_count, plural, =0 {0 个位置} other {# 个位置}} 中移除了 {message_count, plural, =0 {0 条消息} other {# 条消息}}。",
	"content.virus_detected": "该文件被标记为可能存在风险，已被移除。",
	"guild.bulk_create.emoji_limit": "已达到最大表情符号数量 ({limit})。",
	"guild.bulk_create.sticker_limit": "已达到最大贴纸数量 ({limit})。",
	"guild.bulk_create.unknown_error": "发生未知错误。",
	"guild.default_category_text": "文字频道",
	"guild.default_category_voice": "语音频道",
	"guild.default_channel_text": "综合",
	"guild.default_channel_voice": "综合"
});

export default CONTENT_I18N_ZH_CN_MESSAGES;
