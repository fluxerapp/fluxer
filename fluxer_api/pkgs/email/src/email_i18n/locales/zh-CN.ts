// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_ZH_CN_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "你的 {product_name} 账号已被暂时禁用",
		"body": "你好 {username}，\n\n我们检测到你的 {product_name} 账号存在可疑活动，因此暂时禁用了你的账号。\n\n{reason, select,\n  null {}\n  other {原因： {reason}}\n}\n\n要重新获得账号访问权限，你需要重置密码：\n\n{forgotUrl}\n\n重置密码后，你将能够再次登录。\n\n如果你认为这是误操作，请联系我们的支持团队。\n\n– {product_name} 安全团队"
	},
	"account_scheduled_deletion": {
		"subject": "你的 {product_name} 账号将被永久删除",
		"body": "你好 {username}，\n\n由于违反了我们的《服务条款》或《社区准则》，你的 {product_name} 账号已被安排永久删除。\n\n计划删除时间：{deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {原因： {reason}}\n}\n\n这是一项严重的强制措施。你的账号数据将在预定日期被永久删除。\n\n请查阅：\n- 服务条款：{termsUrl}\n- 社区准则：{guidelinesUrl}\n\n申诉流程：\n如果你认为此强制决定不正确或不合理，你有 60 天时间提交申诉。请使用此邮箱地址发送邮件至 {appeals_email}。\n\n在你的申诉中：\n- 清楚说明你认为强制决定不正确或不合理的原因\n- 提供任何相关证据或背景信息\n\n{product_name} 安全团队成员将审核你的申诉，并可能暂停待处理的删除，直到最终决定做出。\n\n– {product_name} 安全团队"
	},
	"account_temp_banned": {
		"subject": "你的 {product_name} 账号已被暂时封禁",
		"body": "你好 {username}，\n\n你的 {product_name} 账号因违反我们的《服务条款》或《社区准则》已被暂时封禁。\n\n封禁时长：{durationHours, plural,\n  =1 {1 小时}\n  other {# 小时}\n}\n封禁至：{bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {原因： {reason}}\n}\n\n在此期间，你将无法访问你的账号。\n\n请查阅：\n- 服务条款：{termsUrl}\n- 社区准则：{guidelinesUrl}\n\n如果你认为此强制决定不正确或不合理，你可以提交申诉。请使用此邮箱地址发送邮件至 {appeals_email}，并清楚说明你认为该决定不正确的原因。我们将审核你的申诉并回复我们的决定。\n\n– {product_name} 安全团队"
	},
	"donation_confirmation": {
		"subject": "感谢你的 {product_name} 捐赠",
		"body": "你好，\n\n感谢你对 {product_name} 的捐赠！你的 {interval, select,\n  month {定期捐赠}\n  year {定期捐赠}\n  other {一次性捐赠}\n} {interval, select,\n  month {已成功设置}\n  year {已成功设置}\n  other {已成功处理}\n}。\n\n捐赠详情：\n金额：{amount} {currency} {interval, select,\n  month {每月}\n  year {每年}\n  other {}\n}\n\nStripe 将很快通过电子邮件向你发送包含发票 PDF 的单独收据。其中包含所有付款详情，可用于税务目的。\n\n你可以随时使用此链接查看你的捐赠历史、下载发票，以及{interval, select,\n  month {管理或取消你的订阅}\n  year {管理或取消你的订阅}\n  other {管理你的未来捐赠}\n}：\n\n{manageUrl}\n\n你的支持有助于 {product_name} 的持续运行。谢谢！\n\n– {product_name} 团队"
	},
	"donation_magic_link": {
		"subject": "管理你的 {product_name} 捐赠",
		"body": "你好，\n\n点击下方链接访问你的捐赠者门户：\n\n{manageUrl}\n\n在门户中，你可以管理订阅、下载发票和查看捐赠历史。\n\n此链接将于 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果你未请求此链接，可以安全忽略此邮件。\n\n– {product_name} 团队"
	},
	"dsa_report_verification": {
		"subject": "验证邮箱以提交 DSA 报告",
		"body": "你好，\n\n请使用以下验证码在 {product_name} 上提交你的数字服务法案报告：\n\n{code}\n\n此验证码将于 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果你未请求此操作，请忽略此邮件。\n\n– {product_name} 安全团队"
	},
	"email_change_new": {
		"subject": "验证你的新 {product_name} 邮箱",
		"body": "你好 {username}，\n\n请在应用中输入此验证码以验证你的新 {product_name} 邮箱：\n\n{code}\n\n此验证码将于 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果你未请求此操作，请忽略此邮件。\n\n– {product_name} 团队"
	},
	"email_change_original": {
		"subject": "确认你的 {product_name} 邮箱变更",
		"body": "你好 {username}，\n\n我们收到了更改你的 {product_name} 账号邮箱的请求。\n\n要确认此更改，请在应用中输入此验证码：\n\n{code}\n\n此验证码将于 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果你未请求此操作，请立即保护账号安全。\n\n– {product_name} 团队"
	},
	"email_change_revert": {
		"subject": "你的 {product_name} 邮箱已变更",
		"body": "你好 {username}，\n\n你的 {product_name} 账号邮箱已变更为 {newEmail}。\n\n如果你进行了此更改，则无需采取任何操作。如果你没有进行此更改，请使用此链接撤销更改并保护账号安全：\n\n{revertUrl}\n\n这将恢复你之前的邮箱，将所有设备退出登录，移除关联的电话号码，禁用多重身份验证，并要求你设置新密码。\n\n– {product_name} 安全团队"
	},
	"email_verification": {
		"subject": "验证你的 {product_name} 邮箱地址",
		"body": "你好 {username}，\n\n请点击下方链接验证你的 {product_name} 账号邮箱地址：\n\n{verifyUrl}\n\n如果你未创建 {product_name} 账号，请忽略此邮件。\n\n此链接 24 小时内有效。\n\n– {product_name} 团队"
	},
	"gift_chargeback_notification": {
		"subject": "已移除你兑换礼品所获得的特权",
		"body": "你好 {username}，\n\n你兑换的礼品码最初由他人支付。该付款现已被撤销（退单）。\n\n因此，我们已移除你在兑换礼品时添加到你账号的特权。\n\n如果你认为此操作有误，请联系我们的支持团队，并提供有关礼品码以及兑换时间的任何详细信息。\n\n– {product_name} 团队"
	},
	"harvest_completed": {
		"subject": "你的 {product_name} 数据导出已准备就绪",
		"body": "你好 {username}，\n\n你的数据导出已准备就绪。\n\n下载链接：\n{downloadUrl}\n\n包含消息数：{totalMessages, number}\n文件大小：{fileSizeMB, number} MB\n\n此链接将于 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果你未请求此导出，请立即更改密码并联系支持团队。\n\n– {product_name} 团队"
	},
	"inactivity_warning": {
		"subject": "你的 {product_name} 账号将因不活跃而被删除",
		"body": "你好 {username}，\n\n自 {lastActiveDate, date, full} 以来，我们未检测到你的 {product_name} 账号有任何活动。\n\n如果你未在 {deletionDate, date, full} {deletionDate, time, short} 前登录，你的账号将因长期未活动而被永久删除。\n\n立即登录：\n{loginUrl}\n\n如果你最近使用过 {product_name}，请立即联系我们的支持团队。\n\n– {product_name} 团队"
	},
	"ip_authorization": {
		"subject": "授权新 IP 地址登录",
		"body": "你好 {username}，\n\n我们检测到你的 {product_name} 账号有来自新 IP 地址的登录尝试：\n\nIP 地址：{ipAddress}\n位置：{location}\n\n如果你本人进行了此操作，请点击下方链接授权此 IP 地址：\n\n{authUrl}\n\n如果你没有进行此操作，请立即更改密码。\n\n此链接 30 分钟内有效。\n\n– {product_name} 团队"
	},
	"password_change_verification": {
		"subject": "确认你的 {product_name} 密码更改",
		"body": "你好 {username}，\n\n我们收到了更改你的 {product_name} 账号密码的请求。\n\n要确认此更改，请在应用中输入此验证码：\n\n{code}\n\n此验证码将于 {expiresAt} 失效。\n\n如果你未请求此操作，可能有人访问了你的账号。请立即更改密码并启用双重身份验证。\n\n– {product_name} 团队"
	},
	"password_reset": {
		"subject": "重置你的 {product_name} 账号密码",
		"body": "你好 {username}，\n\n你请求重置 {product_name} 账号密码。请使用下方链接设置新密码：\n\n{resetUrl}\n\n如果你未请求此操作，请忽略此邮件。\n\n此链接 1 小时内有效。\n\n– {product_name} 团队"
	},
	"registration_approved": {
		"subject": "你的 {product_name} 注册已通过",
		"body": "你好 {username}，\n\n好消息：你的 {product_name} 注册已通过。\n\n你现在可以登录 {product_name} 应用：\n{channelsUrl}\n\n欢迎加入 {product_name} 社区。\n\n– {product_name} Team"
	},
	"report_resolved": {
		"subject": "你的 {product_name} 报告已审核",
		"body": "你好 {username}，\n\n你的报告（ID: {reportId}）已通过我们的安全团队审核。{hasComment, select, yes {\n\n安全团队的回复：\n{publicComment}} other {}}\n\n感谢你为维护 {product_name} 社区安全所做的贡献。我们认真对待所有报告，并感谢你对社区的支持。\n\n如果你对此结果有任何疑问或担忧，请联系 {safety_email}。\n\n– {product_name} 安全团队"
	},
	"scheduled_deletion_notification": {
		"subject": "你的 {product_name} 账号将被永久删除",
		"body": "你好 {username}，\n\n你的 {product_name} 账号已被安排永久删除。\n\n计划删除时间：{deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {原因： {reason}}\n}\n\n这是一项严重的强制措施。你的账号数据将在预定日期被永久删除。\n\n如果你认为此强制决定不正确，可以提交申诉。请使用此邮箱地址发送邮件至 {appeals_email}。\n\n– {product_name} 安全团队"
	},
	"self_deletion_scheduled": {
		"subject": "你的 {product_name} 账号已计划删除",
		"body": "你好 {username}，\n\n你请求删除你的 {product_name} 账号。你的账号计划于以下时间永久删除：\n\n{deletionDate, date, full} {deletionDate, time, short}\n\n如果你未请求此操作，请登录账号取消删除。建议同时更改密码以保护账号安全。\n\n– {product_name} 团队"
	},
	"unban_notification": {
		"subject": "你的 {product_name} 账号已解除封禁",
		"body": "你好 {username}，\n\n好消息：你的 {product_name} 账号已解除封禁。\n\n{reason, select,\n  null {}\n  other {原因： {reason}}\n}\n\n你现在可以重新登录，正常使用 {product_name}。\n\n– {product_name} 安全团队"
	}
});

export default EMAIL_I18N_ZH_CN_MESSAGES;
