/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmailTranslations} from '../types';

export const zhCN: EmailTranslations = {
	passwordReset: {
		subject: '重置你的 Fluxer 密码',
		body: `你好，{username}：

你请求重置 Fluxer 账户密码。请点击以下链接设置新密码：

{resetUrl}

如果这不是你本人操作，请忽略此邮件。

该链接将于 1 小时后失效。

- Fluxer 团队`,
	},
	emailVerification: {
		subject: '验证你的 Fluxer 邮箱地址',
		body: `你好，{username}：

请点击以下链接，验证你的 Fluxer 账户邮箱地址：

{verifyUrl}

若你未创建 Fluxer 账户，请忽略此邮件。

该链接将于 24 小时后失效。

- Fluxer 团队`,
	},
	ipAuthorization: {
		subject: '确认来自新 IP 地址的登录',
		body: `你好，{username}：

我们检测到你的 Fluxer 账户有来自新 IP 地址的登录尝试：

IP 地址：{ipAddress}
位置：{location}

如果这是你本人，请点击以下链接授权该 IP 地址：

{authUrl}

如果并非你本人，请立即修改密码。

该授权链接将于 30 分钟后失效。

- Fluxer 团队`,
	},
	accountDisabledSuspicious: {
		subject: '你的 Fluxer 账户因异常活动已被暂时停用',
		body: `你好，{username}：

由于检测到可疑活动，你的 Fluxer 账户已被暂时停用。

{reason, select,
	null {}
	other {原因：{reason}

}}要恢复账户访问，你必须先重置密码：

{forgotUrl}

重置密码后，你将能够再次登录。

如果你认为这是错误操作，请联系支持团队。

- Fluxer 安全团队`,
	},
	accountTempBanned: {
		subject: '你的 Fluxer 账户已被临时封禁',
		body: `你好，{username}：

你的 Fluxer 账户因违反服务条款或社区指南而被临时封禁。

封禁时长：{durationHours, plural,
	=1 {1 小时}
	other {# 小时}
}
封禁截止时间：{bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {原因：{reason}}
}

在封禁期间，你将无法访问你的账户。

请阅读以下内容：
- 服务条款：{termsUrl}
- 社区指南：{guidelinesUrl}

若你认为此封禁不正确或不合理，你可以使用该邮箱向 appeals@fluxer.app 提交申诉。  
请清楚说明你认为决定错误的理由。我们会审核你的申诉并回复最终结果。

- Fluxer 安全团队`,
	},
	accountScheduledDeletion: {
		subject: '你的 Fluxer 账户已被安排删除',
		body: `你好，{username}：

由于违反服务条款或社区指南，你的 Fluxer 账户已被安排进行永久删除。

计划删除时间：{deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {原因：{reason}}
}

这是严重的执行措施。你的账户数据将在指定日期永久删除。

建议阅读：
- 服务条款：{termsUrl}
- 社区指南：{guidelinesUrl}

申诉流程：
如果你认为该决定错误或不公平，你有 30 天时间向 appeals@fluxer.app 提交申诉。

请在申诉中包括：
- 你认为决定不正确的原因
- 任何相关证据或说明

Fluxer 安全团队成员会审核你的申诉，并可能在最终裁定前暂停删除操作。

- Fluxer 安全团队`,
	},
	selfDeletionScheduled: {
		subject: '你的 Fluxer 账户删除已被安排',
		body: `你好，{username}：

很遗憾看到你选择离开！你的 Fluxer 账户已被安排删除。

计划删除时间：{deletionDate, date, full} {deletionDate, time, short}

重要提示：你可以在 {deletionDate, date, full} {deletionDate, time, short} 之前随时通过登录账户取消此删除。

离开前请注意：
隐私控制面板允许你：
- 删除你在平台上的消息
- 导出重要数据

请注意：账户删除完成后，你将无法再删除消息。如需删除，请提前处理。

如果你改变主意，只需重新登录即可取消删除。

- Fluxer 团队`,
	},
	inactivityWarning: {
		subject: '你的 Fluxer 账户因长期未使用将被删除',
		body: `你好，{username}：

我们注意到你已有超过两年未登录你的 Fluxer 账户。

上次登录时间：{lastActiveDate, date, full} {lastActiveDate, time, short}

根据我们的数据保留政策，长期未使用的账户会被自动安排删除。

计划删除时间：{deletionDate, date, full} {deletionDate, time, short}

如何保留你的账户：
在删除日期前登录 {loginUrl} 即可取消此自动删除，无需其他操作。

若你不登录：
- 你的账户及所有数据将被永久删除
- 你的消息将被匿名化（显示为“已删除用户”）
- 此操作不可撤销

想提前删除你的消息？
你可以登录后在隐私控制面板中进行操作。

期待你回到 Fluxer！

- Fluxer 团队`,
	},
	harvestCompleted: {
		subject: '你的 Fluxer 数据导出已准备好',
		body: `你好，{username}：

你的数据导出已经完成，可以下载了！

导出内容摘要：
- 消息总数：{totalMessages, number}
- 文件大小：{fileSizeMB} MB
- 格式：包含 JSON 文件的 ZIP 压缩包

下载链接：{downloadUrl}

重要提示：该链接将于 {expiresAt, date, full} {expiresAt, time, short} 失效。

包含内容：
- 所有按频道组织的消息
- 频道元数据
- 你的用户资料和账户信息
- Guild 成员关系与设置
- 身份验证会话与安全信息

数据以 JSON 格式提供，便于分析。

如有疑问，请联系 support@fluxer.app

- Fluxer 团队`,
	},
	unbanNotification: {
		subject: '你的 Fluxer 账户封禁已解除',
		body: `你好，{username}：

好消息！你的 Fluxer 账户封禁已被解除。

原因：{reason}

你现在可以重新登录继续使用 Fluxer。

- Fluxer 安全团队`,
	},
	scheduledDeletionNotification: {
		subject: '你的 Fluxer 账户已被安排删除',
		body: `你好，{username}：

你的 Fluxer 账户已被安排永久删除。

删除时间：{deletionDate, date, full} {deletionDate, time, short}
原因：{reason}

这是严肃的措施。你的账户数据将被永久删除。

若你认为这是错误的决定，你可以发送申诉至 appeals@fluxer.app

- Fluxer 安全团队`,
	},
	giftChargebackNotification: {
		subject: '你的 Fluxer Premium 礼物已被撤销',
		body: `你好，{username}：

我们通知你，你所兑换的 Fluxer Premium 礼物因原购买者发起支付争议（chargeback）而被撤销。

你的 Premium 权益已被移除，因为付款已被撤回。

如有疑问，请联系 support@fluxer.app

- Fluxer 团队`,
	},
	reportResolved: {
		subject: '你的 Fluxer 举报已处理完毕',
		body: `你好，{username}：

你的举报（ID：{reportId}）已由安全团队处理。

安全团队回复：
{publicComment}

感谢你为 Fluxer 的社区安全作出的贡献。

如你对处理结果有疑问，请联系 safety@fluxer.app

- Fluxer 安全团队`,
	},
	dsaReportVerification: {
		subject: '验证你的邮箱以提交 DSA 举报',
		body: `你好：

请使用以下验证码在 Fluxer 上提交数字服务法案（Digital Services Act）举报：

{code}

此验证码将于 {expiresAt, date, full} {expiresAt, time, short} 失效。

如果这不是你本人操作，请忽略此邮件。

- Fluxer 安全团队`,
	},
	registrationApproved: {
		subject: '你的 Fluxer 注册已获批准',
		body: `你好，{username}：

好消息！你的 Fluxer 注册已获批准。

你现在可以通过以下链接进入 Fluxer：
{channelsUrl}

欢迎加入 Fluxer 社区！

- Fluxer 团队`,
	},
	emailChangeRevert: {
		subject: '你的 Fluxer 邮箱已被更改',
		body: `你好，{username}：

你的 Fluxer 帐户邮箱已更改为 {newEmail}。

如果是你本人操作，则无需处理。若非本人，请通过以下链接撤销并保护你的帐户：

{revertUrl}

这将恢复你之前的邮箱，登出所有会话，移除绑定的手机号，停用 MFA，并要求设置新密码。

- Fluxer 安全团队`,
	},
};
