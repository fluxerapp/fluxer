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

export const zhTW: EmailTranslations = {
	passwordReset: {
		subject: '重設你的 Fluxer 密碼',
		body: `你好，{username}：

你已提出重設 Fluxer 帳號密碼的請求。請點擊以下連結設定新密碼：

{resetUrl}

如果這不是你本人的操作，請忽略此郵件。

此連結將於 1 小時後失效。

— Fluxer 團隊`,
	},
	emailVerification: {
		subject: '驗證你的 Fluxer 電子郵件地址',
		body: `你好，{username}：

請點擊以下連結，以驗證你在 Fluxer 帳號所使用的電子郵件地址：

{verifyUrl}

若你未曾註冊 Fluxer 帳號，請忽略此郵件。

此連結將於 24 小時後失效。

— Fluxer 團隊`,
	},
	ipAuthorization: {
		subject: '確認從新 IP 位址的登入請求',
		body: `你好，{username}：

我們偵測到你的 Fluxer 帳號有來自新的 IP 位址的登入嘗試：

IP 位址：{ipAddress}
位置：{location}

如果這是你本人，請點擊以下連結授權此 IP 位址：

{authUrl}

如果這不是你，請立即變更密碼。

此授權連結將於 30 分鐘後失效。

— Fluxer 團隊`,
	},
	accountDisabledSuspicious: {
		subject: '你的 Fluxer 帳號因可疑活動已被暫時停用',
		body: `你好，{username}：

由於偵測到可疑活動，你的 Fluxer 帳號已被暫時停用。

{reason, select,
	null {}
	other {原因：{reason}

}}要重新取得帳號存取權，你必須重設密碼：

{forgotUrl}

完成密碼重設後，你將能重新登入。

如果你認為這是錯誤的處置，請聯繫我們的支援團隊。

— Fluxer 安全團隊`,
	},
	accountTempBanned: {
		subject: '你的 Fluxer 帳號已被暫時停權',
		body: `你好，{username}：

你的 Fluxer 帳號因違反服務條款或社群指南而遭到暫時停權。

停權時長：{durationHours, plural,
	=1 {1 小時}
	other {# 小時}
}
停權結束時間：{bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {原因：{reason}}
}

在停權期間，你將無法存取帳號。

請務必閱讀：
- 服務條款：{termsUrl}
- 社群指南：{guidelinesUrl}

若你認為此處置不正確或不公平，可以使用此電子郵件地址向 appeals@fluxer.app 提出申訴。  
請清楚說明你認為決定錯誤的原因。我們會審查你的申訴並回覆結果。

— Fluxer 安全團隊`,
	},
	accountScheduledDeletion: {
		subject: '你的 Fluxer 帳號已排程刪除',
		body: `你好，{username}：

由於違反服務條款或社群指南，你的 Fluxer 帳號已被排程永久刪除。

排程刪除時間：{deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {原因：{reason}}
}

這是嚴重的措施。所有帳號資料將在指定日期永久刪除。

請參考以下內容：
- 服務條款：{termsUrl}
- 社群指南：{guidelinesUrl}

申訴流程：
若你認為此決定有誤或不公平，你可在 30 天內向 appeals@fluxer.app 提出申訴。

你的申訴應包含：
- 為何你認為決定錯誤或不公
- 任何可佐證的相關資訊

Fluxer 安全團隊將審核申訴並可能暫停刪除作業，直至做出最終裁決。

— Fluxer 安全團隊`,
	},
	selfDeletionScheduled: {
		subject: '你的 Fluxer 帳號刪除已預定',
		body: `你好，{username}：

很遺憾看到你選擇離開！你的 Fluxer 帳號刪除作業已排程完成。

預定刪除時間：{deletionDate, date, full} {deletionDate, time, short}

重要提示：在 {deletionDate, date, full} {deletionDate, time, short} 之前，你可以隨時重新登入以取消刪除。

離開前請注意：
隱私控制面板可讓你：
- 刪除你在平台上的訊息
- 匯出資料以備份保存

注意：帳號刪除完成後，你將無法刪除訊息。若需刪除請提前處理。

若改變心意，只要重新登入即可取消刪除。

— Fluxer 團隊`,
	},
	inactivityWarning: {
		subject: '你的 Fluxer 帳號因長期未使用將被刪除',
		body: `你好，{username}：

我們注意到你已有超過兩年未登入 Fluxer 帳號。

上次登入時間：{lastActiveDate, date, full} {lastActiveDate, time, short}

依據我們的資料保存政策，長期未使用的帳號會自動排程刪除。

預定刪除時間：{deletionDate, date, full} {deletionDate, time, short}

如何保留你的帳號：
只需在刪除日期前於 {loginUrl} 登入即可取消刪除。

如果你未登入：
- 帳號及所有資料將被永久刪除
- 你的訊息將被匿名化（顯示為「已刪除使用者」）
- 此操作無法復原

想先刪除你的訊息嗎？
登入後可於隱私控制面板操作。

期待你再次回到 Fluxer！

— Fluxer 團隊`,
	},
	harvestCompleted: {
		subject: '你的 Fluxer 資料匯出已準備完成',
		body: `你好，{username}：

你的資料匯出已完成，可立即下載！

匯出摘要：
- 訊息總數：{totalMessages, number}
- 檔案大小：{fileSizeMB} MB
- 格式：包含 JSON 檔案的 ZIP 壓縮包

下載你的資料：{downloadUrl}

重要：此下載連結將於 {expiresAt, date, full} {expiresAt, time, short} 到期。

匯出內容包括：
- 所有訊息，依頻道分類
- 頻道後設資料
- 你的使用者資料與帳號資訊
- Guild 加入與設定
- 驗證工作階段與安全資訊

資料以 JSON 格式提供，方便後續分析。

若你有任何疑問，請聯繫 support@fluxer.app

— Fluxer 團隊`,
	},
	unbanNotification: {
		subject: '你的 Fluxer 帳號停權已解除',
		body: `你好，{username}：

好消息！你的 Fluxer 帳號停權已被解除。

原因：{reason}

你現在可以重新登入並繼續使用 Fluxer。

— Fluxer 安全團隊`,
	},
	scheduledDeletionNotification: {
		subject: '你的 Fluxer 帳號已排程刪除',
		body: `你好，{username}：

你的 Fluxer 帳號已排程進行永久刪除。

刪除日期：{deletionDate, date, full} {deletionDate, time, short}
原因：{reason}

這是嚴重的操作，你的帳號資料將永久刪除。

若你認為此決定有誤，可寄信至 appeals@fluxer.app 提出申訴。

— Fluxer 安全團隊`,
	},
	giftChargebackNotification: {
		subject: '你的 Fluxer Premium 禮物已被撤銷',
		body: `你好，{username}：

我們通知你，你兌換的 Fluxer Premium 禮物因原購買者提出付款爭議（chargeback）而被撤銷。

你的 Premium 權益已從帳號中移除，因付款已被退回。

若有疑問，請聯繫 support@fluxer.app

— Fluxer 團隊`,
	},
	reportResolved: {
		subject: '你的 Fluxer 檢舉已處理完成',
		body: `你好，{username}：

你的檢舉（ID：{reportId}）已由 Fluxer 安全團隊審查完成。

安全團隊回覆：
{publicComment}

感謝你協助維護 Fluxer 社群的安全。

若你對此結果有疑慮，請聯繫 safety@fluxer.app

— Fluxer 安全團隊`,
	},
	dsaReportVerification: {
		subject: '驗證你的電子郵件以提交 DSA 檢舉',
		body: `你好：

請使用以下驗證碼提交你在 Fluxer 的數位服務法檢舉：

{code}

此驗證碼將於 {expiresAt, date, full} {expiresAt, time, short} 失效。

若非你本人提出此請求，請忽略此郵件。

— Fluxer 安全團隊`,
	},
	registrationApproved: {
		subject: '你的 Fluxer 註冊已獲批准',
		body: `你好，{username}：

好消息！你的 Fluxer 註冊已獲批准。

你現在可以透過以下連結登入 Fluxer：
{channelsUrl}

歡迎加入 Fluxer 社群！

— Fluxer 團隊`,
	},
	emailChangeRevert: {
		subject: '你的 Fluxer 電子郵件已被更改',
		body: `你好，{username}：

你的 Fluxer 帳戶電子郵件已變更為 {newEmail}。

若此變更為你本人操作，則無需處理。若非你本人，請透過以下連結撤銷並保護你的帳戶：

{revertUrl}

這將恢復你先前的電子郵件、登出所有工作階段、移除綁定的電話號碼、停用 MFA，並要求設定新密碼。

- Fluxer 安全團隊`,
	},
};
