// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_ZH_TW_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "您的 {product_name} 帳號已暫時停用",
		"body": "哈囉 {username}，\n\n我們偵測到您的 {product_name} 帳號有可疑活動，因此已暫時停用您的帳號。\n\n{reason, select,\n  null {}\n  other {原因：{reason}}\n}\n\n若要重新取得帳號存取權，您需要重設密碼：\n\n{forgotUrl}\n\n重設密碼後，您將可以再次登入。\n\n如果您認為這是錯誤操作，請聯絡我們的支援團隊。\n\n– {product_name} 安全團隊"
	},
	"account_scheduled_deletion": {
		"subject": "您的 {product_name} 帳號將被永久刪除",
		"body": "哈囉 {username}，\n\n由於您違反了我們的服務條款或社群準則，您的 {product_name} 帳號已排定永久刪除。\n\n排定刪除時間：{deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {原因：{reason}}\n}\n\n這是一項嚴重的強制措施。您的帳號資料將在排定日期永久刪除。\n\n請檢閱：\n- 服務條款：{termsUrl}\n- 社群準則：{guidelinesUrl}\n\n申訴流程：\n如果您認為此強制措施的決定不正確或不合理，您有 60 天的時間可以提交申訴。請從此電子郵件地址寄送電子郵件至 {appeals_email}。\n\n在您的申訴中：\n- 請清楚說明您認為此強制措施的決定不正確或不合理的原因\n- 提供任何相關證據或背景資訊\n\n{product_name} 安全團隊的成員將會審查您的申訴，並可能暫停待處理的刪除，直到做出最終決定。\n\n– {product_name} 安全團隊"
	},
	"account_temp_banned": {
		"subject": "您的 {product_name} 帳號已暫時停權",
		"body": "哈囉 {username}，\n\n由於您違反了我們的服務條款或社群準則，您的 {product_name} 帳號已暫時停權。\n\n持續時間：{durationHours, plural,\n  =1 {1 小時}\n  other {# 小時}\n}\n停權至：{bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {原因：{reason}}\n}\n\n在此期間，您將無法存取您的帳號。\n\n請檢閱：\n- 服務條款：{termsUrl}\n- 社群準則：{guidelinesUrl}\n\n如果您認為此強制措施的決定不正確或不合理，您可以提交申訴。請從此電子郵件地址寄送電子郵件至 {appeals_email}，並清楚說明您認為該決定不正確的原因。我們將審查您的申訴並回覆我們的決定。\n\n– {product_name} 安全團隊"
	},
	"donation_confirmation": {
		"subject": "感謝您對 {product_name} 的捐款",
		"body": "哈囉，\n\n感謝您對 {product_name} 的捐款！您的 {interval, select,\n  month {定期捐款}\n  year {定期捐款}\n  other {一次性捐款}\n} 已成功{interval, select,\n  month {設定}\n  year {設定}\n  other {處理}\n}。\n\n捐款詳情：\n金額：{amount} {currency} {interval, select,\n  month {每月}\n  year {每年}\n  other {}\n}\n\nStripe 將會很快寄送一份包含發票 PDF 的獨立收據給您。這份收據包含所有付款詳情，可用於報稅。\n\n您可以隨時使用此連結查看您的捐款歷史記錄、下載發票，{interval, select,\n  month {以及管理或取消您的訂閱}\n  year {以及管理或取消您的訂閱}\n  other {以及管理未來的捐款}\n}：\n\n{manageUrl}\n\n您的支持有助於 {product_name} 的運作。謝謝您！\n\n– {product_name} 團隊"
	},
	"donation_magic_link": {
		"subject": "管理您的 {product_name} 捐款",
		"body": "哈囉，\n\n點擊下方連結以存取您的捐款者入口網站：\n\n{manageUrl}\n\n在入口網站中，您可以管理訂閱、下載發票並查看您的捐款歷史記錄。\n\n此連結將於 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果您沒有要求此連結，您可以安全地忽略此電子郵件。\n\n– {product_name} 團隊"
	},
	"dsa_report_verification": {
		"subject": "驗證您的電子郵件以提交 DSA 報告",
		"body": "哈囉，\n\n請使用以下驗證碼在 {product_name} 上提交您的數位服務法報告：\n\n{code}\n\n此驗證碼將於 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果您沒有要求此驗證碼，您可以忽略此電子郵件。\n\n– {product_name} 安全團隊"
	},
	"email_change_new": {
		"subject": "驗證您的新 {product_name} 電子郵件",
		"body": "哈囉 {username}，\n\n請在應用程式中輸入此驗證碼以驗證您的新 {product_name} 電子郵件：\n\n{code}\n\n此驗證碼將於 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果您沒有要求此驗證碼，您可以忽略此電子郵件。\n\n– {product_name} 團隊"
	},
	"email_change_original": {
		"subject": "確認您的 {product_name} 電子郵件變更",
		"body": "哈囉 {username}，\n\n我們收到變更您 {product_name} 帳號電子郵件地址的請求。\n\n若要確認此變更，請在應用程式中輸入此驗證碼：\n\n{code}\n\n此驗證碼將於 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果您沒有要求此變更，請立即保護您的帳號安全。\n\n– {product_name} 團隊"
	},
	"email_change_revert": {
		"subject": "您的 {product_name} 電子郵件已變更",
		"body": "哈囉 {username}，\n\n您的 {product_name} 帳號電子郵件地址已變更為 {newEmail}。\n\n如果您進行了此變更，則無需採取任何行動。如果您沒有進行此變更，您可以使用此連結還原變更並保護您的帳號安全：\n\n{revertUrl}\n\n這將會還原您先前的電子郵件、將您從所有裝置登出、移除連結的電話號碼、停用多重驗證，並要求您設定新密碼。\n\n– {product_name} 安全團隊"
	},
	"email_verification": {
		"subject": "驗證您的 {product_name} 電子郵件地址",
		"body": "哈囉 {username}，\n\n請點擊下方連結以驗證您的 {product_name} 帳號電子郵件地址：\n\n{verifyUrl}\n\n如果您沒有建立 {product_name} 帳號，您可以安全地忽略此電子郵件。\n\n此連結在 24 小時內有效。\n\n– {product_name} 團隊"
	},
	"gift_chargeback_notification": {
		"subject": "您兌換禮物獲得的福利已被移除",
		"body": "哈囉 {username}，\n\n您兌換的禮物代碼最初是由其他人支付的。該筆付款已遭撤銷（退款）。\n\n因此，我們已移除您兌換禮物時獲得的福利。\n\n如果您認為這是錯誤，請聯絡我們的支援團隊，並提供您所知道的禮物代碼和兌換時間的任何詳細資訊。\n\n– {product_name} 團隊"
	},
	"harvest_completed": {
		"subject": "您的 {product_name} 資料匯出已可下載",
		"body": "哈囉 {username}，\n\n您的資料已可下載。\n\n下載連結：\n{downloadUrl}\n\n包含的訊息：{totalMessages, number} 則\n檔案大小：{fileSizeMB, number} MB\n\n此連結將於 {expiresAt, date, full} {expiresAt, time, short} 失效。\n\n如果您沒有要求此匯出，請立即變更您的密碼並聯絡我們的支援團隊。\n\n– {product_name} 團隊"
	},
	"inactivity_warning": {
		"subject": "您的 {product_name} 帳號將因不活躍而被刪除",
		"body": "哈囉 {username}，\n\n自從 {lastActiveDate, date, full} 以來，我們沒有在您的 {product_name} 帳號上看到任何活動。\n\n如果您未在 {deletionDate, date, full} {deletionDate, time, short} 前登入，您的帳號將因不活躍而被永久刪除。\n\n請在此登入：\n{loginUrl}\n\n如果您最近有使用 {product_name}，請立即聯絡我們的支援團隊。\n\n– {product_name} 團隊"
	},
	"ip_authorization": {
		"subject": "授權從新的 IP 位址登入",
		"body": "哈囉 {username}，\n\n我們偵測到您的 {product_name} 帳號有來自新 IP 位址的登入嘗試：\n\nIP 位址：{ipAddress}\n位置：{location}\n\n如果這是您本人操作，請點擊下方連結授權此 IP 位址：\n\n{authUrl}\n\n如果您沒有嘗試登入，請立即變更您的密碼。\n\n此連結在 30 分鐘內有效。\n\n– {product_name} 團隊"
	},
	"password_change_verification": {
		"subject": "確認您的 {product_name} 密碼變更",
		"body": "哈囉 {username}，\n\n我們收到變更您 {product_name} 帳號密碼的請求。\n\n若要確認此變更，請在應用程式中輸入此驗證碼：\n\n{code}\n\n此驗證碼將於 {expiresAt} 失效。\n\n如果您沒有要求此驗證碼，可能有人存取了您的帳號。請立即變更您的密碼並啟用雙重驗證。\n\n– {product_name} 團隊"
	},
	"password_reset": {
		"subject": "重設您的 {product_name} 密碼",
		"body": "哈囉 {username}，\n\n您要求重設 {product_name} 密碼。請使用下方連結設定新密碼：\n\n{resetUrl}\n\n如果您沒有要求此重設，您可以安全地忽略此電子郵件。\n\n此連結在 1 小時內有效。\n\n– {product_name} 團隊"
	},
	"registration_approved": {
		"subject": "您的 {product_name} 註冊已獲批准",
		"body": "哈囉 {username}，\n\n好消息：您的 {product_name} 註冊已獲批准。\n\n您現在可以從這裡登入 {product_name} 應用程式：\n{channelsUrl}\n\n歡迎加入 {product_name} 社群。\n\n– {product_name} 團隊"
	},
	"report_resolved": {
		"subject": "您的 {product_name} 報告已審核",
		"body": "哈囉 {username}，\n\n您的報告（ID：{reportId}）已由我們的安全團隊審閱。{hasComment, select, yes {\n\n安全團隊的回覆：\n{publicComment}} other {}}\n\n感謝您協助維護 {product_name} 的安全。我們認真對待所有報告，並感謝您對社群的貢獻。\n\n如果您對此結果有任何疑問或疑慮，請聯絡 {safety_email}。\n\n– {product_name} 安全團隊"
	},
	"scheduled_deletion_notification": {
		"subject": "您的 {product_name} 帳號將被永久刪除",
		"body": "哈囉 {username}，\n\n您的 {product_name} 帳號已排定永久刪除。\n\n排定刪除時間：{deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {原因：{reason}}\n}\n\n這是一項嚴重的強制措施。您的帳號資料將在排定日期永久刪除。\n\n如果您認為此強制措施的決定不正確，您可以提交申訴。請從此電子郵件地址寄送電子郵件至 {appeals_email}。\n\n– {product_name} 安全團隊"
	},
	"self_deletion_scheduled": {
		"subject": "您的 {product_name} 帳號刪除已排定",
		"body": "哈囉 {username}，\n\n您要求刪除您的 {product_name} 帳號。您的帳號已排定於以下時間永久刪除：\n\n{deletionDate, date, full} {deletionDate, time, short}\n\n如果您沒有要求此刪除，請登入您的帳號以取消刪除。我們也建議您變更密碼以保護您的帳號安全。\n\n– {product_name} 團隊"
	},
	"unban_notification": {
		"subject": "您的 {product_name} 帳號停權已解除",
		"body": "哈囉 {username}，\n\n好消息：您的 {product_name} 帳號停權已解除。\n\n{reason, select,\n  null {}\n  other {原因：{reason}}\n}\n\n您現在可以重新登入並繼續正常使用 {product_name}。\n\n– {product_name} 安全團隊"
	}
});

export default EMAIL_I18N_ZH_TW_MESSAGES;
