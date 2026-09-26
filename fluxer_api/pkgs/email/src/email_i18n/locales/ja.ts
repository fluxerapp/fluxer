// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '@pkgs/email/src/email_i18n/EmailI18nMessages';

const EMAIL_I18N_JA_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "{product_name}アカウントが一時的に無効になりました",
		"body": "こんにちは、{username}さん\n\n不審なアクティビティが検出されたため、{product_name}アカウントを一時的に無効にしました。\n\n{reason, select,\n  null {}\n  other {理由: {reason}}\n}\n\nアカウントに再度アクセスするには、パスワードをリセットする必要があります。\n\n{forgotUrl}\n\nパスワードをリセットすると、再度ログインできるようになります。\n\nこの措置が誤りであると思われる場合は、サポートチームにお問い合わせください。\n\n– {product_name}安全チーム"
	},
	"account_scheduled_deletion": {
		"subject": "{product_name}アカウントは完全に削除されます",
		"body": "こんにちは、{username}さん\n\n{product_name}アカウントは、利用規約またはコミュニティガイドラインへの違反のため、完全な削除が予定されています。\n\n削除予定日時: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {理由: {reason}}\n}\n\nこれは重大な措置です。アカウントデータは、予定日に完全に削除されます。\n\n以下をご確認ください。\n- 利用規約: {termsUrl}\n- コミュニティガイドライン: {guidelinesUrl}\n\n異議申し立て手続き:\nこの措置が不正確または不当であると思われる場合は、60日以内に異議申し立てを提出できます。このメールアドレスから{appeals_email}までメールを送信してください。\n\n異議申し立てには、以下を記載してください。\n- 措置が不正確または不当であると考える理由の明確な説明\n- 関連する証拠や背景情報\n\n{product_name}安全チームのメンバーが異議申し立てを審査し、最終決定が下されるまで削除を一時停止する場合があります。\n\n– {product_name}安全チーム"
	},
	"account_temp_banned": {
		"subject": "{product_name}アカウントが一時的に停止されました",
		"body": "こんにちは、{username}さん\n\n{product_name}アカウントは、利用規約またはコミュニティガイドラインへの違反のため、一時的に停止されました。\n\n期間: {durationHours, plural,\n  =1 {1時間}\n  other {#時間}\n}\n停止解除日時: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {理由: {reason}}\n}\n\nこの期間中、アカウントにアクセスすることはできません。\n\n以下をご確認ください。\n- 利用規約: {termsUrl}\n- コミュニティガイドライン: {guidelinesUrl}\n\nこの措置が不正確または不当であると思われる場合は、異議申し立てを提出できます。このメールアドレスから{appeals_email}までメールを送信し、決定が不正確であると考える理由を明確に説明してください。異議申し立てを審査し、決定をご連絡いたします。\n\n– {product_name}安全チーム"
	},
	"donation_confirmation": {
		"subject": "{product_name}へのご寄付ありがとうございます",
		"body": "こんにちは。\n\n{product_name}へのご寄付ありがとうございます。{interval, select,\n  month {定期寄付}\n  year {定期寄付}\n  other {1回のみの寄付}\n}の{interval, select,\n  month {設定}\n  year {設定}\n  other {処理}\n}が完了しました。\n\n寄付の詳細:\n金額: {amount}{interval, select,\n  month {（毎月）}\n  year {（毎年）}\n  other {}\n}\n\nStripeから、請求書PDF付きの領収書が別途メールで送信されます。これにはすべての支払い詳細が含まれており、税務目的にご利用いただけます。\n\n以下のリンクから、いつでも寄付履歴の表示、請求書のダウンロード、{interval, select,\n  month {およびサブスクリプションの管理またはキャンセル}\n  year {およびサブスクリプションの管理またはキャンセル}\n  other {および今後の寄付の管理}\n}が可能です。\n\n{manageUrl}\n\n皆様のご支援が{product_name}の運営を支えています。ありがとうございます。\n\n– {product_name}チーム"
	},
	"donation_magic_link": {
		"subject": "{product_name}への寄付を管理",
		"body": "こんにちは。\n\n以下のリンクをクリックして、寄付者ポータルにアクセスしてください。\n\n{manageUrl}\n\nポータルでは、サブスクリプションの管理、請求書のダウンロード、寄付履歴の表示ができます。\n\nこのリンクの有効期限は{expiresAt, date, full} {expiresAt, time, short}です。\n\nこのリンクをリクエストしていない場合は、このメールを無視して問題ありません。\n\n– {product_name}チーム"
	},
	"dsa_report_verification": {
		"subject": "DSA報告のためのメールアドレスを確認",
		"body": "こんにちは。\n\n以下の確認コードを使用して、{product_name}でデジタルサービス法に基づく報告を提出してください。\n\n{code}\n\nこのコードの有効期限は{expiresAt, date, full} {expiresAt, time, short}です。\n\nご自身でリクエストしていない場合は、このメールを無視して問題ありません。\n\n– {product_name}安全チーム"
	},
	"email_change_new": {
		"subject": "{product_name}の新しいメールアドレスを確認",
		"body": "こんにちは、{username}さん\n\n{product_name}の新しいメールアドレスを確認するには、アプリでこのコードを入力してください。\n\n{code}\n\nこのコードの有効期限は{expiresAt, date, full} {expiresAt, time, short}です。\n\nご自身でリクエストしていない場合は、このメールを無視して問題ありません。\n\n– {product_name}チーム"
	},
	"email_change_original": {
		"subject": "{product_name}のメールアドレス変更を確認",
		"body": "こんにちは、{username}さん\n\n{product_name}アカウントのメールアドレス変更リクエストを受け付けました。\n\nこの変更を確認するには、アプリでこのコードを入力してください。\n\n{code}\n\nこのコードの有効期限は{expiresAt, date, full} {expiresAt, time, short}です。\n\nご自身でリクエストしていない場合は、すぐにアカウントを保護してください。\n\n– {product_name}チーム"
	},
	"email_change_revert": {
		"subject": "{product_name}のメールアドレスが変更されました",
		"body": "こんにちは、{username}さん\n\n{product_name}アカウントのメールアドレスが{newEmail}に変更されました。\n\nこの変更を行った場合は、何もする必要はありません。変更を行っていない場合は、このリンクを使用して変更を元に戻し、アカウントを保護できます。\n\n{revertUrl}\n\nこれにより、以前のメールアドレスが復元され、すべてのデバイスからログアウトされ、連携済みの電話番号が削除され、MFAが無効になり、新しいパスワードの設定が必要になります。\n\n– {product_name}安全チーム"
	},
	"email_verification": {
		"subject": "{product_name}のメールアドレスを確認",
		"body": "こんにちは、{username}さん\n\n以下のリンクをクリックして、{product_name}アカウントのメールアドレスを確認してください。\n\n{verifyUrl}\n\n{product_name}アカウントを作成していない場合は、このメールを無視して問題ありません。\n\nこのリンクは24時間有効です。\n\n– {product_name}チーム"
	},
	"gift_chargeback_notification": {
		"subject": "引き換えたギフトの特典が削除されました",
		"body": "こんにちは、{username}さん\n\n引き換えたギフトコードは、元は別の方が支払ったものです。その支払いはその後取り消されました（チャージバック）。\n\nそのため、ギフトを引き換えた際にアカウントに追加された特典を削除しました。\n\nこれが間違いであると思われる場合は、ギフトコードと引き換え時期に関する詳細を添えて、サポートチームにお問い合わせください。\n\n– {product_name}チーム"
	},
	"harvest_completed": {
		"subject": "{product_name}データのエクスポート準備が完了しました",
		"body": "こんにちは、{username}さん\n\nデータのエクスポート準備が完了しました。\n\nダウンロードリンク:\n{downloadUrl}\n\n含まれるメッセージ数: {totalMessages, number}件\nファイルサイズ: {fileSizeMB, number} MB\n\nこのリンクの有効期限は{expiresAt, date, full} {expiresAt, time, short}です。\n\nご自身でこのエクスポートをリクエストしていない場合は、すぐにパスワードを変更し、サポートチームにお問い合わせください。\n\n– {product_name}チーム"
	},
	"inactivity_warning": {
		"subject": "{product_name}アカウントは非アクティブのため削除されます",
		"body": "こんにちは、{username}さん\n\n{lastActiveDate, date, full}以降、{product_name}アカウントでのアクティビティが確認されていません。\n\n{deletionDate, date, full} {deletionDate, time, short}までにログインしない場合、アカウントは非アクティブのため完全に削除されます。\n\nこちらからログインしてください。\n{loginUrl}\n\n最近{product_name}を利用した場合は、すぐにサポートチームにお問い合わせください。\n\n– {product_name}チーム"
	},
	"ip_authorization": {
		"subject": "新しいIPアドレスからのログインを承認",
		"body": "こんにちは、{username}さん\n\n新しいIPアドレスから{product_name}アカウントへのログイン試行を検出しました。\n\nIPアドレス: {ipAddress}\n場所: {location}\n\nこれがご自身によるものである場合は、以下のリンクをクリックしてこのIPアドレスを承認してください。\n\n{authUrl}\n\nログインを試行していない場合は、すぐにパスワードを変更してください。\n\nこのリンクは30分間有効です。\n\n– {product_name}チーム"
	},
	"mfa_backup_codes_view": {
		"subject": "{product_name}のバックアップコードへのアクセスを確認",
		"body": "こんにちは、{username}さん\n\n{product_name}アカウントのバックアップコードを表示するリクエストを受け付けました。\n\nこのリクエストを確認するには、アプリでこのコードを入力してください。\n\n{code}\n\nこのコードの有効期限は{expiresAt, date, full} {expiresAt, time, short}です。\n\nご自身でリクエストしていない場合は、誰かがあなたのアカウントにアクセスしている可能性があります。すぐにパスワードを変更してください。\n\n– {product_name}チーム"
	},
	"password_change_verification": {
		"subject": "{product_name}のパスワード変更を確認",
		"body": "こんにちは、{username}さん\n\n{product_name}アカウントのパスワード変更リクエストを受け付けました。\n\nこの変更を確認するには、アプリでこのコードを入力してください。\n\n{code}\n\nこのコードの有効期限は{expiresAt}です。\n\nご自身でリクエストしていない場合は、誰かがあなたのアカウントにアクセスしている可能性があります。すぐにパスワードを変更し、2段階認証を有効にしてください。\n\n– {product_name}チーム"
	},
	"password_reset": {
		"subject": "{product_name}のパスワードをリセット",
		"body": "こんにちは、{username}さん\n\n{product_name}のパスワードリセットをリクエストされました。以下のリンクを使用して新しいパスワードを設定してください。\n\n{resetUrl}\n\nご自身でリクエストしていない場合は、このメールを無視して問題ありません。\n\nこのリンクは1時間有効です。\n\n– {product_name}チーム"
	},
	"registration_approved": {
		"subject": "{product_name}の登録が承認されました",
		"body": "こんにちは、{username}さん\n\n良いお知らせです。{product_name}の登録が承認されました。\n\nこちらから{product_name}アプリにログインできます。\n{channelsUrl}\n\n{product_name}コミュニティへようこそ。\n\n– {product_name}チーム"
	},
	"report_resolved": {
		"subject": "{product_name}への報告の審査が完了しました",
		"body": "こんにちは、{username}さん\n\nご報告いただいた内容（ID: {reportId}）を安全チームが審査しました。{hasComment, select, yes {\n\n安全チームからの回答:\n{publicComment}} other {}}\n\n{product_name}をすべての人にとって安全に保つためのご協力ありがとうございます。すべての報告を真剣に受け止め、コミュニティへのご貢献に感謝します。\n\nこの結果についてご質問やご不明な点がございましたら、{safety_email}までお問い合わせください。\n\n– {product_name}安全チーム"
	},
	"scheduled_deletion_notification": {
		"subject": "{product_name}アカウントは完全に削除されます",
		"body": "こんにちは、{username}さん\n\n{product_name}アカウントの完全な削除が予定されています。\n\n削除予定日時: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {理由: {reason}}\n}\n\nこれは重大な措置です。アカウントデータは、予定日に完全に削除されます。\n\nこの措置が不正確であると思われる場合は、異議申し立てを提出できます。このメールアドレスから{appeals_email}までメールを送信してください。\n\n– {product_name}安全チーム"
	},
	"self_deletion_scheduled": {
		"subject": "{product_name}アカウントの削除が予定されています",
		"body": "こんにちは、{username}さん\n\n{product_name}アカウントの削除をリクエストされました。アカウントは以下の日時に完全に削除される予定です。\n\n{deletionDate, date, full} {deletionDate, time, short}\n\nご自身でリクエストしていない場合は、アカウントにログインして削除をキャンセルしてください。また、アカウントを保護するためにパスワードの変更をお勧めします。\n\n– {product_name}チーム"
	},
	"unban_notification": {
		"subject": "{product_name}アカウントの停止が解除されました",
		"body": "こんにちは、{username}さん\n\n良いお知らせです。{product_name}アカウントの停止が解除されました。\n\n{reason, select,\n  null {}\n  other {理由: {reason}}\n}\n\nこれで再度ログインして、通常通り{product_name}を利用できます。\n\n– {product_name}安全チーム"
	}
});

export default EMAIL_I18N_JA_MESSAGES;
