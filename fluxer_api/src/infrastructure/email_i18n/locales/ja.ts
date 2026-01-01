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

export const ja: EmailTranslations = {
	passwordReset: {
		subject: 'Fluxer（フラクサー）パスワードのリセット',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントのパスワード再設定がリクエストされました。以下のリンクから新しいパスワードを設定してください。

{resetUrl}

このパスワード再設定に心当たりがない場合は、このメールは無視していただいて問題ありません。

このリンクは1時間後に有効期限が切れます。

- Fluxer チーム`,
	},
	emailVerification: {
		subject: 'Fluxer（フラクサー）メールアドレスの確認',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントのメールアドレス確認のため、以下のリンクをクリックしてください。

{verifyUrl}

もし Fluxer アカウントを作成していない場合は、このメールを無視して問題ありません。

このリンクは24時間後に有効期限が切れます。

- Fluxer チーム`,
	},
	ipAuthorization: {
		subject: '新しい IP アドレスからのログインを承認してください（Fluxer）',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントに、新しい IP アドレスからのログイン試行が検出されました。

IP アドレス: {ipAddress}
場所: {location}

ご本人の場合は、以下のリンクをクリックしてこの IP アドレスを承認してください。

{authUrl}

心当たりがない場合は、ただちにパスワードを変更してください。

この承認リンクは30分後に有効期限が切れます。

- Fluxer チーム`,
	},
	accountDisabledSuspicious: {
		subject: 'Fluxer（フラクサー）アカウントが一時的に無効化されました',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントで不審な活動が検出されたため、一時的にアカウントを無効化しました。

{reason, select,
	null {}
	other {理由: {reason}

}}アカウントへ再びアクセスするには、パスワードをリセットしてください。

{forgotUrl}

パスワードをリセットすると、再度ログインできるようになります。

この処理に心当たりがない場合は、サポートチームまでご連絡ください。

- Fluxer セーフティチーム`,
	},
	accountTempBanned: {
		subject: 'Fluxer（フラクサー）アカウントが一時的に停止されました',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントが、サービス利用規約またはコミュニティガイドラインへの違反により一時停止されました。

停止期間: {durationHours, plural,
	=1 {1時間}
	other {#時間}
}
停止解除予定: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {理由: {reason}}
}

この期間中はアカウントへアクセスできません。

以下の内容をご確認ください:
- 利用規約: {termsUrl}
- コミュニティガイドライン: {guidelinesUrl}

もしこの措置が誤っている、または不当だと感じる場合は、appeals@fluxer.app までメールをお送りください。  
その際、判断が誤っていると思われる理由を明確に記載してください。

- Fluxer セーフティチーム`,
	},
	accountScheduledDeletion: {
		subject: 'Fluxer（フラクサー）アカウントが削除予定となっています',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントが、利用規約またはコミュニティガイドライン違反により永久削除の対象となりました。

削除予定日時: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {理由: {reason}}
}

これは重大な措置です。アカウントデータは削除予定日時に完全に削除されます。

以下をご確認ください:
- 利用規約: {termsUrl}
- コミュニティガイドライン: {guidelinesUrl}

【異議申し立て手続き】  
この決定が誤っている、または不当であると感じる場合は、30日以内に appeals@fluxer.app までメールをお送りください。

メール内容には以下を含めてください:
- なぜ決定が誤りまたは不当だと考えるのか
- 関連する証拠や背景情報

Fluxer セーフティチームが審査し、最終判断が下るまで削除が保留になる場合があります。

- Fluxer セーフティチーム`,
	},
	selfDeletionScheduled: {
		subject: 'Fluxer（フラクサー）アカウント削除がスケジュールされました',
		body: `こんにちは、{username} さん

ご利用ありがとうございました！Fluxer（フラクサー）アカウントの削除がスケジュールされました。

削除予定日時: {deletionDate, date, full} {deletionDate, time, short}

重要: 上記の日時より前に再ログインすれば、削除をいつでも取り消すことができます。

【退会前にできること】  
ユーザー設定内のプライバシーダッシュボードでは、以下が可能です:
- プラットフォーム上の自分のメッセージ削除  
- データのエクスポート

注意: アカウント削除後は、メッセージを削除することはできません。必要な場合は削除前に行ってください。

もし気が変わった場合は、再度ログインするだけで削除を取り消せます。

- Fluxer チーム`,
	},
	inactivityWarning: {
		subject: 'Fluxer（フラクサー）アカウントが長期間の未使用により削除されます',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントに2年以上ログインしていないことを確認しました。

最終ログイン日時: {lastActiveDate, date, full} {lastActiveDate, time, short}

データ保持ポリシーに基づき、長期間使用されていないアカウントは自動的に削除対象となります。

削除予定日時: {deletionDate, date, full} {deletionDate, time, short}

【アカウントを保持する方法】  
削除予定日時より前に {loginUrl} からログインすれば、自動削除をキャンセルできます。

【ログインしなかった場合】  
- アカウントとすべてのデータは永久に削除されます  
- メッセージは匿名化されます（「削除されたユーザー」として表示）  
- この操作は取り消せません  

【メッセージを削除したい場合】  
アカウント削除前にログインし、プライバシーダッシュボードをご利用ください。

Fluxerに戻ってきていただけることを願っています！

- Fluxer チーム`,
	},
	harvestCompleted: {
		subject: 'Fluxer（フラクサー）データエクスポートの準備ができました',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントのデータエクスポートが完了し、ダウンロード可能になりました！

エクスポート概要:
- 合計メッセージ数: {totalMessages, number}
- ファイルサイズ: {fileSizeMB} MB
- 形式: JSON ファイルを含む ZIP アーカイブ

データをダウンロード: {downloadUrl}

重要: このダウンロードリンクは {expiresAt, date, full} {expiresAt, time, short} に失効します。

エクスポートに含まれる内容:
- チャンネル別に整理されたすべてのメッセージ
- チャンネルメタデータ
- ユーザープロフィールとアカウント情報
- ギルドメンバーシップと設定
- 認証セッションとセキュリティ情報

データは JSON 形式で整理されているため、解析しやすくなっています。

ご不明点があれば support@fluxer.app までご連絡ください。

- Fluxer チーム`,
	},
	unbanNotification: {
		subject: 'Fluxer（フラクサー）アカウントの停止が解除されました',
		body: `こんにちは、{username} さん

朗報です！Fluxer（フラクサー）アカウントの停止措置が解除されました。

理由: {reason}

再ログインして、Fluxer の利用を再開できます。

- Fluxer セーフティチーム`,
	},
	scheduledDeletionNotification: {
		subject: 'Fluxer（フラクサー）アカウントが削除予定です',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントが永久削除の対象となりました。

削除予定日時: {deletionDate, date, full} {deletionDate, time, short}
理由: {reason}

これは重大な措置です。アカウントデータは削除予定日時に完全削除されます。

この措置に異議がある場合は、appeals@fluxer.app までメールをお送りください。

- Fluxer セーフティチーム`,
	},
	giftChargebackNotification: {
		subject: 'Fluxer（フラクサー）Premium ギフトが取り消されました',
		body: `こんにちは、{username} さん

ご利用の Fluxer（フラクサー）Premium ギフトが、購入者による支払い異議申し立て（チャージバック）により取り消されました。

これに伴い、Premium 特典はアカウントから削除されました。

ご不明点があれば support@fluxer.app までご連絡ください。

- Fluxer チーム`,
	},
	reportResolved: {
		subject: 'Fluxer（フラクサー）へのご報告内容が審査されました',
		body: `こんにちは、{username} さん

あなたの報告（ID: {reportId}）が Fluxer セーフティチームによって審査されました。

セーフティチームからの回答:
{publicComment}

Fluxer を安全な場所に保つためご協力いただき、ありがとうございます。
私たちはすべての報告を真剣に受け止めています。

ご不明点や懸念があれば safety@fluxer.app までご連絡ください。

- Fluxer セーフティチーム`,
	},
	dsaReportVerification: {
		subject: 'DSA 報告のためのメールアドレス確認',
		body: `こんにちは

Fluxer でデジタルサービス法に基づく報告を送信するため、以下の確認コードをご使用ください:

{code}

このコードは {expiresAt, date, full} {expiresAt, time, short} に有効期限が切れます。

このリクエストに心当たりがない場合は、このメールを無視してください。

- Fluxer セーフティチーム`,
	},
	registrationApproved: {
		subject: 'Fluxer（フラクサー）への登録が承認されました',
		body: `こんにちは、{username} さん

嬉しいお知らせです！Fluxer（フラクサー）への登録が承認されました。

以下から Fluxer アプリにログインできます:
{channelsUrl}

Fluxer コミュニティへようこそ！

- Fluxer チーム`,
	},
	emailChangeRevert: {
		subject: 'Fluxer（フラクサー）メールアドレスが変更されました',
		body: `こんにちは、{username} さん

Fluxer（フラクサー）アカウントのメールアドレスが {newEmail} に変更されました。

この変更に心当たりがある場合は、何もする必要はありません。もし身に覚えがない場合は、以下のリンクから元に戻してアカウントを保護してください。

{revertUrl}

これにより以前のメールアドレスが復元され、すべてのセッションからサインアウトされ、紐づく電話番号が削除され、MFAが無効になり、新しいパスワードの設定が必要になります。

- Fluxer セキュリティチーム`,
	},
};
