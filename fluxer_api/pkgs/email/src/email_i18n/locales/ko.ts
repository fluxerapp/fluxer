// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_KO_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "{product_name} 계정이 일시적으로 비활성화되었어요",
		"body": "안녕하세요, {username}님.\n\n의심스러운 활동이 감지되어 {product_name} 계정을 일시적으로 비활성화했어요.\n\n{reason, select,\n  null {}\n  other {사유: {reason}}\n}\n\n계정에 다시 액세스하려면 비밀번호를 재설정해야 해요:\n\n{forgotUrl}\n\n비밀번호를 재설정한 후 다시 로그인할 수 있어요.\n\n실수로 비활성화되었다고 생각되면 고객지원팀에 문의해 주세요.\n\n– {product_name} 안전팀"
	},
	"account_scheduled_deletion": {
		"subject": "{product_name} 계정이 영구적으로 삭제될 예정이에요",
		"body": "안녕하세요, {username}님.\n\n서비스 약관 또는 커뮤니티 가이드라인 위반으로 인해 {product_name} 계정이 영구적으로 삭제될 예정이에요.\n\n삭제 예정일: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {사유: {reason}}\n}\n\n이는 중대한 조치이며, 계정 데이터는 예정된 날짜에 영구적으로 삭제될 예정이에요.\n\n다음 내용을 확인해 주세요:\n- 서비스 약관: {termsUrl}\n- 커뮤니티 가이드라인: {guidelinesUrl}\n\n이의 제기 절차:\n이 조치가 잘못되었거나 부당하다고 생각되면 60일 이내에 이의를 제기할 수 있어요. 이 이메일 주소에서 {appeals_email}로 이메일을 보내 이의를 제기해 주세요.\n\n이의 제기 시에는:\n- 조치가 잘못되었거나 부당하다고 생각하는 이유를 명확하게 설명해 주세요.\n- 관련 증거 또는 맥락을 제공해 주세요.\n\n{product_name} 안전팀 멤버가 이의 제기를 검토하고 최종 결정이 내려질 때까지 삭제를 일시적으로 보류할 수 있어요.\n\n– {product_name} 안전팀"
	},
	"account_temp_banned": {
		"subject": "{product_name} 계정이 일시적으로 정지되었어요",
		"body": "안녕하세요, {username}님.\n\n서비스 약관 또는 커뮤니티 가이드라인 위반으로 인해 {product_name} 계정이 일시적으로 정지되었어요.\n\n기간: {durationHours, plural,\n  =1 {1시간}\n  other {#시간}\n}\n정지 해제일: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {사유: {reason}}\n}\n\n이 기간 동안에는 계정에 액세스할 수 없어요.\n\n다음 내용을 확인해 주세요:\n- 서비스 약관: {termsUrl}\n- 커뮤니티 가이드라인: {guidelinesUrl}\n\n이 조치가 잘못되었거나 부당하다고 생각되면 이의를 제기할 수 있어요. 이 이메일 주소에서 {appeals_email}로 이메일을 보내 결정이 잘못되었다고 생각하는 이유를 명확하게 설명해 주세요. 이의 제기를 검토한 후 결과를 알려 드릴 예정이에요.\n\n– {product_name} 안전팀"
	},
	"donation_confirmation": {
		"subject": "{product_name} 후원에 감사드립니다",
		"body": "안녕하세요,\n\n{product_name}에 후원해 주셔서 감사해요! 회원님의 {interval, select,\n  month {정기 후원}\n  year {정기 후원}\n  other {일회성 후원}\n}이 성공적으로 {interval, select,\n  month {설정되었어요}\n  year {설정되었어요}\n  other {처리되었어요}\n}.\n\n후원 상세 정보:\n금액: {amount} {currency} {interval, select,\n  month {매월}\n  year {매년}\n  other {}\n}\n\nStripe에서 곧 인보이스 PDF가 포함된 별도의 영수증을 이메일로 보내드릴 예정이에요. 이 영수증에는 모든 결제 세부 정보가 포함되어 있으며, 세금 목적으로 활용할 수 있어요.\n\n다음 링크를 통해 언제든지 후원 내역을 확인하고, 인보이스를 다운로드하고, {interval, select,\n  month {구독을 관리하거나 취소할 수 있어요}\n  year {구독을 관리하거나 취소할 수 있어요}\n  other {향후 후원을 관리할 수 있어요}\n}:\n\n{manageUrl}\n\n회원님의 후원은 {product_name}를 계속 운영하는 데 큰 도움이 돼요. 진심으로 감사해요!\n\n– {product_name} 팀"
	},
	"donation_magic_link": {
		"subject": "{product_name} 후원 내역 관리",
		"body": "안녕하세요,\n\n아래 링크를 클릭하여 후원자 포털에 접속해 주세요:\n\n{manageUrl}\n\n포털에서 구독을 관리하고, 인보이스를 다운로드하고, 후원 내역을 확인할 수 있어요.\n\n이 링크는 {expiresAt, date, full} {expiresAt, time, short}에 만료돼요.\n\n이 링크를 요청하지 않았다면 이 이메일을 무시해도 괜찮아요.\n\n– {product_name} 팀"
	},
	"dsa_report_verification": {
		"subject": "DSA 보고서 제출 이메일 인증",
		"body": "안녕하세요,\n\n아래 인증 코드를 사용하여 {product_name}에 대한 디지털 서비스법 보고서를 제출해 주세요:\n\n{code}\n\n이 코드는 {expiresAt, date, full} {expiresAt, time, short}에 만료돼요.\n\n요청하지 않으셨다면 이 이메일을 무시해도 괜찮아요.\n\n– {product_name} 안전팀"
	},
	"email_change_new": {
		"subject": "새 {product_name} 이메일 인증",
		"body": "안녕하세요, {username}님.\n\n새 {product_name} 이메일을 인증하려면 앱에 이 코드를 입력해 주세요:\n\n{code}\n\n이 코드는 {expiresAt, date, full} {expiresAt, time, short}에 만료돼요.\n\n요청하지 않으셨다면 이 이메일을 무시해도 괜찮아요.\n\n– {product_name} 팀"
	},
	"email_change_original": {
		"subject": "{product_name} 이메일 변경 확인",
		"body": "안녕하세요, {username}님.\n\n{product_name} 계정의 이메일 주소 변경 요청이 접수되었어요.\n\n이 변경을 확인하려면 앱에 이 코드를 입력해 주세요:\n\n{code}\n\n이 코드는 {expiresAt, date, full} {expiresAt, time, short}에 만료돼요.\n\n요청하지 않으셨다면 즉시 계정을 보호하세요.\n\n– {product_name} 팀"
	},
	"email_change_revert": {
		"subject": "{product_name} 이메일 변경 완료",
		"body": "안녕하세요, {username}님.\n\n{product_name} 계정의 이메일 주소가 {newEmail}로 변경되었어요.\n\n직접 변경하셨다면 별도의 조치는 필요 없어요. 직접 변경하지 않으셨다면 다음 링크를 사용하여 변경 사항을 되돌리고 계정을 보호할 수 있어요:\n\n{revertUrl}\n\n이전 이메일이 복원되고, 모든 기기에서 로그아웃되며, 연결된 전화번호가 제거되고, MFA가 비활성화되며, 새 비밀번호를 설정해야 해요.\n\n– {product_name} 안전팀"
	},
	"email_verification": {
		"subject": "{product_name} 이메일 주소 인증",
		"body": "안녕하세요, {username}님.\n\n아래 링크를 클릭하여 {product_name} 계정의 이메일 주소를 인증해 주세요:\n\n{verifyUrl}\n\n{product_name} 계정을 생성하지 않으셨다면 이 이메일을 무시해도 괜찮아요.\n\n이 링크는 24시간 동안 유효해요.\n\n– {product_name} 팀"
	},
	"gift_chargeback_notification": {
		"subject": "사용하신 선물 혜택이 제거되었어요",
		"body": "안녕하세요, {username}님.\n\n사용하신 선물 코드는 다른 사용자가 결제한 것이었어요. 해당 결제가 취소(차지백)되었어요.\n\n이로 인해 선물을 사용했을 때 계정에 추가되었던 혜택이 제거되었어요.\n\n실수라고 생각되면 고객지원팀에 문의하여 선물 코드 및 사용 시기에 대한 자세한 정보를 알려 주세요.\n\n– {product_name} 팀"
	},
	"harvest_completed": {
		"subject": "{product_name} 데이터 내보내기 다운로드가 준비되었어요",
		"body": "안녕하세요, {username}님.\n\n데이터 내보내기가 준비되었어요.\n\n다운로드 링크:\n{downloadUrl}\n\n포함된 메시지: {totalMessages, number}개\n파일 크기: {fileSizeMB, number} MB\n\n이 링크는 {expiresAt, date, full} {expiresAt, time, short}에 만료돼요.\n\n이 내보내기를 요청하지 않으셨다면 즉시 비밀번호를 변경하고 고객지원팀에 문의하세요.\n\n– {product_name} 팀"
	},
	"inactivity_warning": {
		"subject": "활동 부족으로 {product_name} 계정이 삭제될 예정이에요",
		"body": "안녕하세요, {username}님.\n\n{lastActiveDate, date, full} 이후로 {product_name} 계정에서 활동이 없었어요.\n\n{deletionDate, date, full} {deletionDate, time, short}까지 로그인하지 않으면 계정이 활동 부족으로 영구적으로 삭제될 예정이에요.\n\n여기에서 로그인해 주세요:\n{loginUrl}\n\n최근에 {product_name}를 사용했다면 즉시 고객지원팀에 문의하세요.\n\n– {product_name} 팀"
	},
	"ip_authorization": {
		"subject": "새 IP 주소에서 로그인을 승인해 주세요",
		"body": "안녕하세요, {username}님.\n\n새로운 IP 주소에서 {product_name} 계정으로 로그인 시도가 감지되었어요:\n\nIP 주소: {ipAddress}\n위치: {location}\n\n본인이 시도한 경우, 아래 링크를 클릭하여 이 IP 주소를 승인해 주세요:\n\n{authUrl}\n\n로그인을 시도하지 않으셨다면 즉시 비밀번호를 변경하세요.\n\n이 링크는 30분 동안 유효해요.\n\n– {product_name} 팀"
	},
	"password_change_verification": {
		"subject": "{product_name} 비밀번호 변경을 확인해 주세요",
		"body": "안녕하세요, {username}님.\n\n{product_name} 계정의 비밀번호 변경 요청이 접수되었어요.\n\n이 변경을 확인하려면 앱에 이 코드를 입력해 주세요:\n\n{code}\n\n이 코드는 {expiresAt}에 만료돼요.\n\n요청하지 않으셨다면 누군가 계정에 액세스했을 수 있어요. 즉시 비밀번호를 변경하고 2단계 인증을 활성화하세요.\n\n– {product_name} 팀"
	},
	"password_reset": {
		"subject": "{product_name} 비밀번호를 재설정해 주세요",
		"body": "안녕하세요, {username}님.\n\n{product_name} 비밀번호 재설정을 요청하셨어요. 아래 링크를 사용하여 새 비밀번호를 설정해 주세요:\n\n{resetUrl}\n\n요청하지 않으셨다면 이 이메일을 무시해도 괜찮아요.\n\n이 링크는 1시간 동안 유효해요.\n\n– {product_name} 팀"
	},
	"registration_approved": {
		"subject": "{product_name} 가입이 승인되었어요",
		"body": "안녕하세요, {username}님.\n\n좋은 소식이에요: {product_name} 가입이 승인되었어요.\n\n이제 여기에서 {product_name} 앱에 로그인할 수 있어요:\n{channelsUrl}\n\n{product_name} 커뮤니티에 오신 것을 환영해요.\n\n– {product_name} 팀"
	},
	"report_resolved": {
		"subject": "{product_name} 신고 검토가 완료되었어요",
		"body": "안녕하세요, {username}님.\n\n회원님의 신고(ID: {reportId})를 안전팀에서 검토했어요.{hasComment, select, yes {\n\n안전팀의 답변:\n{publicComment}} other {}}\n\n모두를 위해 {product_name}를 안전하게 유지하는 데 도움을 주셔서 감사해요. 모든 신고를 진지하게 검토하고 있으며, 커뮤니티에 기여해 주셔서 감사해요.\n\n이 결과에 대해 궁금한 점이나 우려 사항이 있다면 {safety_email}으로 문의해 주세요.\n\n– {product_name} 안전팀"
	},
	"scheduled_deletion_notification": {
		"subject": "{product_name} 계정이 영구적으로 삭제될 예정이에요",
		"body": "안녕하세요, {username}님.\n\n{product_name} 계정이 영구적으로 삭제될 예정이에요.\n\n삭제 예정일: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {사유: {reason}}\n}\n\n이는 중대한 조치이며, 계정 데이터는 예정된 날짜에 영구적으로 삭제될 예정이에요.\n\n이 조치가 잘못되었다고 생각되면 이의를 제기할 수 있어요. 이 이메일 주소에서 {appeals_email}로 이메일을 보내 이의를 제기해 주세요.\n\n– {product_name} 안전팀"
	},
	"self_deletion_scheduled": {
		"subject": "{product_name} 계정 삭제가 예정되었어요",
		"body": "안녕하세요, {username}님.\n\n{product_name} 계정 삭제를 요청하셨어요. 계정은 다음 날짜에 영구적으로 삭제될 예정이에요:\n\n{deletionDate, date, full} {deletionDate, time, short}\n\n요청하지 않으셨다면 계정에 로그인하여 삭제를 취소해 주세요. 계정을 보호하기 위해 비밀번호를 변경하는 것을 권장해요.\n\n– {product_name} 팀"
	},
	"unban_notification": {
		"subject": "{product_name} 계정 정지가 해제되었어요",
		"body": "안녕하세요, {username}님.\n\n좋은 소식이에요: {product_name} 계정 정지가 해제되었어요.\n\n{reason, select,\n  null {}\n  other {사유: {reason}}\n}\n\n이제 다시 로그인하여 평소처럼 {product_name}를 계속 사용할 수 있어요.\n\n– {product_name} 안전팀"
	}
});

export default EMAIL_I18N_KO_MESSAGES;
