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

export const ko: EmailTranslations = {
	passwordReset: {
		subject: 'Fluxer 비밀번호 재설정',
		body: `안녕하세요, {username}님.

Fluxer 계정의 비밀번호 재설정 요청이 접수되었습니다. 아래 링크를 통해 새 비밀번호를 설정해주세요.

{resetUrl}

비밀번호 재설정을 요청하지 않으셨다면, 이 이메일은 무시하셔도 안전합니다.

이 링크는 1시간 후 만료됩니다.

- Fluxer 팀`,
	},
	emailVerification: {
		subject: 'Fluxer 이메일 주소를 확인해주세요',
		body: `안녕하세요, {username}님.

아래 링크를 클릭하여 Fluxer 계정에 등록된 이메일 주소를 확인해주세요.

{verifyUrl}

Fluxer 계정을 생성하지 않으셨다면, 이 이메일은 무시하셔도 됩니다.

이 링크는 24시간 후 만료됩니다.

- Fluxer 팀`,
	},
	ipAuthorization: {
		subject: '새 IP 주소에서의 로그인 승인',
		body: `안녕하세요, {username}님.

새로운 IP 주소에서 Fluxer 계정으로 로그인 시도가 감지되었습니다.

IP 주소: {ipAddress}
위치: {location}

본인이 맞다면 아래 링크를 클릭하여 이 IP 주소를 승인해주세요.

{authUrl}

로그인을 시도하지 않으셨다면 즉시 비밀번호를 변경하시는 것을 권장드립니다.

이 승인 링크는 30분 후 만료됩니다.

- Fluxer 팀`,
	},
	accountDisabledSuspicious: {
		subject: 'Fluxer 계정이 일시적으로 비활성화되었습니다',
		body: `안녕하세요, {username}님.

의심스러운 활동이 감지되어 Fluxer 계정이 일시적으로 비활성화되었습니다.

{reason, select,
	null {}
	other {사유: {reason}

}}계정에 다시 접근하시려면 비밀번호를 재설정해야 합니다.

{forgotUrl}

비밀번호 재설정이 완료되면 다시 로그인하실 수 있습니다.

이 조치가 실수라고 생각되면 고객 지원 팀에 문의해주세요.

- Fluxer 안전팀`,
	},
	accountTempBanned: {
		subject: 'Fluxer 계정이 일시적으로 정지되었습니다',
		body: `안녕하세요, {username}님.

서비스 이용약관 또는 커뮤니티 가이드라인 위반으로 인해 Fluxer 계정이 일시적으로 정지되었습니다.

정지 기간: {durationHours, plural,
	=1 {1시간}
	other {#시간}
}
정지 해제 예정: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {사유: {reason}}
}

정지 기간 동안에는 계정에 접근하실 수 없습니다.

아래 내용을 확인해주시기 바랍니다.
- 이용약관: {termsUrl}
- 커뮤니티 가이드라인: {guidelinesUrl}

해당 조치가 잘못되었거나 부당하다고 생각되면, 이 이메일 주소에서 appeals@fluxer.app 로 이의 제기 메일을 보내실 수 있습니다.  
왜 잘못된 결정이라고 생각하는지 자세히 설명해주시면, 검토 후 결과를 회신드리겠습니다.

- Fluxer 안전팀`,
	},
	accountScheduledDeletion: {
		subject: 'Fluxer 계정이 삭제 예정 상태입니다',
		body: `안녕하세요, {username}님.

서비스 이용약관 또는 커뮤니티 가이드라인 위반으로 인해 Fluxer 계정이 영구 삭제될 예정입니다.

예정된 삭제 일시: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {사유: {reason}}
}

이는 매우 중대한 조치이며, 예정된 시점에 계정 데이터는 완전히 삭제됩니다.

아래 문서를 다시 한 번 확인해주시기 바랍니다.
- 이용약관: {termsUrl}
- 커뮤니티 가이드라인: {guidelinesUrl}

[이의 제기 절차]  
이 결정이 잘못되었거나 부당하다고 생각되면, 이 이메일 주소에서 30일 이내에 appeals@fluxer.app 로 이의 제기 메일을 보내실 수 있습니다.

메일에는 다음 내용을 포함해 주세요.
- 결정이 잘못되었다고 생각하는 구체적인 이유
- 관련 증거나 추가 설명

Fluxer 안전팀이 이의를 검토하며, 최종 결정이 내려질 때까지 삭제가 보류될 수 있습니다.

- Fluxer 안전팀`,
	},
	selfDeletionScheduled: {
		subject: 'Fluxer 계정 삭제가 예약되었습니다',
		body: `안녕하세요, {username}님.

떠나시게 되어 아쉽습니다. Fluxer 계정 삭제가 예약되었습니다.

예약된 삭제 일시: {deletionDate, date, full} {deletionDate, time, short}

중요: {deletionDate, date, full} {deletionDate, time, short} 이전에 계정으로 다시 로그인하시면 언제든지 삭제를 취소하실 수 있습니다.

[탈퇴 전에 확인하세요]  
사용자 설정의 개인정보 보호 대시보드에서 다음 작업을 수행할 수 있습니다.
- 플랫폼 내 본인 메시지 삭제
- 떠나기 전 중요한 데이터 내보내기

주의: 계정이 삭제된 이후에는 메시지를 삭제할 수 없습니다. 메시지 삭제를 원하신다면, 계정 삭제가 완료되기 전에 반드시 진행해주세요.

생각이 바뀌었다면, 다시 로그인하시면 삭제 예약이 취소됩니다.

- Fluxer 팀`,
	},
	inactivityWarning: {
		subject: '장기간 미사용으로 Fluxer 계정이 삭제될 예정입니다',
		body: `안녕하세요, {username}님.

2년 이상 Fluxer 계정에 로그인하지 않으신 것으로 확인되었습니다.

마지막 로그인: {lastActiveDate, date, full} {lastActiveDate, time, short}

데이터 보존 정책에 따라, 장기간 사용되지 않은 계정은 자동으로 삭제가 예약됩니다. 회원님의 계정은 다음 시점에 영구 삭제됩니다.

삭제 예정 일시: {deletionDate, date, full} {deletionDate, time, short}

[계정을 유지하는 방법]  
삭제 예정일 이전에 {loginUrl} 에 로그인만 해주시면, 자동 삭제가 취소됩니다. 추가 조치는 필요하지 않습니다.

[로그인하지 않을 경우]  
- 계정과 모든 관련 데이터가 영구적으로 삭제됩니다.
- 메시지는 익명 처리되어 “Deleted User(삭제된 사용자)”로 표시됩니다.
- 이 작업은 되돌릴 수 없습니다.

[메시지를 먼저 삭제하고 싶다면]  
계정 삭제 전에 로그인하신 뒤, 사용자 설정의 개인정보 보호 대시보드를 이용해 메시지를 삭제하실 수 있습니다.

다시 Fluxer에서 만나 뵙길 바랍니다!

- Fluxer 팀`,
	},
	harvestCompleted: {
		subject: 'Fluxer 데이터 내보내기가 준비되었습니다',
		body: `안녕하세요, {username}님.

요청하신 데이터 내보내기가 완료되었으며, 이제 다운로드하실 수 있습니다!

내보내기 요약:
- 총 메시지 수: {totalMessages, number}
- 파일 크기: {fileSizeMB} MB
- 형식: JSON 파일이 포함된 ZIP 아카이브

데이터 다운로드: {downloadUrl}

중요: 이 다운로드 링크는 {expiresAt, date, full} {expiresAt, time, short} 에 만료됩니다.

내보내기에는 다음 내용이 포함됩니다.
- 채널별로 정리된 모든 메시지
- 채널 메타데이터
- 사용자 프로필 및 계정 정보
- 길드 멤버십 및 설정
- 인증 세션 및 보안 관련 정보

데이터는 분석이 용이하도록 JSON 형식으로 제공됩니다.

데이터 내보내기와 관련해 궁금한 점이 있다면 support@fluxer.app 로 문의해주세요.

- Fluxer 팀`,
	},
	unbanNotification: {
		subject: 'Fluxer 계정 정지가 해제되었습니다',
		body: `안녕하세요, {username}님.

좋은 소식입니다! Fluxer 계정에 대한 정지 조치가 해제되었습니다.

사유: {reason}

이제 다시 로그인하여 Fluxer를 이용하실 수 있습니다.

- Fluxer 안전팀`,
	},
	scheduledDeletionNotification: {
		subject: 'Fluxer 계정이 삭제될 예정입니다',
		body: `안녕하세요, {username}님.

Fluxer 계정이 영구 삭제될 예정입니다.

삭제 예정 일시: {deletionDate, date, full} {deletionDate, time, short}
사유: {reason}

이는 중대한 조치이며, 예정된 시점에 계정 데이터가 영구적으로 삭제됩니다.

결정이 부당하다고 생각되면, 이 이메일 주소에서 appeals@fluxer.app 로 이의 제기 메일을 보내실 수 있습니다.

- Fluxer 안전팀`,
	},
	giftChargebackNotification: {
		subject: 'Fluxer Premium 선물이 취소되었습니다',
		body: `안녕하세요, {username}님.

회원님이 사용하신 Fluxer Premium 선물이, 원 구매자의 결제 분쟁(차지백) 제기로 인해 취소되었습니다.

이에 따라 계정에서 Premium 혜택이 제거되었습니다. 이는 선물 결제가 취소된 데 따른 조치입니다.

궁금한 점이 있으시면 support@fluxer.app 로 문의해주세요.

- Fluxer 팀`,
	},
	reportResolved: {
		subject: 'Fluxer 신고가 검토되었습니다',
		body: `안녕하세요, {username}님.

회원님이 제출하신 신고(ID: {reportId})가 Fluxer 안전팀에 의해 검토되었습니다.

안전팀의 답변:
{publicComment}

Fluxer를 모두에게 안전한 공간으로 만드는 데 함께해주셔서 감사합니다.
모든 신고는 중요하게 다루고 있으며, 회원님의 기여에 감사드립니다.

이 결정에 대해 궁금한 점이나 우려 사항이 있다면 safety@fluxer.app 로 문의해주세요.

- Fluxer 안전팀`,
	},
	dsaReportVerification: {
		subject: 'DSA 신고를 위한 이메일 인증',
		body: `안녕하세요,

Fluxer에서 디지털 서비스법 신고를 제출하려면 다음 인증 코드를 사용하세요:

{code}

이 코드는 {expiresAt, date, full} {expiresAt, time, short}에 만료됩니다.

요청하지 않으셨다면 이 이메일을 무시하세요.

- Fluxer 안전팀`,
	},
	registrationApproved: {
		subject: 'Fluxer 가입이 승인되었습니다',
		body: `안녕하세요, {username}님.

좋은 소식입니다! Fluxer 가입이 승인되었습니다.

이제 아래 링크에서 Fluxer 앱에 로그인하실 수 있습니다.
{channelsUrl}

Fluxer 커뮤니티에 오신 것을 환영합니다!

- Fluxer 팀`,
	},
	emailChangeRevert: {
		subject: 'Fluxer 이메일이 변경되었습니다',
		body: `안녕하세요, {username} 님.

Fluxer 계정 이메일이 {newEmail}(으)로 변경되었습니다.

직접 변경하신 경우 추가 조치가 필요 없습니다. 아니라면 아래 링크로 되돌리고 계정을 보호하세요:

{revertUrl}

이렇게 하면 이전 이메일이 복원되고, 모든 세션에서 로그아웃되며, 연결된 전화번호가 제거되고, MFA가 비활성화되며, 새 비밀번호가 필요합니다.

- Fluxer 보안팀`,
	},
};
