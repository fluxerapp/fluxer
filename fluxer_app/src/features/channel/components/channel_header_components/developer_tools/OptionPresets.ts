// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	NONE_DESCRIPTOR,
	type RadioMenuOption,
	USE_ACTUAL_VALUE_DESCRIPTOR,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsShared';
import {
	formatDurationMs,
	NO_TIMER_DESCRIPTOR,
} from '@app/features/channel/components/channel_header_components/developer_tools/FormatHelpers';
import type {DeveloperOptionsState} from '@app/features/devtools/state/DeveloperOptions';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Subscription',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const LIFETIME_DESCRIPTOR = msg({
	message: 'Lifetime',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const USE_ACTUAL_PREMIUM_TYPE_DESCRIPTOR = msg({
	message: 'Use actual premium type',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const NONE_NORMAL_BEHAVIOR_DESCRIPTOR = msg({
	message: 'None (normal behavior)',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const UNVERIFIED_EMAIL_DESCRIPTOR = msg({
	message: 'Unverified email',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const UNCLAIMED_ACCOUNT_DESCRIPTOR = msg({
	message: 'Unclaimed account',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const ACCOUNT_TOO_NEW_DESCRIPTOR = msg({
	message: 'Account too new',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const NOT_A_MEMBER_LONG_ENOUGH_DESCRIPTOR = msg({
	message: 'Not a member long enough',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const ACCOUNT_LIMITED_DESCRIPTOR = msg({
	message: 'Messaging paused',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const SEND_MESSAGES_DISABLED_DESCRIPTOR = msg({
	message: 'Send messages disabled',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const GEO_RESTRICTED_DESCRIPTOR = msg({
	message: 'Geo restricted',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const MATURE_CONTENT_GATE_DESCRIPTOR = msg({
	message: 'Mature content',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const CONSENT_REQUIRED_DESCRIPTOR = msg({
	message: 'Consent required',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const MESSAGE_1_MONTH_DESCRIPTOR = msg({
	message: '1 month',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const MESSAGE_3_MONTHS_DESCRIPTOR = msg({
	message: '3 months',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const MESSAGE_6_MONTHS_DESCRIPTOR = msg({
	message: '6 months',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const MESSAGE_12_MONTHS_1_YEAR_DESCRIPTOR = msg({
	message: '12 months (1 year)',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const getPremiumTypeLabel = (i18n: I18n, premiumType: number | null): string => {
	switch (premiumType) {
		case UserPremiumTypes.NONE:
			return i18n._(NONE_DESCRIPTOR);
		case UserPremiumTypes.SUBSCRIPTION:
			return i18n._(SUBSCRIPTION_DESCRIPTOR);
		case UserPremiumTypes.LIFETIME:
			return i18n._(LIFETIME_DESCRIPTOR);
		default:
			return i18n._(USE_ACTUAL_VALUE_DESCRIPTOR);
	}
};
export const getPremiumTypeOptions = (): Array<RadioMenuOption<DeveloperOptionsState['premiumTypeOverride']>> => [
	{value: null, label: USE_ACTUAL_PREMIUM_TYPE_DESCRIPTOR},
	{value: UserPremiumTypes.NONE, label: NONE_DESCRIPTOR},
	{value: UserPremiumTypes.SUBSCRIPTION, label: SUBSCRIPTION_DESCRIPTOR},
	{value: UserPremiumTypes.LIFETIME, label: LIFETIME_DESCRIPTOR},
];
export const getVerificationBarrierOptions = (): Array<
	RadioMenuOption<DeveloperOptionsState['mockVerificationBarrier']>
> => [
	{value: 'none', label: NONE_NORMAL_BEHAVIOR_DESCRIPTOR},
	{value: 'unclaimed_account', label: UNCLAIMED_ACCOUNT_DESCRIPTOR},
	{value: 'unverified_email', label: UNVERIFIED_EMAIL_DESCRIPTOR},
	{value: 'account_too_new', label: ACCOUNT_TOO_NEW_DESCRIPTOR},
	{value: 'not_member_long', label: NOT_A_MEMBER_LONG_ENOUGH_DESCRIPTOR},
	{value: 'account_limited', label: ACCOUNT_LIMITED_DESCRIPTOR},
	{value: 'send_message_disabled', label: SEND_MESSAGES_DISABLED_DESCRIPTOR},
];
export const getCountdownTimerOptions = (
	i18n: I18n,
): Array<RadioMenuOption<DeveloperOptionsState['mockBarrierTimeRemaining']>> => [
	{value: 0, label: NO_TIMER_DESCRIPTOR},
	{value: 10000, label: formatDurationMs(i18n, 10000)},
	{value: 30000, label: formatDurationMs(i18n, 30000)},
	{value: 60000, label: formatDurationMs(i18n, 60000)},
	{value: 120000, label: formatDurationMs(i18n, 120000)},
	{value: 300000, label: formatDurationMs(i18n, 300000)},
	{value: 600000, label: formatDurationMs(i18n, 600000)},
];
export const getMatureContentChannelGateOptions = (): Array<
	RadioMenuOption<DeveloperOptionsState['mockMatureContentGateReason']>
> => [
	{value: 'none', label: NONE_NORMAL_BEHAVIOR_DESCRIPTOR},
	{value: 'geo_restricted', label: GEO_RESTRICTED_DESCRIPTOR},
	{value: 'mature_content_check_required', label: MATURE_CONTENT_GATE_DESCRIPTOR},
	{value: 'consent_required', label: CONSENT_REQUIRED_DESCRIPTOR},
];
export const getMatureContentMediaGateOptions = (): Array<
	RadioMenuOption<DeveloperOptionsState['mockMatureMediaGateReason']>
> => [
	{value: 'none', label: NONE_NORMAL_BEHAVIOR_DESCRIPTOR},
	{value: 'geo_restricted', label: GEO_RESTRICTED_DESCRIPTOR},
	{value: 'mature_content_check_required', label: MATURE_CONTENT_GATE_DESCRIPTOR},
];
export const getGiftDurationOptions = (): Array<RadioMenuOption<DeveloperOptionsState['mockGiftDurationMonths']>> => [
	{value: 1, label: MESSAGE_1_MONTH_DESCRIPTOR},
	{value: 3, label: MESSAGE_3_MONTHS_DESCRIPTOR},
	{value: 6, label: MESSAGE_6_MONTHS_DESCRIPTOR},
	{value: 12, label: MESSAGE_12_MONTHS_1_YEAR_DESCRIPTOR},
	{value: 0, label: LIFETIME_DESCRIPTOR},
];
