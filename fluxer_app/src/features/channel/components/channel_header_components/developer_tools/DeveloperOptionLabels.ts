// SPDX-License-Identifier: AGPL-3.0-or-later

import {ATTACH_FILES_PERMISSION, SEND_MESSAGES_PERMISSION} from '@app/features/app/config/I18nDisplayConstants';
import {
	ACTIVE_DESCRIPTOR,
	type ActiveOverrideEntry,
	CUSTOM_VALUE_DESCRIPTOR,
	ENABLED_DESCRIPTOR,
	FORCED_OFF_DESCRIPTOR,
	humanizeDeveloperStateKey,
	nonEmptyText,
	translateDescriptor,
	USE_ACTUAL_VALUE_DESCRIPTOR,
	USE_DEFAULT_DESCRIPTOR,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsShared';
import type {DeveloperOptionsState} from '@app/features/devtools/state/DeveloperOptions';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {formatDurationMs} from './FormatHelpers';
import {getPremiumTypeLabel, LIFETIME_DESCRIPTOR} from './OptionPresets';
import {DEVELOPER_OPTION_KEYS, isDeveloperOptionAtDefault, resetDeveloperOption} from './ResetOptions';

const ACTIVE_DEVELOPER_OPTION_KEYS = DEVELOPER_OPTION_KEYS.filter((key) => key !== 'premiumScenarioOverride');
export const ATTACHMENT_MOCKS_DESCRIPTOR = msg({
	message: 'Attachment mocks',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const ATTACHMENTS_PLURAL_DESCRIPTOR = msg({
	message: '{count, plural, one {# attachment} other {# attachments}}',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GIFTS_PLURAL_DESCRIPTOR = msg({
	message: '{count, plural, one {# gift} other {# gifts}}',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MONTHS_PLURAL_DESCRIPTOR = msg({
	message: '{months, plural, one {# month} other {# months}}',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const BYPASS_LOADING_SKELETON_DESCRIPTOR = msg({
	message: 'Bypass loading skeleton',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FORCE_LOADING_SKELETON_DESCRIPTOR = msg({
	message: 'Force loading skeleton',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FAIL_MESSAGE_SENDS_DESCRIPTOR = msg({
	message: 'Fail message sends',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FAIL_MESSAGE_LOADS_DESCRIPTOR = msg({
	message: 'Fail message loads',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const RENDER_PLACEHOLDERS_DESCRIPTOR = msg({
	message: 'Render placeholders',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const EMBED_SKELETONS_DESCRIPTOR = msg({
	message: 'Embed skeletons',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MEDIA_LOADING_STATE_DESCRIPTOR = msg({
	message: 'Media loading state',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const ANY_UPDATER_READY_DESCRIPTOR = msg({
	message: 'Any updater ready',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const NATIVE_UPDATER_READY_DESCRIPTOR = msg({
	message: 'Native updater ready',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const NATIVE_UPDATE_PROGRESS_DESCRIPTOR = msg({
	message: 'Native update progress',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const WEB_UPDATER_READY_DESCRIPTOR = msg({
	message: 'Web updater ready',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const UPDATER_STATE_DESCRIPTOR = msg({
	message: 'Updater state',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SHOW_MYSELF_TYPING_DESCRIPTOR = msg({
	message: 'Show myself typing',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOW_ATTACHMENT_UPLOAD_DESCRIPTOR = msg({
	message: 'Slow attachment upload',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOW_MESSAGE_LOAD_DESCRIPTOR = msg({
	message: 'Slow message load',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOW_MESSAGE_SEND_DESCRIPTOR = msg({
	message: 'Slow message send',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOW_MESSAGE_EDIT_DESCRIPTOR = msg({
	message: 'Slow message edit',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOW_PROFILE_LOAD_DESCRIPTOR = msg({
	message: 'Slow profile load',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const PROFILE_SKELETONS_DESCRIPTOR = msg({
	message: 'Profile skeletons',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const PROFILE_DATA_WARNING_DESCRIPTOR = msg({
	message: 'Profile data warning',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const CLOUD_UPLOAD_DESCRIPTOR = msg({
	message: 'Cloud upload',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GIF_PICKER_LOADING_DESCRIPTOR = msg({
	message: 'GIF picker loading',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const UNKNOWN_MESSAGE_TYPE_DESCRIPTOR = msg({
	message: 'Unknown message type',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SELF_HOSTED_MODE_DESCRIPTOR = msg({
	message: 'Self-hosted mode',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const VANITY_URL_DISCLAIMER_DESCRIPTOR = msg({
	message: 'Vanity URL disclaimer',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const VOICE_CONNECTION_DEBUG_DESCRIPTOR = msg({
	message: 'Voice connection debug',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const PREMIUM_TYPE_DESCRIPTOR = msg({
	message: 'Premium type',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const VISIONARY_BADGE_ID_DESCRIPTOR = msg({
	message: 'Visionary badge ID',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const PREMIUM_SINCE_DATE_DESCRIPTOR = msg({
	message: 'Premium since date',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const PREMIUM_UNTIL_DATE_DESCRIPTOR = msg({
	message: 'Premium until date',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const BILLING_CYCLE_DESCRIPTOR = msg({
	message: 'Billing cycle',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const WILL_CANCEL_DESCRIPTOR = msg({
	message: 'Will cancel',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const HAS_EVER_PURCHASED_DESCRIPTOR = msg({
	message: 'Has ever purchased',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const UNREAD_GIFT_INVENTORY_DESCRIPTOR = msg({
	message: 'Unread gift inventory',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const UNREAD_GIFT_COUNT_DESCRIPTOR = msg({
	message: 'Unread gift count',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const EMAIL_VERIFIED_DESCRIPTOR = msg({
	message: 'Email verified',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const UNCLAIMED_ACCOUNT_LABEL_DESCRIPTOR = msg({
	message: 'Unclaimed account',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const VERIFICATION_BARRIER_DESCRIPTOR = msg({
	message: 'Verification barrier',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const BARRIER_COUNTDOWN_DESCRIPTOR = msg({
	message: 'Barrier countdown',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MATURE_CHANNEL_GATE_DESCRIPTOR = msg({
	message: 'Mature content channel gate',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MATURE_MEDIA_GATE_DESCRIPTOR = msg({
	message: 'Mature content media gate',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FORCE_MATURE_MEDIA_DESCRIPTOR = msg({
	message: 'Force mature media',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const UK_GEO_DESCRIPTOR = msg({
	message: 'UK geo',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GEO_BLOCK_OVERLAY_DESCRIPTOR = msg({
	message: 'Geo block overlay',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REQUIRED_ACTIONS_OVERLAY_DESCRIPTOR = msg({
	message: 'Required actions overlay',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REQUIRED_ACTION_VARIANT_DESCRIPTOR = msg({
	message: 'Required action variant',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REQUIRED_ACTION_DEFAULT_TAB_DESCRIPTOR = msg({
	message: 'Required action default tab',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REQUIRED_ACTION_PHONE_STEP_DESCRIPTOR = msg({
	message: 'Required action phone step',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REQUIRED_ACTION_RESEND_LOADING_DESCRIPTOR = msg({
	message: 'Required action resend loading',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REQUIRED_ACTION_RESEND_OUTCOME_DESCRIPTOR = msg({
	message: 'Required action resend outcome',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REQUIRED_ACTION_REVERIFICATION_TEXT_DESCRIPTOR = msg({
	message: 'Required action reverification text',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const NO_SEND_PERMISSION_DESCRIPTOR = msg({
	message: 'No {sendMessagesPermission} permission',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const NO_ATTACH_PERMISSION_DESCRIPTOR = msg({
	message: 'No {attachFilesPermission} permission',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOWMODE_ACTIVE_DESCRIPTOR = msg({
	message: 'Slowmode active',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOWMODE_REMAINING_DESCRIPTOR = msg({
	message: 'Slowmode remaining',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GIFT_INVENTORY_DESCRIPTOR = msg({
	message: 'Gift inventory',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GIFT_DURATION_DESCRIPTOR = msg({
	message: 'Gift duration',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GIFT_REDEEMED_DESCRIPTOR = msg({
	message: 'Gift redeemed',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const TITLEBAR_PLATFORM_DESCRIPTOR = msg({
	message: 'Titlebar platform',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const DEVELOPER_OPTION_DESCRIPTOR = msg({
	message: 'Developer option',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GAME_CAPTURE_INJECTION_METHOD_DESCRIPTOR = msg({
	message: 'Game capture injection (Windows)',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const DEVELOPER_OPTION_LABEL_FALLBACKS: Partial<Record<keyof DeveloperOptionsState, MessageDescriptor>> = {
	mockAttachmentStates: ATTACHMENT_MOCKS_DESCRIPTOR,
};
export const getDeveloperOptionFallbackLabel = (i18n: I18n, key: keyof DeveloperOptionsState): string => {
	const descriptor = DEVELOPER_OPTION_LABEL_FALLBACKS[key];
	return descriptor ? i18n._(descriptor) : humanizeDeveloperStateKey(String(key));
};
const getDeveloperOptionFallbackValue = <K extends keyof DeveloperOptionsState>(
	i18n: I18n,
	key: K,
	value: DeveloperOptionsState[K],
): string => {
	if (key === 'mockAttachmentStates') {
		const count = Object.keys(value as DeveloperOptionsState['mockAttachmentStates']).length;
		return i18n._(ATTACHMENTS_PLURAL_DESCRIPTOR, {count});
	}
	if (value instanceof Date) return value.toLocaleDateString();
	if (typeof value === 'boolean') return value ? i18n._(ENABLED_DESCRIPTOR) : i18n._(FORCED_OFF_DESCRIPTOR);
	if (typeof value === 'number') return String(value);
	if (typeof value === 'string') return nonEmptyText(value.replace(/_/g, ' '), i18n._(CUSTOM_VALUE_DESCRIPTOR));
	if (value === null) return i18n._(USE_ACTUAL_VALUE_DESCRIPTOR);
	return i18n._(ACTIVE_DESCRIPTOR);
};
const formatDeveloperOptionValue = <K extends keyof DeveloperOptionsState>(
	i18n: I18n,
	key: K,
	value: DeveloperOptionsState[K],
): string => {
	switch (key) {
		case 'premiumTypeOverride':
			return getPremiumTypeLabel(i18n, value as DeveloperOptionsState['premiumTypeOverride']);
		case 'premiumLifetimeSequenceOverride':
			return value === null ? i18n._(USE_ACTUAL_VALUE_DESCRIPTOR) : `#${value}`;
		case 'mockBarrierTimeRemaining':
		case 'mockSlowmodeRemaining':
			return typeof value === 'number' ? formatDurationMs(i18n, value) : i18n._(USE_DEFAULT_DESCRIPTOR);
		case 'mockVerificationBarrier':
		case 'mockMatureContentGateReason':
		case 'mockMatureMediaGateReason':
		case 'mockRequiredActionsMode':
		case 'mockRequiredActionsSelectedTab':
		case 'mockRequiredActionsPhoneStep':
		case 'mockRequiredActionsResendOutcome':
		case 'mockTitlebarPlatformOverride':
		case 'mockUpdaterState':
		case 'gameCaptureInjectionMethod':
			return String(value).replace(/_/g, ' ');
		case 'premiumSinceOverride':
		case 'premiumUntilOverride':
			return value instanceof Date ? value.toLocaleDateString() : i18n._(USE_ACTUAL_VALUE_DESCRIPTOR);
		case 'premiumBillingCycleOverride':
			return typeof value === 'string' ? value : i18n._(USE_ACTUAL_VALUE_DESCRIPTOR);
		case 'unreadGiftInventoryCountOverride': {
			const count = value as number | null;
			return count === null ? i18n._(USE_ACTUAL_VALUE_DESCRIPTOR) : i18n._(GIFTS_PLURAL_DESCRIPTOR, {count});
		}
		case 'mockGiftDurationMonths': {
			const months = value as number | null;
			return months === 0
				? i18n._(LIFETIME_DESCRIPTOR)
				: months === null
					? i18n._(USE_DEFAULT_DESCRIPTOR)
					: i18n._(MONTHS_PLURAL_DESCRIPTOR, {months});
		}
		case 'mockAttachmentStates': {
			const count = Object.keys(value as DeveloperOptionsState['mockAttachmentStates']).length;
			return i18n._(ATTACHMENTS_PLURAL_DESCRIPTOR, {count});
		}
		default:
			if (typeof value === 'boolean') return value ? i18n._(ENABLED_DESCRIPTOR) : i18n._(FORCED_OFF_DESCRIPTOR);
			if (value === null) return i18n._(USE_ACTUAL_VALUE_DESCRIPTOR);
			return String(value);
	}
};
export const getDeveloperOptionLabel = (key: keyof DeveloperOptionsState): MessageDescriptor => {
	switch (key) {
		case 'bypassLoadingSkeleton':
			return BYPASS_LOADING_SKELETON_DESCRIPTOR;
		case 'forceLoadingSkeleton':
			return FORCE_LOADING_SKELETON_DESCRIPTOR;
		case 'forceFailMessageSends':
			return FAIL_MESSAGE_SENDS_DESCRIPTOR;
		case 'forceFailMessageLoads':
			return FAIL_MESSAGE_LOADS_DESCRIPTOR;
		case 'forceRenderPlaceholders':
			return RENDER_PLACEHOLDERS_DESCRIPTOR;
		case 'forceEmbedSkeletons':
			return EMBED_SKELETONS_DESCRIPTOR;
		case 'forceMediaLoading':
			return MEDIA_LOADING_STATE_DESCRIPTOR;
		case 'forceUpdateReady':
			return ANY_UPDATER_READY_DESCRIPTOR;
		case 'forceNativeUpdateReady':
			return NATIVE_UPDATER_READY_DESCRIPTOR;
		case 'mockNativeUpdateProgress':
			return NATIVE_UPDATE_PROGRESS_DESCRIPTOR;
		case 'forceWebUpdateReady':
			return WEB_UPDATER_READY_DESCRIPTOR;
		case 'mockUpdaterState':
			return UPDATER_STATE_DESCRIPTOR;
		case 'showMyselfTyping':
			return SHOW_MYSELF_TYPING_DESCRIPTOR;
		case 'slowAttachmentUpload':
			return SLOW_ATTACHMENT_UPLOAD_DESCRIPTOR;
		case 'slowMessageLoad':
			return SLOW_MESSAGE_LOAD_DESCRIPTOR;
		case 'slowMessageSend':
			return SLOW_MESSAGE_SEND_DESCRIPTOR;
		case 'slowMessageEdit':
			return SLOW_MESSAGE_EDIT_DESCRIPTOR;
		case 'slowProfileLoad':
			return SLOW_PROFILE_LOAD_DESCRIPTOR;
		case 'forceProfileSkeletons':
			return PROFILE_SKELETONS_DESCRIPTOR;
		case 'forceProfileDataWarning':
			return PROFILE_DATA_WARNING_DESCRIPTOR;
		case 'useCloudUpload':
			return CLOUD_UPLOAD_DESCRIPTOR;
		case 'forceGifPickerLoading':
			return GIF_PICKER_LOADING_DESCRIPTOR;
		case 'forceUnknownMessageType':
			return UNKNOWN_MESSAGE_TYPE_DESCRIPTOR;
		case 'selfHostedModeOverride':
			return SELF_HOSTED_MODE_DESCRIPTOR;
		case 'forceShowVanityURLDisclaimer':
			return VANITY_URL_DISCLAIMER_DESCRIPTOR;
		case 'forceShowVoiceConnection':
			return VOICE_CONNECTION_DEBUG_DESCRIPTOR;
		case 'gameCaptureInjectionMethod':
			return GAME_CAPTURE_INJECTION_METHOD_DESCRIPTOR;
		case 'premiumTypeOverride':
			return PREMIUM_TYPE_DESCRIPTOR;
		case 'premiumLifetimeSequenceOverride':
			return VISIONARY_BADGE_ID_DESCRIPTOR;
		case 'premiumSinceOverride':
			return PREMIUM_SINCE_DATE_DESCRIPTOR;
		case 'premiumUntilOverride':
			return PREMIUM_UNTIL_DATE_DESCRIPTOR;
		case 'premiumBillingCycleOverride':
			return BILLING_CYCLE_DESCRIPTOR;
		case 'premiumWillCancelOverride':
			return WILL_CANCEL_DESCRIPTOR;
		case 'hasEverPurchasedOverride':
			return HAS_EVER_PURCHASED_DESCRIPTOR;
		case 'hasUnreadGiftInventoryOverride':
			return UNREAD_GIFT_INVENTORY_DESCRIPTOR;
		case 'unreadGiftInventoryCountOverride':
			return UNREAD_GIFT_COUNT_DESCRIPTOR;
		case 'emailVerifiedOverride':
			return EMAIL_VERIFIED_DESCRIPTOR;
		case 'unclaimedAccountOverride':
			return UNCLAIMED_ACCOUNT_LABEL_DESCRIPTOR;
		case 'mockVerificationBarrier':
			return VERIFICATION_BARRIER_DESCRIPTOR;
		case 'mockBarrierTimeRemaining':
			return BARRIER_COUNTDOWN_DESCRIPTOR;
		case 'mockMatureContentGateReason':
			return MATURE_CHANNEL_GATE_DESCRIPTOR;
		case 'mockMatureMediaGateReason':
			return MATURE_MEDIA_GATE_DESCRIPTOR;
		case 'forceMatureMedia':
			return FORCE_MATURE_MEDIA_DESCRIPTOR;
		case 'mockInUK':
			return UK_GEO_DESCRIPTOR;
		case 'mockGeoBlocked':
			return GEO_BLOCK_OVERLAY_DESCRIPTOR;
		case 'mockRequiredActionsOverlay':
			return REQUIRED_ACTIONS_OVERLAY_DESCRIPTOR;
		case 'mockRequiredActionsMode':
			return REQUIRED_ACTION_VARIANT_DESCRIPTOR;
		case 'mockRequiredActionsSelectedTab':
			return REQUIRED_ACTION_DEFAULT_TAB_DESCRIPTOR;
		case 'mockRequiredActionsPhoneStep':
			return REQUIRED_ACTION_PHONE_STEP_DESCRIPTOR;
		case 'mockRequiredActionsResending':
			return REQUIRED_ACTION_RESEND_LOADING_DESCRIPTOR;
		case 'mockRequiredActionsResendOutcome':
			return REQUIRED_ACTION_RESEND_OUTCOME_DESCRIPTOR;
		case 'mockRequiredActionsReverify':
			return REQUIRED_ACTION_REVERIFICATION_TEXT_DESCRIPTOR;
		case 'forceNoSendMessages':
			return {...NO_SEND_PERMISSION_DESCRIPTOR, values: {sendMessagesPermission: SEND_MESSAGES_PERMISSION}};
		case 'forceNoAttachFiles':
			return {...NO_ATTACH_PERMISSION_DESCRIPTOR, values: {attachFilesPermission: ATTACH_FILES_PERMISSION}};
		case 'mockSlowmodeActive':
			return SLOWMODE_ACTIVE_DESCRIPTOR;
		case 'mockSlowmodeRemaining':
			return SLOWMODE_REMAINING_DESCRIPTOR;
		case 'mockGiftInventory':
			return GIFT_INVENTORY_DESCRIPTOR;
		case 'mockGiftDurationMonths':
			return GIFT_DURATION_DESCRIPTOR;
		case 'mockGiftRedeemed':
			return GIFT_REDEEMED_DESCRIPTOR;
		case 'mockTitlebarPlatformOverride':
			return TITLEBAR_PLATFORM_DESCRIPTOR;
		case 'mockAttachmentStates':
			return ATTACHMENT_MOCKS_DESCRIPTOR;
		default:
			return DEVELOPER_OPTION_DESCRIPTOR;
	}
};
export const getActiveDeveloperOptionEntries = (i18n: I18n): Array<ActiveOverrideEntry> => {
	return ACTIVE_DEVELOPER_OPTION_KEYS.filter((key) => !isDeveloperOptionAtDefault(key)).map((key) => {
		const currentValue = DeveloperOptions[key];
		return {
			key,
			label: nonEmptyText(
				translateDescriptor(i18n, getDeveloperOptionLabel(key)),
				getDeveloperOptionFallbackLabel(i18n, key),
			),
			value: nonEmptyText(
				formatDeveloperOptionValue(i18n, key, currentValue),
				getDeveloperOptionFallbackValue(i18n, key, currentValue),
			),
			reset: () => resetDeveloperOption(key),
		};
	});
};
