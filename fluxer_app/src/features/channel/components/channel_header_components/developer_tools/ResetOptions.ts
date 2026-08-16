// SPDX-License-Identifier: AGPL-3.0-or-later

import * as DeveloperOptionsCommands from '@app/features/devtools/commands/DeveloperOptionsCommands';
import type {DeveloperOptionsState} from '@app/features/devtools/state/DeveloperOptions';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';

export const DEFAULT_DEVELOPER_OPTIONS = {
	bypassLoadingSkeleton: false,
	forceLoadingSkeleton: false,
	forceFailMessageSends: false,
	forceFailMessageLoads: false,
	forceRenderPlaceholders: false,
	forceEmbedSkeletons: false,
	forceMediaLoading: false,
	forceUpdateReady: false,
	forceNativeUpdateReady: false,
	mockNativeUpdateProgress: null,
	forceWebUpdateReady: false,
	mockUpdaterState: 'none',
	showMyselfTyping: false,
	slowAttachmentUpload: false,
	slowMessageLoad: false,
	slowMessageSend: false,
	slowMessageEdit: false,
	slowProfileLoad: false,
	forceProfileSkeletons: false,
	forceProfileDataWarning: false,
	useCloudUpload: false,
	forceGifPickerLoading: false,
	forceUnknownMessageType: false,
	selfHostedModeOverride: false,
	forceShowVanityURLDisclaimer: false,
	forceShowVoiceConnection: false,
	showExpressionPacksSettings: false,
	showProfileTimezoneSettings: false,
	premiumScenarioOverride: null,
	premiumTypeOverride: null,
	premiumLifetimeSequenceOverride: null,
	premiumSinceOverride: null,
	premiumUntilOverride: null,
	premiumBillingCycleOverride: null,
	premiumWillCancelOverride: null,
	hasEverPurchasedOverride: null,
	hasUnreadGiftInventoryOverride: null,
	unreadGiftInventoryCountOverride: null,
	emailVerifiedOverride: null,
	unclaimedAccountOverride: null,
	mockVerificationBarrier: 'none',
	mockBarrierTimeRemaining: null,
	mockMatureContentGateReason: 'none',
	mockMatureMediaGateReason: 'none',
	forceMatureMedia: false,
	mockInUK: false,
	mockGeoBlocked: false,
	mockRequiredActionsOverlay: false,
	mockRequiredActionsMode: 'email',
	mockRequiredActionsSelectedTab: 'email',
	mockRequiredActionsPhoneStep: 'phone',
	mockRequiredActionsResending: false,
	mockRequiredActionsResendOutcome: 'success',
	mockRequiredActionsReverify: false,
	forceNoSendMessages: false,
	forceNoAttachFiles: false,
	mockSlowmodeActive: false,
	mockSlowmodeRemaining: 10000,
	mockGiftInventory: null,
	mockGiftDurationMonths: 12,
	mockGiftRedeemed: null,
	mockTitlebarPlatformOverride: 'auto',
	mockAttachmentStates: {},
	noOpInAppReports: false,
	gameCaptureInjectionMethod: 'auto',
} satisfies DeveloperOptionsState;
const PREMIUM_SCENARIO_OVERRIDE_KEYS = new Set<keyof DeveloperOptionsState>([
	'premiumTypeOverride',
	'premiumSinceOverride',
	'premiumUntilOverride',
	'premiumBillingCycleOverride',
	'premiumWillCancelOverride',
	'hasEverPurchasedOverride',
]);
export const DEVELOPER_OPTION_KEYS = Object.keys(DEFAULT_DEVELOPER_OPTIONS) as Array<keyof DeveloperOptionsState>;
export const updateOption = <K extends keyof DeveloperOptionsState>(key: K, value: DeveloperOptionsState[K]): void => {
	DeveloperOptionsCommands.updateOption(key, value);
	if (
		key !== 'premiumScenarioOverride' &&
		PREMIUM_SCENARIO_OVERRIDE_KEYS.has(key) &&
		DeveloperOptions.premiumScenarioOverride !== null
	) {
		DeveloperOptionsCommands.updateOption('premiumScenarioOverride', null);
	}
};
export const resetDeveloperOption = <K extends keyof DeveloperOptionsState>(key: K): void => {
	const shouldReload = key === 'selfHostedModeOverride' && DeveloperOptions.selfHostedModeOverride;
	if (key === 'mockAttachmentStates') {
		DeveloperOptionsCommands.clearAllAttachmentMocks();
	} else {
		updateOption(key, DEFAULT_DEVELOPER_OPTIONS[key]);
	}
	if (shouldReload) {
		window.location.reload();
	}
};
export const resetAllDeveloperOptions = (): void => {
	const shouldReload = DeveloperOptions.selfHostedModeOverride;
	for (const key of DEVELOPER_OPTION_KEYS) {
		if (key === 'mockAttachmentStates') {
			DeveloperOptionsCommands.clearAllAttachmentMocks();
		} else {
			updateOption(key, DEFAULT_DEVELOPER_OPTIONS[key]);
		}
	}
	if (shouldReload) {
		window.location.reload();
	}
};
export const isDeveloperOptionAtDefault = <K extends keyof DeveloperOptionsState>(key: K): boolean => {
	const value = DeveloperOptions[key];
	const defaultValue = DEFAULT_DEVELOPER_OPTIONS[key];
	if (key === 'mockAttachmentStates') {
		return Object.keys(value as DeveloperOptionsState['mockAttachmentStates']).length === 0;
	}
	if (value instanceof Date || defaultValue instanceof Date) {
		return value instanceof Date && defaultValue instanceof Date && value.getTime() === defaultValue.getTime();
	}
	return Object.is(value, defaultValue);
};
