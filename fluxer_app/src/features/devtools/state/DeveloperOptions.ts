// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {makeAutoObservable} from 'mobx';

export type PremiumScenarioOverride =
	| 'free_no_purchases'
	| 'free_with_history'
	| 'active_monthly'
	| 'active_monthly_cancelled'
	| 'active_yearly'
	| 'active_yearly_cancelled'
	| 'gift_active_no_history'
	| 'gift_active_with_history'
	| 'gift_expiring_soon'
	| 'gift_grace_period_active'
	| 'gift_expired_recent'
	| 'grace_period_active'
	| 'expired_recent'
	| 'expired_old'
	| 'visionary';
export type DeveloperOptionsState = Readonly<{
	bypassLoadingSkeleton: boolean;
	forceLoadingSkeleton: boolean;
	forceFailMessageSends: boolean;
	forceFailMessageLoads: boolean;
	forceRenderPlaceholders: boolean;
	forceEmbedSkeletons: boolean;
	forceMediaLoading: boolean;
	forceUpdateReady: boolean;
	forceNativeUpdateReady: boolean;
	mockNativeUpdateProgress: number | null;
	forceWebUpdateReady: boolean;
	mockUpdaterState: 'none' | 'checking' | 'available' | 'downloading' | 'ready' | 'installing' | 'error';
	showMyselfTyping: boolean;
	slowAttachmentUpload: boolean;
	slowMessageLoad: boolean;
	slowMessageSend: boolean;
	slowMessageEdit: boolean;
	slowProfileLoad: boolean;
	forceProfileSkeletons: boolean;
	forceProfileDataWarning: boolean;
	useCloudUpload: boolean;
	forceGifPickerLoading: boolean;
	forceUnknownMessageType: boolean;
	selfHostedModeOverride: boolean;
	forceShowVanityURLDisclaimer: boolean;
	forceShowVoiceConnection: boolean;
	showExpressionPacksSettings: boolean;
	showProfileTimezoneSettings: boolean;
	premiumScenarioOverride: PremiumScenarioOverride | null;
	premiumTypeOverride: number | null;
	premiumLifetimeSequenceOverride: number | null;
	premiumSinceOverride: Date | null;
	premiumUntilOverride: Date | null;
	premiumBillingCycleOverride: string | null;
	premiumWillCancelOverride: boolean | null;
	hasEverPurchasedOverride: boolean | null;
	hasUnreadGiftInventoryOverride: boolean | null;
	unreadGiftInventoryCountOverride: number | null;
	emailVerifiedOverride: boolean | null;
	unclaimedAccountOverride: boolean | null;
	mockVerificationBarrier:
		| 'none'
		| 'unclaimed_account'
		| 'unverified_email'
		| 'account_too_new'
		| 'not_member_long'
		| 'no_phone'
		| 'send_message_disabled';
	mockBarrierTimeRemaining: number | null;
	mockMatureContentGateReason: 'none' | 'geo_restricted' | 'mature_content_check_required' | 'consent_required';
	mockMatureMediaGateReason: 'none' | 'geo_restricted' | 'mature_content_check_required';
	forceMatureMedia: boolean;
	mockInUK: boolean;
	mockGeoBlocked: boolean;
	mockRequiredActionsOverlay: boolean;
	mockRequiredActionsMode: 'email' | 'phone' | 'email_or_phone';
	mockRequiredActionsSelectedTab: 'email' | 'phone';
	mockRequiredActionsPhoneStep: 'phone' | 'code';
	mockRequiredActionsResending: boolean;
	mockRequiredActionsResendOutcome: 'success' | 'rate_limited' | 'server_error';
	mockRequiredActionsReverify: boolean;
	forceNoSendMessages: boolean;
	forceNoAttachFiles: boolean;
	mockSlowmodeActive: boolean;
	mockSlowmodeRemaining: number;
	mockGiftInventory: boolean | null;
	mockGiftDurationMonths: number | null;
	mockGiftRedeemed: boolean | null;
	mockTitlebarPlatformOverride: 'auto' | 'macos' | 'windows' | 'linux';
	mockAttachmentStates: Record<
		string,
		{
			expired?: boolean;
			expiresAt?: string | null;
		}
	>;
	noOpInAppReports: boolean;
	gameCaptureInjectionMethod: 'auto' | 'remote-thread' | 'set-windows-hook';
}>;
type MutableDeveloperOptionsState = {
	-readonly [K in keyof DeveloperOptionsState]: DeveloperOptionsState[K];
};

class DeveloperOptions implements DeveloperOptionsState {
	bypassLoadingSkeleton = false;
	forceLoadingSkeleton = false;
	forceFailMessageSends = false;
	forceFailMessageLoads = false;
	forceRenderPlaceholders = false;
	forceEmbedSkeletons = false;
	forceMediaLoading = false;
	forceUpdateReady = false;
	forceNativeUpdateReady = false;
	mockNativeUpdateProgress: number | null = null;
	forceWebUpdateReady = false;
	mockUpdaterState: DeveloperOptionsState['mockUpdaterState'] = 'none';
	showMyselfTyping = false;
	slowAttachmentUpload = false;
	slowMessageLoad = false;
	slowMessageSend = false;
	slowMessageEdit = false;
	slowProfileLoad = false;
	forceProfileSkeletons = false;
	forceProfileDataWarning = false;
	useCloudUpload = false;
	forceGifPickerLoading = false;
	forceUnknownMessageType = false;
	selfHostedModeOverride = false;
	forceShowVanityURLDisclaimer = false;
	forceShowVoiceConnection = false;
	showExpressionPacksSettings = false;
	showProfileTimezoneSettings = false;
	premiumScenarioOverride: PremiumScenarioOverride | null = null;
	premiumTypeOverride: number | null = null;
	premiumLifetimeSequenceOverride: number | null = null;
	premiumSinceOverride: Date | null = null;
	premiumUntilOverride: Date | null = null;
	premiumBillingCycleOverride: string | null = null;
	premiumWillCancelOverride: boolean | null = null;
	hasEverPurchasedOverride: boolean | null = null;
	hasUnreadGiftInventoryOverride: boolean | null = null;
	unreadGiftInventoryCountOverride: number | null = null;
	emailVerifiedOverride: boolean | null = null;
	unclaimedAccountOverride: boolean | null = null;
	mockVerificationBarrier:
		| 'none'
		| 'unclaimed_account'
		| 'unverified_email'
		| 'account_too_new'
		| 'not_member_long'
		| 'no_phone'
		| 'send_message_disabled' = 'none';
	mockBarrierTimeRemaining: number | null = null;
	mockMatureContentGateReason: 'none' | 'geo_restricted' | 'mature_content_check_required' | 'consent_required' =
		'none';
	mockMatureMediaGateReason: 'none' | 'geo_restricted' | 'mature_content_check_required' = 'none';
	forceMatureMedia = false;
	mockInUK = false;
	mockGeoBlocked = false;
	mockRequiredActionsOverlay = false;
	mockRequiredActionsMode: 'email' | 'phone' | 'email_or_phone' = 'email';
	mockRequiredActionsSelectedTab: 'email' | 'phone' = 'email';
	mockRequiredActionsPhoneStep: 'phone' | 'code' = 'phone';
	mockRequiredActionsResending = false;
	mockRequiredActionsResendOutcome: 'success' | 'rate_limited' | 'server_error' = 'success';
	mockRequiredActionsReverify = false;
	forceNoSendMessages = false;
	forceNoAttachFiles = false;
	mockSlowmodeActive = false;
	mockSlowmodeRemaining = 10000;
	mockAttachmentStates: Record<
		string,
		{
			expired?: boolean;
			expiresAt?: string | null;
		}
	> = {};
	mockGiftInventory: boolean | null = null;
	mockGiftDurationMonths: number | null = 12;
	mockGiftRedeemed: boolean | null = null;
	mockTitlebarPlatformOverride: DeveloperOptionsState['mockTitlebarPlatformOverride'] = 'auto';
	noOpInAppReports = false;
	gameCaptureInjectionMethod: DeveloperOptionsState['gameCaptureInjectionMethod'] = 'auto';

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'DeveloperOptions', [
			'bypassLoadingSkeleton',
			'forceLoadingSkeleton',
			'forceFailMessageSends',
			'forceFailMessageLoads',
			'forceRenderPlaceholders',
			'forceEmbedSkeletons',
			'forceMediaLoading',
			'forceUpdateReady',
			'forceNativeUpdateReady',
			'mockNativeUpdateProgress',
			'forceWebUpdateReady',
			'mockUpdaterState',
			'showMyselfTyping',
			'slowAttachmentUpload',
			'slowMessageLoad',
			'slowMessageSend',
			'slowMessageEdit',
			'slowProfileLoad',
			'forceProfileSkeletons',
			'forceProfileDataWarning',
			'useCloudUpload',
			'forceGifPickerLoading',
			'forceUnknownMessageType',
			'selfHostedModeOverride',
			'forceShowVanityURLDisclaimer',
			'forceShowVoiceConnection',
			'showExpressionPacksSettings',
			'showProfileTimezoneSettings',
			'premiumScenarioOverride',
			'premiumTypeOverride',
			'premiumLifetimeSequenceOverride',
			'premiumSinceOverride',
			'premiumUntilOverride',
			'premiumBillingCycleOverride',
			'premiumWillCancelOverride',
			'hasEverPurchasedOverride',
			'hasUnreadGiftInventoryOverride',
			'unreadGiftInventoryCountOverride',
			'emailVerifiedOverride',
			'unclaimedAccountOverride',
			'mockVerificationBarrier',
			'mockBarrierTimeRemaining',
			'mockMatureContentGateReason',
			'mockMatureMediaGateReason',
			'forceMatureMedia',
			'mockInUK',
			'mockGeoBlocked',
			'mockRequiredActionsOverlay',
			'mockRequiredActionsMode',
			'mockRequiredActionsSelectedTab',
			'mockRequiredActionsPhoneStep',
			'mockRequiredActionsResending',
			'mockRequiredActionsResendOutcome',
			'mockRequiredActionsReverify',
			'forceNoSendMessages',
			'forceNoAttachFiles',
			'mockSlowmodeActive',
			'mockSlowmodeRemaining',
			'mockGiftInventory',
			'mockGiftDurationMonths',
			'mockGiftRedeemed',
			'mockTitlebarPlatformOverride',
			'mockAttachmentStates',
			'noOpInAppReports',
			'gameCaptureInjectionMethod',
		]);
	}

	updateOption<K extends keyof DeveloperOptions & keyof DeveloperOptionsState>(
		key: K,
		value: DeveloperOptionsState[K],
	): void {
		(this as MutableDeveloperOptionsState)[key] = value;
	}
}

export default new DeveloperOptions();
