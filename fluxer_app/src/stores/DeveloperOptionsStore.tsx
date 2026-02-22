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

import AppStorage from '@app/lib/AppStorage';
import {makePersistent} from '@app/lib/MobXPersistence';
import {makeAutoObservable} from 'mobx';

export type DeveloperOptionsState = Readonly<{
	bypassSplashScreen: boolean;
	forceFailMessageSends: boolean;
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
	forceProfileDataWarning: boolean;
	useCloudUpload: boolean;
	debugLogging: boolean;
	forceGifPickerLoading: boolean;
	forceUnknownMessageType: boolean;
	selfHostedModeOverride: boolean;
	forceShowVanityURLDisclaimer: boolean;
	forceShowVoiceConnection: boolean;
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
	mockNSFWGateReason: 'none' | 'geo_restricted' | 'age_restricted' | 'consent_required';
	mockNSFWMediaGateReason: 'none' | 'geo_restricted' | 'age_restricted';
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
}>;

type MutableDeveloperOptionsState = {
	-readonly [K in keyof DeveloperOptionsState]: DeveloperOptionsState[K];
};

class DeveloperOptionsStore implements DeveloperOptionsState {
	bypassSplashScreen = false;
	forceFailMessageSends = false;
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
	forceProfileDataWarning = false;
	useCloudUpload = false;
	debugLogging = false;
	forceGifPickerLoading = false;
	forceUnknownMessageType = false;
	selfHostedModeOverride = false;
	forceShowVanityURLDisclaimer = false;
	forceShowVoiceConnection = false;
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
	mockNSFWGateReason: 'none' | 'geo_restricted' | 'age_restricted' | 'consent_required' = 'none';
	mockNSFWMediaGateReason: 'none' | 'geo_restricted' | 'age_restricted' = 'none';
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

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'DeveloperOptionsStore', [
			'bypassSplashScreen',
			'forceFailMessageSends',
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
			'forceProfileDataWarning',
			'useCloudUpload',
			'debugLogging',
			'forceGifPickerLoading',
			'forceUnknownMessageType',
			'selfHostedModeOverride',
			'forceShowVanityURLDisclaimer',
			'forceShowVoiceConnection',
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
			'mockNSFWGateReason',
			'mockNSFWMediaGateReason',
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
		]);
	}

	updateOption<K extends keyof DeveloperOptionsStore & keyof DeveloperOptionsState>(
		key: K,
		value: DeveloperOptionsState[K],
	): void {
		(this as MutableDeveloperOptionsState)[key] = value;

		if (key === 'debugLogging') {
			AppStorage.setItem('debugLoggingEnabled', value?.toString() ?? 'false');
		}
	}
}

export default new DeveloperOptionsStore();
