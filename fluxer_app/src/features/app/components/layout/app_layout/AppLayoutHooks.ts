// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AppLayoutState,
	type NagbarConditions,
	type NagbarState,
	NagbarType,
} from '@app/features/app/components/layout/app_layout/AppLayoutTypes';
import {isScheduledMaintenanceNagbarDismissed} from '@app/features/app/components/layout/app_layout/ScheduledMaintenanceDismissal';
import Initialization from '@app/features/app/state/Initialization';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import * as NotificationUtils from '@app/features/notification/utils/NotificationUtils';
import NativePermission from '@app/features/permissions/system/state/NativePermission';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import Nagbar from '@app/features/ui/state/Nagbar';
import {hasUnavailableElectronNativeContext, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {isStandalonePwa} from '@app/features/ui/utils/PwaUtils';
import StatusPage from '@app/features/user/state/StatusPage';
import Users from '@app/features/user/state/Users';
import MediaEngine, {useVoiceEngineV2Model} from '@app/features/voice/engine/MediaEngineFacade';
import {selectVoiceEngineV2AppConnectionWithFallback} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import CallState from '@app/features/voice/state/CallState';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import VoiceSessionRestore from '@app/features/voice/state/VoiceSessionRestore';
import {
	getVoiceSessionRestoreSnapshotKey,
	isRestorableVoiceChannelType,
} from '@app/features/voice/utils/VoiceSessionRestoreUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {PRIVACY_POLICY_LAST_UPDATED, TERMS_OF_SERVICE_LAST_UPDATED} from '@fluxer/constants/src/PolicyConstants';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {useEffect, useMemo, useState} from 'react';

function sortNagbarsByPriority(a: NagbarState, b: NagbarState): number {
	return a.priority - b.priority;
}

export function selectVisibleNagbars(nagbars: Array<NagbarState>): Array<NagbarState> {
	const visibleNagbars = nagbars.filter((nagbar) => nagbar.visible).sort(sortNagbarsByPriority);
	const nonDismissible = visibleNagbars.filter((nagbar) => !nagbar.dismissible);
	const dismissible = visibleNagbars.filter((nagbar) => nagbar.dismissible);
	const selectedNonDismissible = nonDismissible.slice(0, 1);
	const selectedDismissible = dismissible.slice(0, 1);
	return [...selectedNonDismissible, ...selectedDismissible].sort(sortNagbarsByPriority);
}

export const useAppLayoutState = (): AppLayoutState => {
	const [isStandalone, setIsStandalone] = useState(isStandalonePwa());
	useEffect(() => {
		const checkStandalone = () => {
			setIsStandalone(isStandalonePwa());
		};
		checkStandalone();
		document.documentElement.classList.toggle('is-standalone', isStandalone);
		return () => {
			document.documentElement.classList.remove('is-standalone');
		};
	}, [isStandalone]);
	return {isStandalone};
};
export const useNagbarConditions = (): NagbarConditions => {
	const voiceEngineV2Model = useVoiceEngineV2Model();
	const user = Users.currentUser;
	const nagbarState = Nagbar;
	const premiumOverrideType = DeveloperOptions.premiumTypeOverride;
	const premiumWillCancel = user?.premiumWillCancel ?? false;
	const isMockPremium = premiumOverrideType != null && premiumOverrideType > 0;
	const isSelfHosted = RuntimeConfig.isSelfHosted();
	const [startupVoiceSessionRestoreSnapshotKey, setStartupVoiceSessionRestoreSnapshotKey] = useState<
		string | null | undefined
	>(undefined);
	const currentUserId = Authentication.currentUserId;
	const voiceSessionRestoreHydrated = VoiceSessionRestore.isHydrated;
	const voiceSessionRestoreSnapshot = VoiceSessionRestore.getSnapshotForUser(currentUserId);
	const voiceSessionRestoreSnapshotKey = getVoiceSessionRestoreSnapshotKey(voiceSessionRestoreSnapshot);
	const voiceSessionRestoreChannel = voiceSessionRestoreSnapshot
		? Channels.getChannel(voiceSessionRestoreSnapshot.channelId)
		: null;
	const voiceConnectionContext = MediaEngine.connectionContext;
	const voiceEngineV2Connection = selectVoiceEngineV2AppConnectionWithFallback(voiceEngineV2Model, {
		connected: voiceConnectionContext.connected,
		connecting: voiceConnectionContext.connecting,
		reconnecting: voiceConnectionContext.reconnecting,
		guildId: voiceConnectionContext.guildId,
		channelId: voiceConnectionContext.channelId,
		sessionId: voiceConnectionContext.connectionId,
	});
	const shouldShowDesktopNotification = (() => {
		if (!NotificationUtils.hasNotification()) return false;
		if (typeof Notification !== 'undefined') {
			if (Notification.permission === 'granted') return false;
			if (Notification.permission === 'denied') return false;
		}
		return true;
	})();
	const canShowPremiumGracePeriod = (() => {
		if (isSelfHosted) return false;
		if (nagbarState.forceHidePremiumGracePeriod) return false;
		if (nagbarState.forcePremiumGracePeriod) return true;
		if (!user?.premiumUntil || user.premiumType === 2 || premiumWillCancel) return false;
		const now = new Date();
		const expiryDate = new Date(user.premiumUntil);
		const gracePeriodMs = 3 * MS_PER_DAY;
		const graceEndDate = new Date(expiryDate.getTime() + gracePeriodMs);
		const isInGracePeriod = now > expiryDate && now <= graceEndDate;
		return isInGracePeriod;
	})();
	const canShowPremiumExpired = (() => {
		if (isSelfHosted) return false;
		if (nagbarState.forceHidePremiumExpired) return false;
		if (nagbarState.forcePremiumExpired) return true;
		if (!user?.premiumUntil || user.premiumType === 2 || premiumWillCancel) return false;
		const now = new Date();
		const expiryDate = new Date(user.premiumUntil);
		const gracePeriodMs = 3 * MS_PER_DAY;
		const expiredStateDurationMs = 30 * MS_PER_DAY;
		const graceEndDate = new Date(expiryDate.getTime() + gracePeriodMs);
		const expiredStateEndDate = new Date(expiryDate.getTime() + expiredStateDurationMs);
		const isExpired = now > graceEndDate;
		const showExpiredState = isExpired && now <= expiredStateEndDate;
		return showExpiredState;
	})();
	const canShowGiftInventory = (() => {
		if (isSelfHosted) return false;
		if (nagbarState.forceHideGiftInventory) return false;
		if (nagbarState.forceGiftInventory) return true;
		return Boolean(user?.hasUnreadGiftInventory && !nagbarState.giftInventoryDismissed);
	})();
	const canShowPremiumOnboarding = (() => {
		if (isSelfHosted) return false;
		if (nagbarState.forceHidePremiumOnboarding) return false;
		if (nagbarState.forcePremiumOnboarding) return true;
		if (isMockPremium) return false;
		return Boolean(
			user?.isPremium() && !user?.hasDismissedPremiumOnboarding && !nagbarState.premiumOnboardingDismissed,
		);
	})();
	const isNativeDesktop = isDesktop();
	const hasBrokenElectronNativeContext = hasUnavailableElectronNativeContext();
	const isMobileDevice = /Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
	const isDesktopBrowser = !isNativeDesktop && !hasBrokenElectronNativeContext && !isMobileDevice;
	const canShowDesktopDownload = (() => {
		if (nagbarState.forceHideDesktopDownload) return false;
		if (nagbarState.forceDesktopDownload) return true;
		return isDesktopBrowser && !nagbarState.desktopDownloadDismissed;
	})();
	const canShowVisionaryMfa = (() => {
		if (isSelfHosted) return false;
		if (nagbarState.forceHideVisionaryMfa) return false;
		if (nagbarState.forceVisionaryMfa) return true;
		if (!user) return false;
		return user.premiumType === UserPremiumTypes.LIFETIME && !user.mfaEnabled && !nagbarState.visionaryMfaDismissed;
	})();
	const canShowGuildMembershipCta = (() => {
		if (nagbarState.forceHideGuildMembershipCta) return false;
		if (nagbarState.forceGuildMembershipCta) return true;
		if (!user) return false;
		if (isSelfHosted) return false;
		return !nagbarState.guildMembershipCtaDismissed;
	})();
	void nagbarState.scheduledMaintenanceDismissalVersion;
	const canShowScheduledMaintenance = (() => {
		if (nagbarState.forceHideScheduledMaintenance) return false;
		if (nagbarState.forceScheduledMaintenance) return true;
		const scheduledMaintenance = StatusPage.scheduledMaintenance;
		return Boolean(
			scheduledMaintenance &&
				!isScheduledMaintenanceNagbarDismissed(scheduledMaintenance.id, scheduledMaintenance.status),
		);
	})();
	useEffect(() => {
		if (!voiceSessionRestoreHydrated || !currentUserId || startupVoiceSessionRestoreSnapshotKey !== undefined) {
			return;
		}
		setStartupVoiceSessionRestoreSnapshotKey(voiceSessionRestoreSnapshotKey);
	}, [
		currentUserId,
		voiceSessionRestoreHydrated,
		voiceSessionRestoreSnapshotKey,
		startupVoiceSessionRestoreSnapshotKey,
	]);
	useEffect(() => {
		if (!voiceSessionRestoreHydrated || !currentUserId || !voiceSessionRestoreSnapshot) {
			return;
		}
		if (!voiceSessionRestoreChannel) {
			VoiceSessionRestore.clearSnapshot();
			setStartupVoiceSessionRestoreSnapshotKey(null);
			return;
		}
		if (!isRestorableVoiceChannelType(voiceSessionRestoreChannel.type)) {
			VoiceSessionRestore.clearSnapshot();
			setStartupVoiceSessionRestoreSnapshotKey(null);
		}
	}, [currentUserId, voiceSessionRestoreChannel, voiceSessionRestoreHydrated, voiceSessionRestoreSnapshot]);
	const canShowVoiceSessionRestore = (() => {
		if (nagbarState.forceHideVoiceSessionRestore) return false;
		if (nagbarState.forceVoiceSessionRestore) return true;
		if (!GatewayConnection.socket || !Initialization.canNavigateToProtectedRoutes) return false;
		if (!voiceSessionRestoreHydrated || !currentUserId) return false;
		if (!voiceSessionRestoreSnapshot || !voiceSessionRestoreChannel) return false;
		if (voiceEngineV2Connection.connected || voiceEngineV2Connection.connecting) return false;
		if (!isRestorableVoiceChannelType(voiceSessionRestoreChannel.type)) return false;
		if (
			(voiceSessionRestoreChannel.type === ChannelTypes.DM ||
				voiceSessionRestoreChannel.type === ChannelTypes.GROUP_DM) &&
			!CallState.hasActiveCall(voiceSessionRestoreChannel.id)
		) {
			return false;
		}
		return Boolean(
			startupVoiceSessionRestoreSnapshotKey && startupVoiceSessionRestoreSnapshotKey === voiceSessionRestoreSnapshotKey,
		);
	})();
	const canShowLinuxInputAccess = NativePermission.shouldShowLinuxInputAccessNagbar;
	const canShowSoftwareEncoder = SoftwareEncoderWarning.showWarning;
	const canShowStreamerMode = StreamerMode.shouldShowNagbar;
	const needsTermsAcceptance = (() => {
		if (!user) return false;
		if (isSelfHosted) return false;
		if (nagbarState.forceHideTermsAcceptance) return false;
		if (nagbarState.forceTermsAcceptance) return true;
		if (TERMS_OF_SERVICE_LAST_UPDATED == null && PRIVACY_POLICY_LAST_UPDATED == null) return false;
		const termsOutdated =
			TERMS_OF_SERVICE_LAST_UPDATED != null &&
			(!user.termsAgreedAt || user.termsAgreedAt.toISOString() < TERMS_OF_SERVICE_LAST_UPDATED);
		const privacyOutdated =
			PRIVACY_POLICY_LAST_UPDATED != null &&
			(!user.privacyAgreedAt || user.privacyAgreedAt.toISOString() < PRIVACY_POLICY_LAST_UPDATED);
		return termsOutdated || privacyOutdated;
	})();
	return {
		canShowCorruptedInstallation: nagbarState.forceHideCorruptedInstallation
			? false
			: nagbarState.forceCorruptedInstallation
				? true
				: hasBrokenElectronNativeContext,
		canShowScheduledMaintenance,
		userIsUnclaimed: nagbarState.forceHideUnclaimedAccount
			? false
			: nagbarState.forceUnclaimedAccount
				? true
				: Boolean(user && !user.isClaimed()),
		userNeedsVerification: nagbarState.forceHideEmailVerification
			? false
			: nagbarState.forceEmailVerification
				? true
				: Boolean(RuntimeConfig.emailsEnabled && user?.isClaimed() && !user.verified),
		canShowDesktopNotification: nagbarState.forceHideDesktopNotification
			? false
			: nagbarState.forceDesktopNotification
				? true
				: shouldShowDesktopNotification && !nagbarState.desktopNotificationDismissed,
		canShowPremiumGracePeriod,
		canShowPremiumExpired,
		canShowPremiumOnboarding,
		canShowGiftInventory,
		canShowDesktopDownload,
		canShowGuildMembershipCta,
		canShowVisionaryMfa,
		canShowVoiceSessionRestore,
		needsTermsAcceptance,
		canShowLinuxInputAccess,
		canShowSoftwareEncoder,
		canShowStreamerMode,
	};
};
export const useActiveNagbars = (conditions: NagbarConditions): Array<NagbarState> => {
	return useMemo(() => {
		const nagbars: Array<NagbarState> = [
			{
				type: NagbarType.CORRUPTED_INSTALLATION,
				priority: -10,
				visible: conditions.canShowCorruptedInstallation,
				dismissible: false,
			},
			{
				type: NagbarType.TERMS_ACCEPTANCE,
				priority: -5,
				visible: conditions.needsTermsAcceptance,
				dismissible: false,
			},
			{
				type: NagbarType.SCHEDULED_MAINTENANCE,
				priority: -1,
				visible: conditions.canShowScheduledMaintenance,
				dismissible: true,
			},
			{
				type: NagbarType.UNCLAIMED_ACCOUNT,
				priority: -4,
				visible: conditions.userIsUnclaimed,
				dismissible: false,
			},
			{
				type: NagbarType.EMAIL_VERIFICATION,
				priority: -3,
				visible: conditions.userNeedsVerification,
				dismissible: false,
			},
			{
				type: NagbarType.PREMIUM_EXPIRED,
				priority: 0,
				visible: conditions.canShowPremiumExpired,
				dismissible: true,
			},
			{
				type: NagbarType.PREMIUM_GRACE_PERIOD,
				priority: 1,
				visible: conditions.canShowPremiumGracePeriod,
				dismissible: true,
			},
			{
				type: NagbarType.VOICE_SESSION_RESTORE,
				priority: -3.5,
				visible: conditions.canShowVoiceSessionRestore,
				dismissible: true,
			},
			{
				type: NagbarType.PREMIUM_ONBOARDING,
				priority: 4,
				visible: conditions.canShowPremiumOnboarding,
				dismissible: true,
			},
			{
				type: NagbarType.GIFT_INVENTORY,
				priority: 5,
				visible: conditions.canShowGiftInventory,
				dismissible: true,
			},
			{
				type: NagbarType.GUILD_MEMBERSHIP_CTA,
				priority: 6,
				visible: conditions.canShowGuildMembershipCta,
				dismissible: true,
			},
			{
				type: NagbarType.VISIONARY_MFA,
				priority: 7,
				visible: conditions.canShowVisionaryMfa,
				dismissible: true,
			},
			{
				type: NagbarType.DESKTOP_NOTIFICATION,
				priority: 8,
				visible: conditions.canShowDesktopNotification,
				dismissible: true,
			},
			{
				type: NagbarType.LINUX_INPUT_ACCESS,
				priority: 8.5,
				visible: conditions.canShowLinuxInputAccess,
				dismissible: true,
			},
			{
				type: NagbarType.DESKTOP_DOWNLOAD,
				priority: 9,
				visible: conditions.canShowDesktopDownload,
				dismissible: true,
			},
			{
				type: NagbarType.SOFTWARE_ENCODER,
				priority: -2,
				visible: conditions.canShowSoftwareEncoder,
				dismissible: true,
			},
			{
				type: NagbarType.STREAMER_MODE,
				priority: -2.5,
				visible: conditions.canShowStreamerMode,
				dismissible: true,
			},
		];
		return selectVisibleNagbars(nagbars);
	}, [conditions]);
};
