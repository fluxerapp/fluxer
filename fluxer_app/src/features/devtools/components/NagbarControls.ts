// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Nagbar, NagbarToggleKey} from '@app/features/ui/state/Nagbar';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const DESKTOP_DOWNLOAD_NAGBAR_DESCRIPTOR = msg({
	message: 'Desktop download nagbar',
	comment:
		'Developer / debug surface — keep terse and technical. Label in the developer Nagbar controls panel for the desktop-download banner.',
});
const COMMUNITY_MEMBERSHIP_CTA_NAGBAR_DESCRIPTOR = msg({
	message: 'Community membership CTA nagbar',
	comment:
		'Developer / debug surface — keep terse and technical. Label in the developer Nagbar controls panel for the community-membership CTA banner.',
});
const CORRUPTED_INSTALLATION_NAGBAR_DESCRIPTOR = msg({
	message: 'Corrupted installation nagbar',
	comment:
		'Developer / debug surface — keep terse and technical. Label in the developer Nagbar controls panel for the corrupted-installation banner.',
});
const SCHEDULED_MAINTENANCE_NAGBAR_DESCRIPTOR = msg({
	message: 'Scheduled maintenance nagbar',
	comment:
		'Developer / debug surface — keep terse and technical. Label in the developer Nagbar controls panel for the scheduled-maintenance banner.',
});
const UNCLAIMED_ACCOUNT_NAGBAR_DESCRIPTOR = msg({
	message: 'Unclaimed account nagbar',
	comment: 'Developer control label for the unclaimed-account banner.',
});
const EMAIL_VERIFICATION_NAGBAR_DESCRIPTOR = msg({
	message: 'Email verification nagbar',
	comment: 'Developer control label for the email-verification banner.',
});
const DESKTOP_NOTIFICATION_NAGBAR_DESCRIPTOR = msg({
	message: 'Desktop notification nagbar',
	comment: 'Developer control label for the desktop-notification banner.',
});
const PREMIUM_GRACE_PERIOD_NAGBAR_DESCRIPTOR = msg({
	message: 'Premium grace period nagbar',
	comment: 'Developer control label for the premium grace-period banner.',
});
const PREMIUM_EXPIRED_NAGBAR_DESCRIPTOR = msg({
	message: 'Premium expired nagbar',
	comment: 'Developer control label for the premium-expired banner.',
});
const PREMIUM_ONBOARDING_NAGBAR_DESCRIPTOR = msg({
	message: 'Premium onboarding nagbar',
	comment: 'Developer control label for the premium-onboarding banner.',
});
const GIFT_INVENTORY_NAGBAR_DESCRIPTOR = msg({
	message: 'Gift inventory nagbar',
	comment: 'Developer control label for the gift-inventory banner.',
});
const VISIONARY_2FA_NAGBAR_DESCRIPTOR = msg({
	message: 'Visionary 2FA nagbar',
	comment: 'Developer control label for the Visionary two-factor-authentication banner.',
});
const TERMS_ACCEPTANCE_NAGBAR_DESCRIPTOR = msg({
	message: 'Terms acceptance nagbar',
	comment: 'Developer control label for the terms-acceptance banner.',
});
const VOICE_SESSION_RESTORE_NAGBAR_DESCRIPTOR = msg({
	message: 'Voice session restore nagbar',
	comment: 'Developer control label for the voice-session-restore banner.',
});
const INVITES_DISABLED_NAGBAR_DESCRIPTOR = msg({
	message: 'Invites disabled nagbar',
	comment: 'Developer control label for the invites-disabled banner.',
});
const GUILD_MFA_REQUIREMENT_NAGBAR_DESCRIPTOR = msg({
	message: 'Community MFA requirement nagbar',
	comment: 'Developer control label for the community MFA requirement banner.',
});

export interface NagbarControlDefinition {
	key: string;
	label: MessageDescriptor;
	forceKey: NagbarToggleKey;
	forceHideKey: NagbarToggleKey;
	resetKeys: Array<NagbarToggleKey>;
	status: (state: Nagbar) => MessageDescriptor;
	useActualDisabled?: (state: Nagbar) => boolean;
	forceShowDisabled?: (state: Nagbar) => boolean;
	forceHideDisabled?: (state: Nagbar) => boolean;
}

const FORCE_ENABLED = msg({
	message: 'Force enabled',
	comment: 'Developer nagbar override status: the banner is forced to show.',
});
const FORCE_DISABLED = msg({
	message: 'Force disabled',
	comment: 'Developer nagbar override status: the banner is forced to hide.',
});
const USING_ACTUAL_ACCOUNT_STATE = msg({
	message: 'Using actual account state',
	comment: 'Developer nagbar status: banner follows real account state.',
});
const USING_ACTUAL_VERIFICATION_STATE = msg({
	message: 'Using actual verification state',
	comment: 'Developer nagbar status: banner follows real email verification state.',
});
const CURRENTLY_DISMISSED = msg({
	message: 'Currently dismissed',
	comment: 'Developer nagbar status: the banner is dismissed in the current real state.',
});
const CURRENTLY_SHOWING = msg({
	message: 'Currently showing',
	comment: 'Developer nagbar status: the banner is showing in the current real state.',
});
const USING_ACTUAL_PREMIUM_STATE = msg({
	message: 'Using actual premium state',
	comment: 'Developer nagbar status: banner follows real premium subscription state.',
});
const USING_ACTUAL_GIFT_INVENTORY_STATE = msg({
	message: 'Using actual gift inventory state',
	comment: 'Developer nagbar status: banner follows real gift inventory state.',
});
const USING_ACTUAL_STATE = msg({
	message: 'Using actual state',
	comment: 'Developer nagbar status: banner follows the real app state.',
});
export const getNagbarControls = (): Array<NagbarControlDefinition> => [
	{
		key: 'forceUnclaimedAccount',
		label: UNCLAIMED_ACCOUNT_NAGBAR_DESCRIPTOR,
		forceKey: 'forceUnclaimedAccount',
		forceHideKey: 'forceHideUnclaimedAccount',
		resetKeys: ['forceUnclaimedAccount'],
		status: (state) =>
			state.forceUnclaimedAccount
				? FORCE_ENABLED
				: state.forceHideUnclaimedAccount
					? FORCE_DISABLED
					: USING_ACTUAL_ACCOUNT_STATE,
		useActualDisabled: (state) => !state.forceUnclaimedAccount && !state.forceHideUnclaimedAccount,
		forceShowDisabled: (state) => state.forceUnclaimedAccount,
		forceHideDisabled: (state) => state.forceHideUnclaimedAccount,
	},
	{
		key: 'forceEmailVerification',
		label: EMAIL_VERIFICATION_NAGBAR_DESCRIPTOR,
		forceKey: 'forceEmailVerification',
		forceHideKey: 'forceHideEmailVerification',
		resetKeys: ['forceEmailVerification'],
		status: (state) =>
			state.forceEmailVerification
				? FORCE_ENABLED
				: state.forceHideEmailVerification
					? FORCE_DISABLED
					: USING_ACTUAL_VERIFICATION_STATE,
		useActualDisabled: (state) => !state.forceEmailVerification && !state.forceHideEmailVerification,
		forceShowDisabled: (state) => state.forceEmailVerification,
		forceHideDisabled: (state) => state.forceHideEmailVerification,
	},
	{
		key: 'forceDesktopNotification',
		label: DESKTOP_NOTIFICATION_NAGBAR_DESCRIPTOR,
		forceKey: 'forceDesktopNotification',
		forceHideKey: 'forceHideDesktopNotification',
		resetKeys: ['forceDesktopNotification', 'desktopNotificationDismissed'],
		status: (state) =>
			state.forceDesktopNotification
				? FORCE_ENABLED
				: state.forceHideDesktopNotification
					? FORCE_DISABLED
					: state.desktopNotificationDismissed
						? CURRENTLY_DISMISSED
						: CURRENTLY_SHOWING,
		useActualDisabled: (state) =>
			!state.forceDesktopNotification && !state.desktopNotificationDismissed && !state.forceHideDesktopNotification,
		forceShowDisabled: (state) => state.forceDesktopNotification,
		forceHideDisabled: (state) => state.forceHideDesktopNotification,
	},
	{
		key: 'forcePremiumGracePeriod',
		label: PREMIUM_GRACE_PERIOD_NAGBAR_DESCRIPTOR,
		forceKey: 'forcePremiumGracePeriod',
		forceHideKey: 'forceHidePremiumGracePeriod',
		resetKeys: ['forcePremiumGracePeriod'],
		status: (state) =>
			state.forcePremiumGracePeriod
				? FORCE_ENABLED
				: state.forceHidePremiumGracePeriod
					? FORCE_DISABLED
					: USING_ACTUAL_PREMIUM_STATE,
		useActualDisabled: (state) => !state.forcePremiumGracePeriod && !state.forceHidePremiumGracePeriod,
		forceShowDisabled: (state) => state.forcePremiumGracePeriod,
		forceHideDisabled: (state) => state.forceHidePremiumGracePeriod,
	},
	{
		key: 'forcePremiumExpired',
		label: PREMIUM_EXPIRED_NAGBAR_DESCRIPTOR,
		forceKey: 'forcePremiumExpired',
		forceHideKey: 'forceHidePremiumExpired',
		resetKeys: ['forcePremiumExpired'],
		status: (state) =>
			state.forcePremiumExpired
				? FORCE_ENABLED
				: state.forceHidePremiumExpired
					? FORCE_DISABLED
					: USING_ACTUAL_PREMIUM_STATE,
		useActualDisabled: (state) => !state.forcePremiumExpired && !state.forceHidePremiumExpired,
		forceShowDisabled: (state) => state.forcePremiumExpired,
		forceHideDisabled: (state) => state.forceHidePremiumExpired,
	},
	{
		key: 'forcePremiumOnboarding',
		label: PREMIUM_ONBOARDING_NAGBAR_DESCRIPTOR,
		forceKey: 'forcePremiumOnboarding',
		forceHideKey: 'forceHidePremiumOnboarding',
		resetKeys: ['forcePremiumOnboarding', 'premiumOnboardingDismissed'],
		status: (state) =>
			state.forcePremiumOnboarding
				? FORCE_ENABLED
				: state.forceHidePremiumOnboarding
					? FORCE_DISABLED
					: state.premiumOnboardingDismissed
						? CURRENTLY_DISMISSED
						: USING_ACTUAL_PREMIUM_STATE,
		useActualDisabled: (state) =>
			!state.forcePremiumOnboarding && !state.premiumOnboardingDismissed && !state.forceHidePremiumOnboarding,
		forceShowDisabled: (state) => state.forcePremiumOnboarding,
		forceHideDisabled: (state) => state.forceHidePremiumOnboarding,
	},
	{
		key: 'forceGiftInventory',
		label: GIFT_INVENTORY_NAGBAR_DESCRIPTOR,
		forceKey: 'forceGiftInventory',
		forceHideKey: 'forceHideGiftInventory',
		resetKeys: ['forceGiftInventory', 'giftInventoryDismissed'],
		status: (state) =>
			state.forceGiftInventory
				? FORCE_ENABLED
				: state.forceHideGiftInventory
					? FORCE_DISABLED
					: state.giftInventoryDismissed
						? CURRENTLY_DISMISSED
						: USING_ACTUAL_GIFT_INVENTORY_STATE,
		useActualDisabled: (state) =>
			!state.forceGiftInventory && !state.giftInventoryDismissed && !state.forceHideGiftInventory,
		forceShowDisabled: (state) => state.forceGiftInventory,
		forceHideDisabled: (state) => state.forceHideGiftInventory,
	},
	{
		key: 'forceVisionaryMfa',
		label: VISIONARY_2FA_NAGBAR_DESCRIPTOR,
		forceKey: 'forceVisionaryMfa',
		forceHideKey: 'forceHideVisionaryMfa',
		resetKeys: ['forceVisionaryMfa', 'visionaryMfaDismissed'],
		status: (state) =>
			state.forceVisionaryMfa
				? FORCE_ENABLED
				: state.forceHideVisionaryMfa
					? FORCE_DISABLED
					: state.visionaryMfaDismissed
						? CURRENTLY_DISMISSED
						: USING_ACTUAL_STATE,
		useActualDisabled: (state) =>
			!state.forceVisionaryMfa && !state.visionaryMfaDismissed && !state.forceHideVisionaryMfa,
		forceShowDisabled: (state) => state.forceVisionaryMfa,
		forceHideDisabled: (state) => state.forceHideVisionaryMfa,
	},
	{
		key: 'forceTermsAcceptance',
		label: TERMS_ACCEPTANCE_NAGBAR_DESCRIPTOR,
		forceKey: 'forceTermsAcceptance',
		forceHideKey: 'forceHideTermsAcceptance',
		resetKeys: ['forceTermsAcceptance'],
		status: (state) =>
			state.forceTermsAcceptance ? FORCE_ENABLED : state.forceHideTermsAcceptance ? FORCE_DISABLED : USING_ACTUAL_STATE,
		useActualDisabled: (state) => !state.forceTermsAcceptance && !state.forceHideTermsAcceptance,
		forceShowDisabled: (state) => state.forceTermsAcceptance,
		forceHideDisabled: (state) => state.forceHideTermsAcceptance,
	},
	{
		key: 'forceDesktopDownload',
		label: DESKTOP_DOWNLOAD_NAGBAR_DESCRIPTOR,
		forceKey: 'forceDesktopDownload',
		forceHideKey: 'forceHideDesktopDownload',
		resetKeys: ['forceDesktopDownload', 'desktopDownloadDismissed'],
		status: (state) =>
			state.forceDesktopDownload
				? FORCE_ENABLED
				: state.forceHideDesktopDownload
					? FORCE_DISABLED
					: state.desktopDownloadDismissed
						? CURRENTLY_DISMISSED
						: USING_ACTUAL_STATE,
		useActualDisabled: (state) =>
			!state.forceDesktopDownload && !state.desktopDownloadDismissed && !state.forceHideDesktopDownload,
		forceShowDisabled: (state) => state.forceDesktopDownload,
		forceHideDisabled: (state) => state.forceHideDesktopDownload,
	},
	{
		key: 'forceGuildMembershipCta',
		label: COMMUNITY_MEMBERSHIP_CTA_NAGBAR_DESCRIPTOR,
		forceKey: 'forceGuildMembershipCta',
		forceHideKey: 'forceHideGuildMembershipCta',
		resetKeys: ['forceGuildMembershipCta', 'guildMembershipCtaDismissed'],
		status: (state) =>
			state.forceGuildMembershipCta
				? FORCE_ENABLED
				: state.forceHideGuildMembershipCta
					? FORCE_DISABLED
					: state.guildMembershipCtaDismissed
						? CURRENTLY_DISMISSED
						: USING_ACTUAL_STATE,
		useActualDisabled: (state) =>
			!state.forceGuildMembershipCta && !state.guildMembershipCtaDismissed && !state.forceHideGuildMembershipCta,
		forceShowDisabled: (state) => state.forceGuildMembershipCta,
		forceHideDisabled: (state) => state.forceHideGuildMembershipCta,
	},
	{
		key: 'forceCorruptedInstallation',
		label: CORRUPTED_INSTALLATION_NAGBAR_DESCRIPTOR,
		forceKey: 'forceCorruptedInstallation',
		forceHideKey: 'forceHideCorruptedInstallation',
		resetKeys: ['forceCorruptedInstallation'],
		status: (state) =>
			state.forceCorruptedInstallation
				? FORCE_ENABLED
				: state.forceHideCorruptedInstallation
					? FORCE_DISABLED
					: USING_ACTUAL_STATE,
		useActualDisabled: (state) => !state.forceCorruptedInstallation && !state.forceHideCorruptedInstallation,
		forceShowDisabled: (state) => state.forceCorruptedInstallation,
		forceHideDisabled: (state) => state.forceHideCorruptedInstallation,
	},
	{
		key: 'forceScheduledMaintenance',
		label: SCHEDULED_MAINTENANCE_NAGBAR_DESCRIPTOR,
		forceKey: 'forceScheduledMaintenance',
		forceHideKey: 'forceHideScheduledMaintenance',
		resetKeys: ['forceScheduledMaintenance'],
		status: (state) =>
			state.forceScheduledMaintenance
				? FORCE_ENABLED
				: state.forceHideScheduledMaintenance
					? FORCE_DISABLED
					: USING_ACTUAL_STATE,
		useActualDisabled: (state) => !state.forceScheduledMaintenance && !state.forceHideScheduledMaintenance,
		forceShowDisabled: (state) => state.forceScheduledMaintenance,
		forceHideDisabled: (state) => state.forceHideScheduledMaintenance,
	},
	{
		key: 'forceVoiceSessionRestore',
		label: VOICE_SESSION_RESTORE_NAGBAR_DESCRIPTOR,
		forceKey: 'forceVoiceSessionRestore',
		forceHideKey: 'forceHideVoiceSessionRestore',
		resetKeys: ['forceVoiceSessionRestore'],
		status: (state) =>
			state.forceVoiceSessionRestore
				? FORCE_ENABLED
				: state.forceHideVoiceSessionRestore
					? FORCE_DISABLED
					: USING_ACTUAL_STATE,
		useActualDisabled: (state) => !state.forceVoiceSessionRestore && !state.forceHideVoiceSessionRestore,
		forceShowDisabled: (state) => state.forceVoiceSessionRestore,
		forceHideDisabled: (state) => state.forceHideVoiceSessionRestore,
	},
	{
		key: 'forceInvitesDisabled',
		label: INVITES_DISABLED_NAGBAR_DESCRIPTOR,
		forceKey: 'forceInvitesDisabled',
		forceHideKey: 'forceHideInvitesDisabled',
		resetKeys: ['forceInvitesDisabled'],
		status: (state) =>
			state.forceInvitesDisabled ? FORCE_ENABLED : state.forceHideInvitesDisabled ? FORCE_DISABLED : USING_ACTUAL_STATE,
		useActualDisabled: (state) => !state.forceInvitesDisabled && !state.forceHideInvitesDisabled,
		forceShowDisabled: (state) => state.forceInvitesDisabled,
		forceHideDisabled: (state) => state.forceHideInvitesDisabled,
	},
	{
		key: 'forceGuildMfaRequirement',
		label: GUILD_MFA_REQUIREMENT_NAGBAR_DESCRIPTOR,
		forceKey: 'forceGuildMfaRequirement',
		forceHideKey: 'forceHideGuildMfaRequirement',
		resetKeys: ['forceGuildMfaRequirement'],
		status: (state) =>
			state.forceGuildMfaRequirement
				? FORCE_ENABLED
				: state.forceHideGuildMfaRequirement
					? FORCE_DISABLED
					: USING_ACTUAL_STATE,
		useActualDisabled: (state) => !state.forceGuildMfaRequirement && !state.forceHideGuildMfaRequirement,
		forceShowDisabled: (state) => state.forceGuildMfaRequirement,
		forceHideDisabled: (state) => state.forceHideGuildMfaRequirement,
	},
];
