// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const NagbarType = {
	CORRUPTED_INSTALLATION: 'corrupted-installation',
	SCHEDULED_MAINTENANCE: 'scheduled-maintenance',
	UNCLAIMED_ACCOUNT: 'unclaimed-account',
	EMAIL_VERIFICATION: 'email-verification',
	DESKTOP_NOTIFICATION: 'desktop-notification',
	PREMIUM_GRACE_PERIOD: 'premium-grace-period',
	PREMIUM_EXPIRED: 'premium-expired',
	PREMIUM_ONBOARDING: 'premium-onboarding',
	GIFT_INVENTORY: 'gift-inventory',
	DESKTOP_DOWNLOAD: 'desktop-download',
	GUILD_MEMBERSHIP_CTA: 'guild-membership-cta',
	VISIONARY_MFA: 'visionary-mfa',
	VOICE_SESSION_RESTORE: 'voice-session-restore',
	TERMS_ACCEPTANCE: 'terms-acceptance',
	LINUX_INPUT_ACCESS: 'linux-input-access',
	SOFTWARE_ENCODER: 'software-encoder',
	STREAMER_MODE: 'streamer-mode',
} as const;

export type NagbarType = ValueOf<typeof NagbarType>;

export interface NagbarState {
	type: NagbarType;
	priority: number;
	visible: boolean;
	dismissible: boolean;
}

export interface AppLayoutState {
	isStandalone: boolean;
}

export interface NagbarConditions {
	canShowCorruptedInstallation: boolean;
	canShowScheduledMaintenance: boolean;
	userIsUnclaimed: boolean;
	userNeedsVerification: boolean;
	canShowDesktopNotification: boolean;
	canShowPremiumGracePeriod: boolean;
	canShowPremiumExpired: boolean;
	canShowPremiumOnboarding: boolean;
	canShowGiftInventory: boolean;
	canShowDesktopDownload: boolean;
	canShowGuildMembershipCta: boolean;
	canShowVisionaryMfa: boolean;
	canShowVoiceSessionRestore: boolean;
	needsTermsAcceptance: boolean;
	canShowLinuxInputAccess: boolean;
	canShowSoftwareEncoder: boolean;
	canShowStreamerMode: boolean;
}
