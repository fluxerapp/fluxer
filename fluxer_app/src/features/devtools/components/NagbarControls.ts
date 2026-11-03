// SPDX-License-Identifier: AGPL-3.0-or-later

import {ACCOUNT_LIMITED_DESCRIPTOR} from '@app/features/channel/components/channel_header_components/developer_tools/OptionPresets';
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
const SERVICE_INCIDENT_NAGBAR_DESCRIPTOR = msg({
	message: 'Service incident nagbar',
	comment:
		'Developer / debug surface — keep terse and technical. Label in the developer Nagbar controls panel for the service incident banner.',
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
const DOMAIN_MOVED_NAGBAR_DESCRIPTOR = msg({
	message: 'Domain moved nagbar',
	comment:
		'Developer or debug surface, keep terse and technical. Label in the developer Nagbar controls panel for the banner telling installed web apps that the app has moved to a new domain.',
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
interface NagbarControlOptions
	extends Pick<NagbarControlDefinition, 'key' | 'label' | 'forceKey' | 'forceHideKey' | 'resetKeys'> {
	actualStatus: MessageDescriptor;
	dismissedKey?: NagbarToggleKey;
}

function createNagbarControl({
	actualStatus,
	dismissedKey,
	...definition
}: NagbarControlOptions): NagbarControlDefinition {
	const {forceKey, forceHideKey} = definition;
	return {
		...definition,
		status(state) {
			if (state[forceKey]) {
				return FORCE_ENABLED;
			}
			if (state[forceHideKey]) {
				return FORCE_DISABLED;
			}
			if (dismissedKey !== undefined && state[dismissedKey]) {
				return CURRENTLY_DISMISSED;
			}
			return actualStatus;
		},
		useActualDisabled: (state) =>
			!state[forceKey] && (dismissedKey === undefined || !state[dismissedKey]) && !state[forceHideKey],
		forceShowDisabled: (state) => state[forceKey],
		forceHideDisabled: (state) => state[forceHideKey],
	};
}

export const getNagbarControls = (): Array<NagbarControlDefinition> => [
	createNagbarControl({
		key: 'forceUnclaimedAccount',
		label: UNCLAIMED_ACCOUNT_NAGBAR_DESCRIPTOR,
		forceKey: 'forceUnclaimedAccount',
		forceHideKey: 'forceHideUnclaimedAccount',
		resetKeys: ['forceUnclaimedAccount'],
		actualStatus: USING_ACTUAL_ACCOUNT_STATE,
	}),
	createNagbarControl({
		key: 'forceEmailVerification',
		label: EMAIL_VERIFICATION_NAGBAR_DESCRIPTOR,
		forceKey: 'forceEmailVerification',
		forceHideKey: 'forceHideEmailVerification',
		resetKeys: ['forceEmailVerification'],
		actualStatus: USING_ACTUAL_VERIFICATION_STATE,
	}),
	createNagbarControl({
		key: 'forceAccountLimited',
		label: ACCOUNT_LIMITED_DESCRIPTOR,
		forceKey: 'forceAccountLimited',
		forceHideKey: 'forceHideAccountLimited',
		resetKeys: ['forceAccountLimited'],
		actualStatus: USING_ACTUAL_ACCOUNT_STATE,
	}),
	createNagbarControl({
		key: 'forceDesktopNotification',
		label: DESKTOP_NOTIFICATION_NAGBAR_DESCRIPTOR,
		forceKey: 'forceDesktopNotification',
		forceHideKey: 'forceHideDesktopNotification',
		resetKeys: ['forceDesktopNotification', 'desktopNotificationDismissed'],
		actualStatus: CURRENTLY_SHOWING,
		dismissedKey: 'desktopNotificationDismissed',
	}),
	createNagbarControl({
		key: 'forcePremiumGracePeriod',
		label: PREMIUM_GRACE_PERIOD_NAGBAR_DESCRIPTOR,
		forceKey: 'forcePremiumGracePeriod',
		forceHideKey: 'forceHidePremiumGracePeriod',
		resetKeys: ['forcePremiumGracePeriod'],
		actualStatus: USING_ACTUAL_PREMIUM_STATE,
	}),
	createNagbarControl({
		key: 'forcePremiumExpired',
		label: PREMIUM_EXPIRED_NAGBAR_DESCRIPTOR,
		forceKey: 'forcePremiumExpired',
		forceHideKey: 'forceHidePremiumExpired',
		resetKeys: ['forcePremiumExpired'],
		actualStatus: USING_ACTUAL_PREMIUM_STATE,
	}),
	createNagbarControl({
		key: 'forcePremiumOnboarding',
		label: PREMIUM_ONBOARDING_NAGBAR_DESCRIPTOR,
		forceKey: 'forcePremiumOnboarding',
		forceHideKey: 'forceHidePremiumOnboarding',
		resetKeys: ['forcePremiumOnboarding', 'premiumOnboardingDismissed'],
		actualStatus: USING_ACTUAL_PREMIUM_STATE,
		dismissedKey: 'premiumOnboardingDismissed',
	}),
	createNagbarControl({
		key: 'forceGiftInventory',
		label: GIFT_INVENTORY_NAGBAR_DESCRIPTOR,
		forceKey: 'forceGiftInventory',
		forceHideKey: 'forceHideGiftInventory',
		resetKeys: ['forceGiftInventory', 'giftInventoryDismissed'],
		actualStatus: USING_ACTUAL_GIFT_INVENTORY_STATE,
		dismissedKey: 'giftInventoryDismissed',
	}),
	createNagbarControl({
		key: 'forceVisionaryMfa',
		label: VISIONARY_2FA_NAGBAR_DESCRIPTOR,
		forceKey: 'forceVisionaryMfa',
		forceHideKey: 'forceHideVisionaryMfa',
		resetKeys: ['forceVisionaryMfa', 'visionaryMfaDismissed'],
		actualStatus: USING_ACTUAL_STATE,
		dismissedKey: 'visionaryMfaDismissed',
	}),
	createNagbarControl({
		key: 'forceTermsAcceptance',
		label: TERMS_ACCEPTANCE_NAGBAR_DESCRIPTOR,
		forceKey: 'forceTermsAcceptance',
		forceHideKey: 'forceHideTermsAcceptance',
		resetKeys: ['forceTermsAcceptance'],
		actualStatus: USING_ACTUAL_STATE,
	}),
	createNagbarControl({
		key: 'forceDesktopDownload',
		label: DESKTOP_DOWNLOAD_NAGBAR_DESCRIPTOR,
		forceKey: 'forceDesktopDownload',
		forceHideKey: 'forceHideDesktopDownload',
		resetKeys: ['forceDesktopDownload', 'desktopDownloadDismissed'],
		actualStatus: USING_ACTUAL_STATE,
		dismissedKey: 'desktopDownloadDismissed',
	}),
	createNagbarControl({
		key: 'forceGuildMembershipCta',
		label: COMMUNITY_MEMBERSHIP_CTA_NAGBAR_DESCRIPTOR,
		forceKey: 'forceGuildMembershipCta',
		forceHideKey: 'forceHideGuildMembershipCta',
		resetKeys: ['forceGuildMembershipCta', 'guildMembershipCtaDismissed'],
		actualStatus: USING_ACTUAL_STATE,
		dismissedKey: 'guildMembershipCtaDismissed',
	}),
	createNagbarControl({
		key: 'forceCorruptedInstallation',
		label: CORRUPTED_INSTALLATION_NAGBAR_DESCRIPTOR,
		forceKey: 'forceCorruptedInstallation',
		forceHideKey: 'forceHideCorruptedInstallation',
		resetKeys: ['forceCorruptedInstallation'],
		actualStatus: USING_ACTUAL_STATE,
	}),
	createNagbarControl({
		key: 'forceScheduledMaintenance',
		label: SCHEDULED_MAINTENANCE_NAGBAR_DESCRIPTOR,
		forceKey: 'forceScheduledMaintenance',
		forceHideKey: 'forceHideScheduledMaintenance',
		resetKeys: ['forceScheduledMaintenance'],
		actualStatus: USING_ACTUAL_STATE,
	}),
	createNagbarControl({
		key: 'forceIncident',
		label: SERVICE_INCIDENT_NAGBAR_DESCRIPTOR,
		forceKey: 'forceConnectionNotice',
		forceHideKey: 'forceHideConnectionNotice',
		resetKeys: ['forceConnectionNotice'],
		actualStatus: USING_ACTUAL_STATE,
	}),
	createNagbarControl({
		key: 'forceVoiceSessionRestore',
		label: VOICE_SESSION_RESTORE_NAGBAR_DESCRIPTOR,
		forceKey: 'forceVoiceSessionRestore',
		forceHideKey: 'forceHideVoiceSessionRestore',
		resetKeys: ['forceVoiceSessionRestore'],
		actualStatus: USING_ACTUAL_STATE,
	}),
	createNagbarControl({
		key: 'forceInvitesDisabled',
		label: INVITES_DISABLED_NAGBAR_DESCRIPTOR,
		forceKey: 'forceInvitesDisabled',
		forceHideKey: 'forceHideInvitesDisabled',
		resetKeys: ['forceInvitesDisabled'],
		actualStatus: USING_ACTUAL_STATE,
	}),
	createNagbarControl({
		key: 'forceGuildMfaRequirement',
		label: GUILD_MFA_REQUIREMENT_NAGBAR_DESCRIPTOR,
		forceKey: 'forceGuildMfaRequirement',
		forceHideKey: 'forceHideGuildMfaRequirement',
		resetKeys: ['forceGuildMfaRequirement'],
		actualStatus: USING_ACTUAL_STATE,
	}),
	createNagbarControl({
		key: 'forceDomainMoved',
		label: DOMAIN_MOVED_NAGBAR_DESCRIPTOR,
		forceKey: 'forceDomainMoved',
		forceHideKey: 'forceHideDomainMoved',
		resetKeys: ['forceDomainMoved'],
		actualStatus: USING_ACTUAL_STATE,
	}),
];
