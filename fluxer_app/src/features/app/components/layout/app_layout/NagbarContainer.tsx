// SPDX-License-Identifier: AGPL-3.0-or-later

import {type NagbarState, NagbarType} from '@app/features/app/components/layout/app_layout/AppLayoutTypes';
import styles from '@app/features/app/components/layout/app_layout/NagbarContainer.module.css';
import {AccountLimitedNagbar} from '@app/features/app/components/layout/app_layout/nagbars/AccountLimitedNagbar';
import {BuildEnvironmentNagbar} from '@app/features/app/components/layout/app_layout/nagbars/BuildEnvironmentNagbar';
import {ConnectionNagbar} from '@app/features/app/components/layout/app_layout/nagbars/ConnectionNagbar';
import {CorruptedInstallationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/CorruptedInstallationNagbar';
import {CustomCssOffNagbar} from '@app/features/app/components/layout/app_layout/nagbars/CustomCssOffNagbar';
import {DesktopDownloadNagbar} from '@app/features/app/components/layout/app_layout/nagbars/DesktopDownloadNagbar';
import {DesktopNotificationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/DesktopNotificationNagbar';
import {DomainMovedNagbar} from '@app/features/app/components/layout/app_layout/nagbars/DomainMovedNagbar';
import {EmailVerificationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/EmailVerificationNagbar';
import {GiftInventoryNagbar} from '@app/features/app/components/layout/app_layout/nagbars/GiftInventoryNagbar';
import {GuildMembershipCtaNagbar} from '@app/features/app/components/layout/app_layout/nagbars/GuildMembershipCtaNagbar';
import {InstanceNoticeNagbar} from '@app/features/app/components/layout/app_layout/nagbars/InstanceNoticeNagbar';
import {LegacyPriceOptInNagbar} from '@app/features/app/components/layout/app_layout/nagbars/LegacyPriceOptInNagbar';
import {PremiumOnboardingNagbar} from '@app/features/app/components/layout/app_layout/nagbars/PremiumOnboardingNagbar';
import {PremiumStatusNagbar} from '@app/features/app/components/layout/app_layout/nagbars/PremiumStatusNagbar';
import {PriceAnnouncementNagbar} from '@app/features/app/components/layout/app_layout/nagbars/PriceAnnouncementNagbar';
import {ScheduledMaintenanceNagbar} from '@app/features/app/components/layout/app_layout/nagbars/ScheduledMaintenanceNagbar';
import {StreamerModeNagbar} from '@app/features/app/components/layout/app_layout/nagbars/StreamerModeNagbar';
import {TermsAcceptanceNagbar} from '@app/features/app/components/layout/app_layout/nagbars/TermsAcceptanceNagbar';
import {UnclaimedAccountNagbar} from '@app/features/app/components/layout/app_layout/nagbars/UnclaimedAccountNagbar';
import {VisionaryMfaNagbar} from '@app/features/app/components/layout/app_layout/nagbars/VisionaryMfaNagbar';
import {VoiceSessionRestoreNagbar} from '@app/features/app/components/layout/app_layout/nagbars/VoiceSessionRestoreNagbar';
import {WindowsFontNagbar} from '@app/features/app/components/layout/app_layout/nagbars/WindowsFontNagbar';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {SoftwareEncoderNagbar} from '@app/features/voice/components/SoftwareEncoderNagbar';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface NagbarContainerProps {
	nagbars: Array<NagbarState>;
}

interface NagbarEntry {
	Component: React.ComponentType<{isMobile: boolean}>;
	premium?: boolean;
}

class UnexpectedNagbarTypeError extends Error {
	constructor(nagbarType: string) {
		super(`Unexpected nagbar type: ${nagbarType}`);
		this.name = 'UnexpectedNagbarTypeError';
	}
}

const PremiumGracePeriodNagbar = ({isMobile}: {isMobile: boolean}) => (
	<PremiumStatusNagbar
		variant="grace-period"
		isMobile={isMobile}
		data-flx="app.app-layout.nagbar-container.premium-grace-period-nagbar"
	/>
);

const PremiumExpiredNagbar = ({isMobile}: {isMobile: boolean}) => (
	<PremiumStatusNagbar
		variant="expired"
		isMobile={isMobile}
		data-flx="app.app-layout.nagbar-container.premium-expired-nagbar"
	/>
);

const NAGBARS: Record<NagbarType, NagbarEntry> = {
	[NagbarType.BUILD_ENVIRONMENT]: {Component: BuildEnvironmentNagbar},
	[NagbarType.CONNECTION]: {Component: ConnectionNagbar},
	[NagbarType.CORRUPTED_INSTALLATION]: {Component: CorruptedInstallationNagbar},
	[NagbarType.TERMS_ACCEPTANCE]: {Component: TermsAcceptanceNagbar},
	[NagbarType.SCHEDULED_MAINTENANCE]: {Component: ScheduledMaintenanceNagbar},
	[NagbarType.INSTANCE_NOTICE]: {Component: InstanceNoticeNagbar},
	[NagbarType.UNCLAIMED_ACCOUNT]: {Component: UnclaimedAccountNagbar},
	[NagbarType.EMAIL_VERIFICATION]: {Component: EmailVerificationNagbar},
	[NagbarType.ACCOUNT_LIMITED]: {Component: AccountLimitedNagbar},
	[NagbarType.DESKTOP_NOTIFICATION]: {Component: DesktopNotificationNagbar},
	[NagbarType.PREMIUM_GRACE_PERIOD]: {Component: PremiumGracePeriodNagbar, premium: true},
	[NagbarType.PREMIUM_EXPIRED]: {Component: PremiumExpiredNagbar, premium: true},
	[NagbarType.PREMIUM_ONBOARDING]: {Component: PremiumOnboardingNagbar, premium: true},
	[NagbarType.PRICE_ANNOUNCEMENT]: {Component: PriceAnnouncementNagbar, premium: true},
	[NagbarType.LEGACY_PRICE_OPT_IN]: {Component: LegacyPriceOptInNagbar, premium: true},
	[NagbarType.GIFT_INVENTORY]: {Component: GiftInventoryNagbar, premium: true},
	[NagbarType.DESKTOP_DOWNLOAD]: {Component: DesktopDownloadNagbar},
	[NagbarType.GUILD_MEMBERSHIP_CTA]: {Component: GuildMembershipCtaNagbar},
	[NagbarType.VISIONARY_MFA]: {Component: VisionaryMfaNagbar, premium: true},
	[NagbarType.VOICE_SESSION_RESTORE]: {Component: VoiceSessionRestoreNagbar},
	[NagbarType.SOFTWARE_ENCODER]: {Component: SoftwareEncoderNagbar},
	[NagbarType.STREAMER_MODE]: {Component: StreamerModeNagbar},
	[NagbarType.DOMAIN_MOVED]: {Component: DomainMovedNagbar},
	[NagbarType.WINDOWS_FONT]: {Component: WindowsFontNagbar},
	[NagbarType.CUSTOM_CSS_OFF]: {Component: CustomCssOffNagbar},
};

export const NagbarContainer: React.FC<NagbarContainerProps> = observer(({nagbars}) => {
	const mobileLayout = MobileLayout;
	const showPremiumFeatures = shouldShowPremiumFeatures();
	if (nagbars.length === 0) return null;
	return (
		<div className={styles.container} data-flx="app.app-layout.nagbar-container.container">
			{nagbars.map((nagbar) => {
				const entry: NagbarEntry | undefined = NAGBARS[nagbar.type];
				if (!entry) throw new UnexpectedNagbarTypeError(nagbar.type);
				if (entry.premium && !showPremiumFeatures) return null;
				return (
					<entry.Component
						key={nagbar.type}
						isMobile={mobileLayout.enabled}
						data-flx="app.app-layout.nagbar-container.nagbar"
					/>
				);
			})}
		</div>
	);
});
