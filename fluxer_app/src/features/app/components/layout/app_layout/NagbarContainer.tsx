// SPDX-License-Identifier: AGPL-3.0-or-later

import {type NagbarState, NagbarType} from '@app/features/app/components/layout/app_layout/AppLayoutTypes';
import styles from '@app/features/app/components/layout/app_layout/NagbarContainer.module.css';
import {CorruptedInstallationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/CorruptedInstallationNagbar';
import {DesktopDownloadNagbar} from '@app/features/app/components/layout/app_layout/nagbars/DesktopDownloadNagbar';
import {DesktopNotificationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/DesktopNotificationNagbar';
import {EmailVerificationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/EmailVerificationNagbar';
import {GiftInventoryNagbar} from '@app/features/app/components/layout/app_layout/nagbars/GiftInventoryNagbar';
import {GuildMembershipCtaNagbar} from '@app/features/app/components/layout/app_layout/nagbars/GuildMembershipCtaNagbar';
import {LinuxInputAccessNagbar} from '@app/features/app/components/layout/app_layout/nagbars/LinuxInputAccessNagbar';
import {PremiumExpiredNagbar} from '@app/features/app/components/layout/app_layout/nagbars/PremiumExpiredNagbar';
import {PremiumGracePeriodNagbar} from '@app/features/app/components/layout/app_layout/nagbars/PremiumGracePeriodNagbar';
import {PremiumOnboardingNagbar} from '@app/features/app/components/layout/app_layout/nagbars/PremiumOnboardingNagbar';
import {ScheduledMaintenanceNagbar} from '@app/features/app/components/layout/app_layout/nagbars/ScheduledMaintenanceNagbar';
import {StreamerModeNagbar} from '@app/features/app/components/layout/app_layout/nagbars/StreamerModeNagbar';
import {TermsAcceptanceNagbar} from '@app/features/app/components/layout/app_layout/nagbars/TermsAcceptanceNagbar';
import {UnclaimedAccountNagbar} from '@app/features/app/components/layout/app_layout/nagbars/UnclaimedAccountNagbar';
import {VisionaryMfaNagbar} from '@app/features/app/components/layout/app_layout/nagbars/VisionaryMfaNagbar';
import {VoiceSessionRestoreNagbar} from '@app/features/app/components/layout/app_layout/nagbars/VoiceSessionRestoreNagbar';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {SoftwareEncoderNagbar} from '@app/features/voice/components/SoftwareEncoderNagbar';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface NagbarContainerProps {
	nagbars: Array<NagbarState>;
}

export const NagbarContainer: React.FC<NagbarContainerProps> = observer(({nagbars}) => {
	const mobileLayout = MobileLayout;
	const showPremiumFeatures = shouldShowPremiumFeatures();
	if (nagbars.length === 0) return null;
	return (
		<div className={styles.container} data-flx="app.app-layout.nagbar-container.container">
			{nagbars.map((nagbar) => {
				switch (nagbar.type) {
					case NagbarType.CORRUPTED_INSTALLATION:
						return (
							<CorruptedInstallationNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.corrupted-installation-nagbar"
							/>
						);
					case NagbarType.TERMS_ACCEPTANCE:
						return (
							<TermsAcceptanceNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.terms-acceptance-nagbar"
							/>
						);
					case NagbarType.SCHEDULED_MAINTENANCE:
						return (
							<ScheduledMaintenanceNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.scheduled-maintenance-nagbar"
							/>
						);
					case NagbarType.UNCLAIMED_ACCOUNT:
						return (
							<UnclaimedAccountNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.unclaimed-account-nagbar"
							/>
						);
					case NagbarType.EMAIL_VERIFICATION:
						return (
							<EmailVerificationNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.email-verification-nagbar"
							/>
						);
					case NagbarType.DESKTOP_NOTIFICATION:
						return (
							<DesktopNotificationNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.desktop-notification-nagbar"
							/>
						);
					case NagbarType.PREMIUM_GRACE_PERIOD:
						if (!showPremiumFeatures) return null;
						return (
							<PremiumGracePeriodNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.premium-grace-period-nagbar"
							/>
						);
					case NagbarType.PREMIUM_EXPIRED:
						if (!showPremiumFeatures) return null;
						return (
							<PremiumExpiredNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.premium-expired-nagbar"
							/>
						);
					case NagbarType.PREMIUM_ONBOARDING:
						if (!showPremiumFeatures) return null;
						return (
							<PremiumOnboardingNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.premium-onboarding-nagbar"
							/>
						);
					case NagbarType.GIFT_INVENTORY:
						if (!showPremiumFeatures) return null;
						return (
							<GiftInventoryNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.gift-inventory-nagbar"
							/>
						);
					case NagbarType.DESKTOP_DOWNLOAD:
						return (
							<DesktopDownloadNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.desktop-download-nagbar"
							/>
						);
					case NagbarType.GUILD_MEMBERSHIP_CTA:
						return (
							<GuildMembershipCtaNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.guild-membership-cta-nagbar"
							/>
						);
					case NagbarType.VISIONARY_MFA:
						if (!showPremiumFeatures) return null;
						return (
							<VisionaryMfaNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.visionary-mfa-nagbar"
							/>
						);
					case NagbarType.VOICE_SESSION_RESTORE:
						return (
							<VoiceSessionRestoreNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.voice-session-restore-nagbar"
							/>
						);
					case NagbarType.LINUX_INPUT_ACCESS:
						return (
							<LinuxInputAccessNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.linux-input-access-nagbar"
							/>
						);
					case NagbarType.SOFTWARE_ENCODER:
						return (
							<SoftwareEncoderNagbar
								key={nagbar.type}
								data-flx="app.app-layout.nagbar-container.software-encoder-nagbar"
							/>
						);
					case NagbarType.STREAMER_MODE:
						return (
							<StreamerModeNagbar
								key={nagbar.type}
								isMobile={mobileLayout.enabled}
								data-flx="app.app-layout.nagbar-container.streamer-mode-nagbar"
							/>
						);
					default:
						return null;
				}
			})}
		</div>
	);
});
