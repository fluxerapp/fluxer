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

import {observer} from 'mobx-react-lite';
import type React from 'react';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import styles from './NagbarContainer.module.css';
import {DesktopDownloadNagbar} from './nagbars/DesktopDownloadNagbar';
import {DesktopNotificationNagbar} from './nagbars/DesktopNotificationNagbar';
import {EmailVerificationNagbar} from './nagbars/EmailVerificationNagbar';
import {GiftInventoryNagbar} from './nagbars/GiftInventoryNagbar';
import {MobileDownloadNagbar} from './nagbars/MobileDownloadNagbar';
import {PendingBulkDeletionNagbar} from './nagbars/PendingBulkDeletionNagbar';
import {PremiumExpiredNagbar} from './nagbars/PremiumExpiredNagbar';
import {PremiumGracePeriodNagbar} from './nagbars/PremiumGracePeriodNagbar';
import {PremiumOnboardingNagbar} from './nagbars/PremiumOnboardingNagbar';
import {UnclaimedAccountNagbar} from './nagbars/UnclaimedAccountNagbar';
import {type NagbarState, NagbarType} from './types';

interface NagbarContainerProps {
	nagbars: Array<NagbarState>;
}

export const NagbarContainer: React.FC<NagbarContainerProps> = observer(({nagbars}) => {
	const mobileLayout = MobileLayoutStore;

	if (nagbars.length === 0) return null;

	return (
		<div className={styles.container}>
			{nagbars.map((nagbar) => {
				switch (nagbar.type) {
					case NagbarType.UNCLAIMED_ACCOUNT:
						return <UnclaimedAccountNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.EMAIL_VERIFICATION:
						return <EmailVerificationNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.BULK_DELETE_PENDING:
						return <PendingBulkDeletionNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.DESKTOP_NOTIFICATION:
						return <DesktopNotificationNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.PREMIUM_GRACE_PERIOD:
						return <PremiumGracePeriodNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.PREMIUM_EXPIRED:
						return <PremiumExpiredNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.PREMIUM_ONBOARDING:
						return <PremiumOnboardingNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.GIFT_INVENTORY:
						return <GiftInventoryNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.DESKTOP_DOWNLOAD:
						return <DesktopDownloadNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					case NagbarType.MOBILE_DOWNLOAD:
						return <MobileDownloadNagbar key={nagbar.type} isMobile={mobileLayout.enabled} />;
					default:
						return null;
				}
			})}
		</div>
	);
});
