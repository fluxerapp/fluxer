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

import styles from '@app/components/popouts/UserProfileBadges.module.css';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {Routes} from '@app/Routes';
import type {ProfileRecord} from '@app/records/ProfileRecord';
import type {UserRecord} from '@app/records/UserRecord';
import RuntimeConfigStore from '@app/stores/RuntimeConfigStore';
import * as DateUtils from '@app/utils/DateUtils';
import {cdnUrl} from '@app/utils/UrlUtils';
import {PublicUserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

interface BaseBadge {
	key: string;
	tooltip: string;
	url: string;
}
interface IconBadge extends BaseBadge {
	type: 'icon';
	iconUrl: string;
}
interface TextBadge extends BaseBadge {
	type: 'text';
	text: string;
}
type Badge = IconBadge | TextBadge;

interface UserProfileBadgesProps {
	user: UserRecord;
	profile: ProfileRecord | null;
	isModal?: boolean;
	isMobile?: boolean;
}

export const UserProfileBadges: React.FC<UserProfileBadgesProps> = observer(
	({user, profile, isModal = false, isMobile = false}) => {
		const {t} = useLingui();
		const selfHosted = RuntimeConfigStore.isSelfHosted();

		const badges = useMemo(() => {
			const result: Array<Badge> = [];

			if (user.flags & PublicUserFlags.STAFF) {
				result.push({
					type: 'icon',
					key: 'staff',
					iconUrl: cdnUrl('badges/staff.svg'),
					tooltip: t`Fluxer Staff`,
					url: Routes.careers(),
				});
			}

			if (!selfHosted && user.flags & PublicUserFlags.CTP_MEMBER) {
				result.push({
					type: 'icon',
					key: 'ctp',
					iconUrl: cdnUrl('badges/ctp.svg'),
					tooltip: t`Fluxer Community Team`,
					url: Routes.careers(),
				});
			}

			if (!selfHosted && user.flags & PublicUserFlags.PARTNER) {
				result.push({
					type: 'icon',
					key: 'partner',
					iconUrl: cdnUrl('badges/partner.svg'),
					tooltip: t`Fluxer Partner`,
					url: Routes.partners(),
				});
			}

			if (!selfHosted && user.flags & PublicUserFlags.BUG_HUNTER) {
				result.push({
					type: 'icon',
					key: 'bug_hunter',
					iconUrl: cdnUrl('badges/bug-hunter.svg'),
					tooltip: t`Fluxer Bug Hunter`,
					url: Routes.bugs(),
				});
			}

			if (!selfHosted && profile?.premiumType && profile.premiumType !== UserPremiumTypes.NONE) {
				let tooltipText = t`Fluxer Plutonium`;
				let badgeUrl = Routes.plutonium();

				if (profile.premiumType === UserPremiumTypes.LIFETIME) {
					if (profile.premiumSince) {
						const premiumSinceFormatted = DateUtils.getFormattedShortDate(profile.premiumSince);
						tooltipText = `Fluxer Visionary since ${premiumSinceFormatted}`;
					} else {
						tooltipText = `Fluxer Visionary`;
					}
					badgeUrl = Routes.helpArticle('visionary');
				} else if (profile.premiumSince) {
					const premiumSinceFormatted = DateUtils.getFormattedShortDate(profile.premiumSince);
					tooltipText = `Fluxer Plutonium subscriber since ${premiumSinceFormatted}`;
				}

				result.push({
					type: 'icon',
					key: 'premium',
					iconUrl: cdnUrl('badges/plutonium.svg'),
					tooltip: tooltipText,
					url: badgeUrl,
				});

				if (profile.premiumType === UserPremiumTypes.LIFETIME && profile.premiumLifetimeSequence != null) {
					result.push({
						type: 'text',
						key: 'premium_sequence',
						text: `#${profile.premiumLifetimeSequence}`,
						tooltip: t`Visionary ID #${profile.premiumLifetimeSequence}`,
						url: badgeUrl,
					});
				}
			}

			return result;
		}, [selfHosted, user.flags, profile?.premiumType, profile?.premiumSince, profile?.premiumLifetimeSequence]);

		if (badges.length === 0) {
			return null;
		}

		const containerClassName = isModal
			? clsx(styles.containerModal, isMobile ? styles.containerModalMobile : styles.containerModalDesktop)
			: styles.containerPopout;

		const badgeClassName = isModal && isMobile ? styles.badgeMobile : styles.badgeDesktop;
		const isDesktopInteractions = !isMobile;

		const renderInteractiveWrapper = (url: string, children: React.ReactNode) => {
			if (isDesktopInteractions) {
				return (
					<a href={url} target="_blank" rel="noopener noreferrer" className={styles.link}>
						{children}
					</a>
				);
			}

			return <div className={styles.link}>{children}</div>;
		};

		return (
			<div className={containerClassName}>
				{badges.map((badge) => {
					const sequenceClassName = isModal && isMobile ? styles.sequenceBadgeMobile : styles.sequenceBadgeDesktop;
					const badgeContent =
						badge.type === 'icon' ? (
							<img src={badge.iconUrl} alt={badge.tooltip} className={badgeClassName} />
						) : (
							<span className={clsx(styles.sequenceBadge, sequenceClassName)} aria-hidden="true">
								{badge.text}
							</span>
						);

					return (
						<Tooltip key={badge.key} text={badge.tooltip} maxWidth="xl">
							<FocusRing offset={-2}>{renderInteractiveWrapper(badge.url, badgeContent)}</FocusRing>
						</Tooltip>
					);
				})}
			</div>
		);
	},
);
