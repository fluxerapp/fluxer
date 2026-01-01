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

import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {UserFlags, UserPremiumTypes} from '~/Constants';
import styles from '~/components/popouts/UserProfileBadges.module.css';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {Routes} from '~/Routes';
import type {ProfileRecord} from '~/records/ProfileRecord';
import type {UserRecord} from '~/records/UserRecord';
import * as DateUtils from '~/utils/DateUtils';
import {cdnUrl} from '~/utils/UrlUtils';

interface Badge {
	key: string;
	iconUrl: string;
	tooltip: string;
	url: string;
}

interface VirtualBadge {
	key: string;
	tooltip: string;
	url: string;
	component: React.ReactElement;
}

interface UserProfileBadgesProps {
	user: UserRecord;
	profile: ProfileRecord | null;
	isModal?: boolean;
	isMobile?: boolean;
	warningIndicator?: React.ReactNode;
}

export const UserProfileBadges: React.FC<UserProfileBadgesProps> = observer(
	({user, profile, isModal = false, isMobile = false, warningIndicator}) => {
		const {t} = useLingui();
		const badges = React.useMemo(() => {
			const result: Array<Badge> = [];

			if (user.flags & UserFlags.STAFF) {
				result.push({
					key: 'staff',
					iconUrl: cdnUrl('badges/staff.svg'),
					tooltip: t`Fluxer Staff`,
					url: Routes.careers(),
				});
			}

			if (user.flags & UserFlags.CTP_MEMBER) {
				result.push({
					key: 'ctp',
					iconUrl: cdnUrl('badges/ctp.svg'),
					tooltip: t`Fluxer Community Team`,
					url: Routes.careers(),
				});
			}

			if (user.flags & UserFlags.PARTNER) {
				result.push({
					key: 'partner',
					iconUrl: cdnUrl('badges/partner.svg'),
					tooltip: t`Fluxer Partner`,
					url: Routes.partners(),
				});
			}

			if (user.flags & UserFlags.BUG_HUNTER) {
				result.push({
					key: 'bug_hunter',
					iconUrl: cdnUrl('badges/bug-hunter.svg'),
					tooltip: t`Fluxer Bug Hunter`,
					url: Routes.bugs(),
				});
			}

			if (profile?.premiumType && profile.premiumType !== UserPremiumTypes.NONE) {
				let tooltipText = t`Fluxer Plutonium`;

				if (profile.premiumType === UserPremiumTypes.LIFETIME) {
					if (profile.premiumSince) {
						const premiumSinceFormatted = DateUtils.getFormattedShortDate(profile.premiumSince);
						tooltipText = `Fluxer Visionary since ${premiumSinceFormatted}`;
					} else {
						tooltipText = `Fluxer Visionary`;
					}
				} else if (profile.premiumSince) {
					const premiumSinceFormatted = DateUtils.getFormattedShortDate(profile.premiumSince);
					tooltipText = `Fluxer Plutonium subscriber since ${premiumSinceFormatted}`;
				}

				result.push({
					key: 'premium',
					iconUrl: cdnUrl('badges/plutonium.svg'),
					tooltip: tooltipText,
					url: Routes.plutonium(),
				});
			}

			return result;
		}, [user.flags, profile?.premiumType, profile?.premiumSince]);

		const virtualBadges = React.useMemo(() => {
			const result: Array<VirtualBadge> = [];

			if (profile?.premiumType === UserPremiumTypes.LIFETIME && profile.premiumLifetimeSequence != null) {
				const sequenceNumber = profile.premiumLifetimeSequence;

				result.push({
					key: 'lifetime_sequence',
					tooltip: `Visionary #${sequenceNumber}`,
					url: Routes.plutoniumVisionary(),
					component: (
						<div
							className={clsx(
								styles.virtualBadge,
								isModal && isMobile ? styles.virtualBadgeMobile : styles.virtualBadgeDesktop,
							)}
						>
							#{sequenceNumber}
						</div>
					),
				});
			}

			return result;
		}, [profile?.premiumType, profile?.premiumLifetimeSequence, isModal, isMobile]);

		if (badges.length === 0 && virtualBadges.length === 0) {
			return null;
		}

		const containerClassName = isModal
			? clsx(styles.containerModal, isMobile ? styles.containerModalMobile : styles.containerModalDesktop)
			: styles.containerPopout;

		const badgeClassName = isModal && isMobile ? styles.badgeMobile : styles.badgeDesktop;
		const isDesktopInteractions = !isMobile;

		const renderInteractiveWrapper = (url: string, children: React.ReactNode, style?: React.CSSProperties) => {
			if (isDesktopInteractions) {
				return (
					<a href={url} target="_blank" rel="noopener noreferrer" className={styles.link} style={style}>
						{children}
					</a>
				);
			}

			return (
				<div className={styles.link} style={style}>
					{children}
				</div>
			);
		};

		const virtualBadgeStyle: React.CSSProperties = {
			WebkitTapHighlightColor: 'transparent',
			WebkitTouchCallout: 'none',
		};

		return (
			<div className={containerClassName}>
				{warningIndicator}
				{badges.map((badge) => {
					const badgeContent = <img src={badge.iconUrl} alt={badge.tooltip} className={badgeClassName} />;

					return (
						<Tooltip key={badge.key} text={badge.tooltip} maxWidth="xl">
							<FocusRing offset={-2}>{renderInteractiveWrapper(badge.url, badgeContent)}</FocusRing>
						</Tooltip>
					);
				})}
				{virtualBadges.map((badge) => (
					<Tooltip key={badge.key} text={badge.tooltip} maxWidth="xl">
						<FocusRing offset={-2}>{renderInteractiveWrapper(badge.url, badge.component, virtualBadgeStyle)}</FocusRing>
					</Tooltip>
				))}
			</div>
		);
	},
);
