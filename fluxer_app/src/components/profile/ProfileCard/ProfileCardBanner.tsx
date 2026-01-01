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
import {useId} from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import type {UserRecord} from '~/records/UserRecord';
import styles from './ProfileCardBanner.module.css';

interface ProfileCardBannerProps {
	bannerUrl: string | null;
	bannerColor: string;
	user: UserRecord;
	avatarUrl: string | null;
	hoverAvatarUrl: string | null;
	disablePresence?: boolean;
	isClickable?: boolean;
	onAvatarClick?: () => void;
	headerHeight?: number;
}

export const ProfileCardBanner: React.FC<ProfileCardBannerProps> = observer(
	({
		bannerUrl,
		bannerColor,
		user,
		avatarUrl,
		hoverAvatarUrl,
		disablePresence = false,
		isClickable = true,
		onAvatarClick,
		headerHeight = 140,
	}) => {
		const bannerHeight = headerHeight === 140 ? 105 : 105;

		const reactId = useId();
		const safeId = reactId.replace(/[^a-zA-Z0-9_-]/g, '');
		const maskId = `uid_${safeId}`;

		const bannerStyle = {
			height: bannerHeight,
			minHeight: bannerHeight,
			backgroundColor: bannerColor,
			...(bannerUrl ? {backgroundImage: `url(${bannerUrl})`} : {}),
		};

		return (
			<header className={styles.headerSection} style={{height: headerHeight}}>
				<div className={styles.bannerWrapper} style={{minHeight: bannerHeight}}>
					{/* biome-ignore lint/a11y/noSvgWithoutTitle: this is fine */}
					<svg className={styles.bannerMask} viewBox="0 0 300 105" preserveAspectRatio="none">
						<mask id={maskId}>
							<rect fill="white" x="0" y="0" width="300" height="105" />
							<circle fill="black" cx="56" cy="101" r="46" />
						</mask>

						<foreignObject x="0" y="0" width="300" height="105" overflow="visible" mask={`url(#${maskId})`}>
							<div className={styles.banner} style={bannerStyle} />
						</foreignObject>
					</svg>
				</div>

				<FocusRing offset={-2}>
					<button type="button" onClick={onAvatarClick} className={styles.avatarButton}>
						<StatusAwareAvatar
							size={80}
							user={user}
							avatarUrl={avatarUrl}
							hoverAvatarUrl={hoverAvatarUrl}
							disablePresence={disablePresence}
							isClickable={isClickable}
						/>
					</button>
				</FocusRing>
			</header>
		);
	},
);
