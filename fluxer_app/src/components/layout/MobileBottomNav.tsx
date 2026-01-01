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

import {Trans} from '@lingui/react/macro';
import {BellIcon, HouseIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {VoiceLobbyBottomSheet} from '~/components/bottomsheets/VoiceLobbyBottomSheet';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {useConnectedVoiceSession} from '~/hooks/useConnectedVoiceSession';
import {useLocation} from '~/lib/router';
import {Routes} from '~/Routes';
import type {UserRecord} from '~/records/UserRecord';
import * as RouterUtils from '~/utils/RouterUtils';
import styles from './MobileBottomNav.module.css';

interface MobileBottomNavProps {
	currentUser: UserRecord;
}

export const MobileBottomNav = observer(({currentUser}: MobileBottomNavProps) => {
	const location = useLocation();
	const lastChannelPathRef = React.useRef<string | null>(null);
	const [voiceLobbyOpen, setVoiceLobbyOpen] = React.useState(false);

	const isHomeActive = Routes.isChannelRoute(location.pathname);
	const isNotificationsActive = location.pathname === Routes.NOTIFICATIONS;
	const isYouActive = location.pathname === Routes.YOU;

	const {channel: voiceChannel, guild: voiceGuild, isConnected: isConnectedToVoice} = useConnectedVoiceSession();

	React.useEffect(() => {
		if (Routes.isChannelRoute(location.pathname)) {
			lastChannelPathRef.current = location.pathname;
		}
	}, [location.pathname]);

	const handleHomeNavigation = () => {
		if (lastChannelPathRef.current && (isNotificationsActive || isYouActive)) {
			RouterUtils.transitionTo(lastChannelPathRef.current);
		} else {
			RouterUtils.transitionTo(Routes.ME);
		}
	};

	const handleNavigation = (path: string) => {
		RouterUtils.transitionTo(path);
	};

	const handleVoiceIndicatorPress = () => {
		setVoiceLobbyOpen(true);
	};

	const handleCloseVoiceLobby = () => {
		setVoiceLobbyOpen(false);
	};

	return (
		<>
			<div className={styles.container}>
				<button
					type="button"
					onClick={handleHomeNavigation}
					className={clsx(styles.navButton, isHomeActive ? styles.navButtonActive : styles.navButtonInactive)}
				>
					<HouseIcon weight="fill" className={styles.icon} />
					<span className={styles.label}>
						<Trans>Home</Trans>
					</span>
				</button>

				{isConnectedToVoice && (
					<button
						type="button"
						onClick={handleVoiceIndicatorPress}
						className={clsx(styles.navButton, styles.voiceButton)}
					>
						<SpeakerHighIcon weight="fill" className={styles.icon} />
						<span className={styles.label}>
							<Trans>Voice</Trans>
						</span>
					</button>
				)}

				<button
					type="button"
					onClick={() => handleNavigation(Routes.NOTIFICATIONS)}
					className={clsx(styles.navButton, isNotificationsActive ? styles.navButtonActive : styles.navButtonInactive)}
				>
					<BellIcon weight="fill" className={styles.icon} />
					<span className={styles.label}>
						<Trans>Notifications</Trans>
					</span>
				</button>

				<button
					type="button"
					onClick={() => handleNavigation(Routes.YOU)}
					className={clsx(styles.navButton, isYouActive ? styles.navButtonActive : styles.navButtonInactive)}
				>
					<StatusAwareAvatar user={currentUser} size={24} showOffline={true} />
					<span className={styles.label}>
						<Trans>You</Trans>
					</span>
				</button>
			</div>

			{isConnectedToVoice && voiceChannel && voiceGuild && (
				<VoiceLobbyBottomSheet
					isOpen={voiceLobbyOpen}
					onClose={handleCloseVoiceLobby}
					channel={voiceChannel}
					guild={voiceGuild}
				/>
			)}
		</>
	);
});
