// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import styles from '@app/features/app/components/layout/MobileBottomNav.module.css';
import {reportSkeletonVoiceConnected} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Channels from '@app/features/channel/state/Channels';
import {PRIMARY_NAVIGATION_LANDMARK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Navigation from '@app/features/navigation/state/Navigation';
import {getDirectMessagesFallbackPath} from '@app/features/navigation/utils/DefaultLandingUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {createNamedLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import type {User} from '@app/features/user/models/User';
import {useConnectedVoiceSession} from '@app/features/voice/hooks/useConnectedVoiceSession';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {BellIcon, HouseIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

const VoiceLobbyBottomSheet = createNamedLoadableComponent<Record<string, unknown>>({
	displayName: 'VoiceLobbyBottomSheet',
	load: async () =>
		(await import('@app/features/voice/components/bottomsheets/VoiceLobbyBottomSheet')).VoiceLobbyBottomSheet,
});
const DirectCallLobbyBottomSheet = createNamedLoadableComponent<Record<string, unknown>>({
	displayName: 'DirectCallLobbyBottomSheet',
	load: async () =>
		(await import('@app/features/voice/components/bottomsheets/DirectCallLobbyBottomSheet')).DirectCallLobbyBottomSheet,
});

interface MobileBottomNavProps {
	currentUser: User;
}

export const MobileBottomNav = observer(({currentUser}: MobileBottomNavProps) => {
	const {i18n} = useLingui();
	const location = useLocation();
	const lastChannelRef = useRef<{guildId: string | null; channelId: string | null} | null>(null);
	const [voiceLobbyOpen, setVoiceLobbyOpen] = useState(false);
	const [voiceLobbyLoaded, setVoiceLobbyLoaded] = useState(false);
	const isHomeActive = Routes.isChannelRoute(location.pathname);
	const isNotificationsActive = location.pathname === Routes.NOTIFICATIONS;
	const isYouActive = location.pathname === Routes.YOU;
	const {channel: voiceChannel, guild: voiceGuild, isConnected: isConnectedToVoice} = useConnectedVoiceSession();
	const isDirectCallChannel = Boolean(
		voiceChannel && (voiceChannel.type === ChannelTypes.DM || voiceChannel.type === ChannelTypes.GROUP_DM),
	);
	useSkeletonLayoutReport(() => {
		reportSkeletonVoiceConnected(isConnectedToVoice);
	}, `${isConnectedToVoice}`);
	useEffect(() => {
		if (Routes.isChannelRoute(location.pathname)) {
			const channel = Navigation.channelId ? Channels.getChannel(Navigation.channelId) : undefined;
			if (
				channel &&
				!channel.isPrivate() &&
				(channel.type === ChannelTypes.GUILD_CATEGORY || channel.type === ChannelTypes.GUILD_LINK)
			) {
				return;
			}
			lastChannelRef.current = {guildId: Navigation.guildId, channelId: Navigation.channelId};
		}
	}, [location.pathname]);
	const handleHomeNavigation = useCallback(() => {
		const last = lastChannelRef.current;
		if (last?.channelId && (isNotificationsActive || isYouActive)) {
			NavigationCommands.selectChannel(last.guildId ?? undefined, last.channelId);
		} else if (RuntimeConfig.directMessagesDisabled) {
			RouterUtils.transitionTo(getDirectMessagesFallbackPath());
		} else {
			NavigationCommands.deselectGuild();
		}
	}, [isNotificationsActive, isYouActive]);
	const handleNavigation = useCallback(
		(path: string) => {
			if (location.pathname === path) return;
			RouterUtils.transitionTo(path);
		},
		[location.pathname],
	);
	const handleVoiceIndicatorPress = useCallback(() => {
		setVoiceLobbyLoaded(true);
		setVoiceLobbyOpen(true);
	}, []);
	const handleCloseVoiceLobby = useCallback(() => {
		setVoiceLobbyOpen(false);
	}, []);
	return (
		<>
			<nav
				className={styles.container}
				aria-label={i18n._(PRIMARY_NAVIGATION_LANDMARK_DESCRIPTOR)}
				data-flx="app.mobile-bottom-nav.container"
			>
				<button
					type="button"
					onClick={handleHomeNavigation}
					className={clsx(styles.navButton, isHomeActive ? styles.navButtonActive : styles.navButtonInactive)}
					aria-current={isHomeActive ? 'page' : undefined}
					data-flx="app.mobile-bottom-nav.nav-button.home-navigation"
				>
					<HouseIcon weight="fill" className={styles.icon} data-flx="app.mobile-bottom-nav.icon" />
					<span className={styles.label} data-flx="app.mobile-bottom-nav.label">
						<Trans>Home</Trans>
					</span>
				</button>
				{isConnectedToVoice && (
					<button
						type="button"
						onClick={handleVoiceIndicatorPress}
						className={clsx(styles.navButton, styles.voiceButton)}
						aria-pressed={voiceLobbyOpen}
						data-flx="app.mobile-bottom-nav.nav-button.voice-indicator-press"
					>
						<SpeakerHighIcon weight="fill" className={styles.icon} data-flx="app.mobile-bottom-nav.icon--2" />
						<span className={styles.label} data-flx="app.mobile-bottom-nav.label--2">
							<Trans>Voice</Trans>
						</span>
					</button>
				)}
				<button
					type="button"
					onClick={() => handleNavigation(Routes.NOTIFICATIONS)}
					className={clsx(styles.navButton, isNotificationsActive ? styles.navButtonActive : styles.navButtonInactive)}
					aria-current={isNotificationsActive ? 'page' : undefined}
					data-flx="app.mobile-bottom-nav.nav-button.navigation"
				>
					<BellIcon weight="fill" className={styles.icon} data-flx="app.mobile-bottom-nav.icon--3" />
					<span className={styles.label} data-flx="app.mobile-bottom-nav.label--3">
						<Trans>Notifications</Trans>
					</span>
				</button>
				<button
					type="button"
					onClick={() => handleNavigation(Routes.YOU)}
					className={clsx(styles.navButton, isYouActive ? styles.navButtonActive : styles.navButtonInactive)}
					aria-current={isYouActive ? 'page' : undefined}
					data-flx="app.mobile-bottom-nav.nav-button.navigation--2"
				>
					<StatusAwareAvatar
						user={currentUser}
						size={24}
						showOffline={true}
						data-flx="app.mobile-bottom-nav.status-aware-avatar"
					/>
					<span className={styles.label} data-flx="app.mobile-bottom-nav.label--4">
						<Trans>You</Trans>
					</span>
				</button>
			</nav>
			{isConnectedToVoice && voiceChannel && voiceGuild && !isDirectCallChannel && voiceLobbyLoaded && (
				<VoiceLobbyBottomSheet
					isOpen={voiceLobbyOpen}
					onClose={handleCloseVoiceLobby}
					channel={voiceChannel}
					guild={voiceGuild}
					data-flx="app.mobile-bottom-nav.voice-lobby-bottom-sheet"
				/>
			)}
			{isConnectedToVoice && voiceChannel && isDirectCallChannel && voiceLobbyLoaded && (
				<DirectCallLobbyBottomSheet
					isOpen={voiceLobbyOpen}
					onClose={handleCloseVoiceLobby}
					channel={voiceChannel}
					data-flx="app.mobile-bottom-nav.direct-call-lobby-bottom-sheet"
				/>
			)}
		</>
	);
});
