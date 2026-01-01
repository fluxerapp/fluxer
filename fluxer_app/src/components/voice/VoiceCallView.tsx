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

import {
	FloatingFocusManager,
	flip,
	offset,
	shift,
	useClick,
	useDismiss,
	useFloating,
	useInteractions,
	useRole,
} from '@floating-ui/react';
import {useLingui} from '@lingui/react/macro';
import type {TrackReferenceOrPlaceholder} from '@livekit/components-react';
import {
	CarouselLayout,
	ParticipantContext,
	TrackRefContext,
	useConnectionState,
	useParticipants,
} from '@livekit/components-react';
import {
	ArrowLeftIcon,
	ArrowsOutIcon,
	CaretDownIcon,
	CaretUpIcon,
	ChartBarIcon,
	ListIcon,
	PhoneIcon,
	StarIcon,
	XIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {ConnectionState, type Participant} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import React, {forwardRef, useCallback, useMemo, useRef, useState} from 'react';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ME} from '~/Constants';
import {ChannelHeaderIcon} from '~/components/channel/ChannelHeader/ChannelHeaderIcon';
import {InboxButton} from '~/components/channel/ChannelHeader/UtilityButtons';
import {NativeDragRegion} from '~/components/layout/NativeDragRegion';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Scroller} from '~/components/uikit/Scroller';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {ChannelRecord} from '~/records/ChannelRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import ContextMenuStore, {isContextMenuNodeTarget} from '~/stores/ContextMenuStore';
import FavoritesStore from '~/stores/FavoritesStore';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PopoutStore from '~/stores/PopoutStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import channelHeaderStyles from '../channel/ChannelHeader.module.css';
import {CompactVoiceCallView} from './CompactVoiceCallView';
import {useVoiceCallTracksAndLayout} from './useVoiceCallTracksAndLayout';
import styles from './VoiceCallView.module.css';
import {VoiceControlBar} from './VoiceControlBar';
import {VoiceGridLayout} from './VoiceGridLayout';
import {VoiceParticipantTile} from './VoiceParticipantTile';
import {VoiceStatsOverlay} from './VoiceStatsOverlay';

interface VoiceCallViewProps {
	channel: ChannelRecord;
}

function useConnectionStateText(connectionState: ConnectionState, t: any) {
	return useMemo(() => {
		switch (connectionState) {
			case ConnectionState.Connecting:
				return t`Connecting...`;
			case ConnectionState.Reconnecting:
				return t`Reconnecting...`;
			case ConnectionState.Disconnected:
				return t`Disconnected`;
			default:
				return null;
		}
	}, [connectionState, t]);
}

function useFullscreen(containerRef: React.RefObject<HTMLElement | null>) {
	const [isFullscreen, setIsFullscreen] = useState(false);

	const toggleFullscreen = useCallback(() => {
		const el = containerRef.current;
		if (!el) return;

		if (document.fullscreenElement) {
			document.exitFullscreen().catch(() => {});
			return;
		}

		el.requestFullscreen().catch(() => {});
	}, [containerRef]);

	React.useEffect(() => {
		const onChange = () => setIsFullscreen(Boolean(document.fullscreenElement));
		document.addEventListener('fullscreenchange', onChange);
		return () => document.removeEventListener('fullscreenchange', onChange);
	}, []);

	return {isFullscreen, toggleFullscreen};
}

const VoiceCallViewInner = observer(({channel}: {channel: ChannelRecord}) => {
	const {t} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);

	const isMobile = MobileLayoutStore.isMobileLayout();
	const {keyboardModeEnabled} = KeyboardModeStore;

	const [isStatsOpen, setIsStatsOpen] = useState(false);
	const [isCallSheetOpen, setIsCallSheetOpen] = useState(false);

	const participants = useParticipants();
	const participantCount = participants.length;
	const connectionState = useConnectionState();
	const connectionStateText = useConnectionStateText(connectionState, t);

	const isInboxPopoutOpen = PopoutStore.isOpen('inbox');
	const isFavorited = channel ? Boolean(FavoritesStore.getChannel(channel.id)) : false;

	const isAnyContextMenuOpen = useMemo(() => {
		const cm = ContextMenuStore.contextMenu;
		const target = cm?.target?.target ?? null;
		const container = containerRef.current;
		if (!cm || !container || !isContextMenuNodeTarget(target)) return false;
		return Boolean(container.contains(target));
	}, [ContextMenuStore.contextMenu]);

	const {isFullscreen, toggleFullscreen} = useFullscreen(containerRef);

	const {
		layoutMode,
		pinnedParticipantIdentity,
		hasScreenShare,
		screenShareTracks,
		filteredCameraTracks,
		focusMainTrack,
		carouselTracks,
	} = useVoiceCallTracksAndLayout({channel});

	const showParticipantsCarousel = VoiceSettingsStore.getShowParticipantsCarousel();

	const {
		refs: statsRefs,
		floatingStyles: statsFloatingStyles,
		context: statsContext,
	} = useFloating({
		open: isStatsOpen,
		onOpenChange: setIsStatsOpen,
		placement: 'bottom-end',
		middleware: [offset(8), flip(), shift({padding: 8})],
	});

	const {getReferenceProps: getStatsReferenceProps, getFloatingProps: getStatsFloatingProps} = useInteractions([
		useClick(statsContext),
		useDismiss(statsContext),
		useRole(statsContext),
	]);
	const statsFloatingProps = isMobile ? {} : getStatsFloatingProps();

	React.useEffect(() => {
		if (!isMobile && isCallSheetOpen) {
			setIsCallSheetOpen(false);
		}
	}, [isCallSheetOpen, isMobile]);

	const handleBackClick = useCallback(() => window.history.back(), []);

	const handleToggleFavorite = useCallback(() => {
		if (!channel) return;

		if (isFavorited) {
			FavoritesStore.removeChannel(channel.id);
			ToastActionCreators.createToast({type: 'success', children: t`Channel removed from favorites`});
			return;
		}

		FavoritesStore.addChannel(channel.id, channel.guildId ?? ME);
		ToastActionCreators.createToast({type: 'success', children: t`Channel added to favorites`});
	}, [channel, isFavorited]);

	const handleToggleCarousel = useCallback(() => {
		VoiceSettingsStore.updateSettings({
			showParticipantsCarousel: !showParticipantsCarousel,
		});
	}, [showParticipantsCarousel]);

	const handleOpenCallSheet = useCallback(() => setIsCallSheetOpen(true), []);
	const handleCloseCallSheet = useCallback(() => setIsCallSheetOpen(false), []);

	const FavoriteIcon = useMemo(() => {
		const Icon = forwardRef<SVGSVGElement, React.ComponentProps<typeof StarIcon>>((props, ref) => (
			<StarIcon ref={ref} weight={isFavorited ? 'fill' : 'bold'} {...props} />
		));
		Icon.displayName = 'FavoriteIcon';
		return Icon;
	}, [isFavorited]);

	const focusLayoutNode = useMemo(() => {
		if (!focusMainTrack && carouselTracks.length === 0) {
			return (
				<div className={styles.gridLayoutWrapper}>
					<VoiceGridLayout tracks={filteredCameraTracks}>
						<VoiceParticipantTile guildId={channel.guildId} channelId={channel.id} />
					</VoiceGridLayout>
				</div>
			);
		}

		const hasCarousel = carouselTracks.length > 0;
		return (
			<div
				className={clsx(
					styles.focusLayoutContent,
					!hasCarousel && styles.noCarousel,
					hasCarousel && !showParticipantsCarousel && styles.carouselCollapsed,
				)}
			>
				<div className={styles.focusLayoutMainWrapper}>
					{focusMainTrack && (
						<div className={styles.focusLayoutMain}>
							<TrackRefContext.Provider value={focusMainTrack as TrackReferenceOrPlaceholder}>
								<ParticipantContext.Provider
									value={(focusMainTrack as TrackReferenceOrPlaceholder).participant as Participant}
								>
									<VoiceParticipantTile
										guildId={channel.guildId}
										channelId={channel.id}
										isPinned={
											(focusMainTrack as TrackReferenceOrPlaceholder).participant.identity === pinnedParticipantIdentity
										}
										showFocusIndicator={false}
									/>
								</ParticipantContext.Provider>
							</TrackRefContext.Provider>
						</div>
					)}
				</div>

				{carouselTracks.length > 0 && (
					<div className={styles.carouselToggleWrap}>
						<Tooltip text={showParticipantsCarousel ? t`Hide Participants` : t`Show Participants`} position="bottom">
							<FocusRing offset={-2} ringClassName={styles.carouselToggleFocusRing}>
								<button
									type="button"
									aria-label={showParticipantsCarousel ? t`Hide participants` : t`Show participants`}
									className={styles.carouselToggle}
									onClick={handleToggleCarousel}
								>
									{showParticipantsCarousel ? (
										<CaretDownIcon weight="bold" className={styles.iconMedium} />
									) : (
										<CaretUpIcon weight="bold" className={styles.iconMedium} />
									)}
								</button>
							</FocusRing>
						</Tooltip>
					</div>
				)}

				{carouselTracks.length > 0 && showParticipantsCarousel && (
					<div className={styles.carouselWrapper}>
						<Scroller
							orientation="horizontal"
							fade
							className={styles.scrollerFullWidth}
							key="voice-call-carousel-scroller"
						>
							<div className={styles.carouselInner}>
								<CarouselLayout tracks={carouselTracks}>
									<VoiceParticipantTile guildId={channel.guildId} channelId={channel.id} showFocusIndicator />
								</CarouselLayout>
							</div>
						</Scroller>
					</div>
				)}
			</div>
		);
	}, [
		focusMainTrack,
		carouselTracks,
		filteredCameraTracks,
		channel.guildId,
		channel.id,
		pinnedParticipantIdentity,
		showParticipantsCarousel,
		handleToggleCarousel,
	]);

	const gridLayoutNode = useMemo(() => {
		if (hasScreenShare) {
			const gridTracks = [...screenShareTracks, ...filteredCameraTracks];
			return (
				<div className={styles.gridLayoutWrapper}>
					<div className={styles.screenshareGridLayout}>
						<VoiceGridLayout tracks={gridTracks}>
							<VoiceParticipantTile guildId={channel.guildId} channelId={channel.id} />
						</VoiceGridLayout>
					</div>
				</div>
			);
		}

		return (
			<div className={styles.gridLayoutWrapper}>
				<VoiceGridLayout tracks={filteredCameraTracks}>
					<VoiceParticipantTile guildId={channel.guildId} channelId={channel.id} />
				</VoiceGridLayout>
			</div>
		);
	}, [hasScreenShare, screenShareTracks, filteredCameraTracks, channel.guildId, channel.id]);

	const mainContentNode = useMemo(() => {
		switch (layoutMode) {
			case 'focus':
				return focusLayoutNode;
			default:
				return gridLayoutNode;
		}
	}, [layoutMode, focusLayoutNode, gridLayoutNode]);

	const statsReferencePropsRaw = getStatsReferenceProps();
	const {ref: _statsRef, onClick: statsOnClickRaw, ...statsReferenceProps} = statsReferencePropsRaw;
	const statsOnClick = statsOnClickRaw as React.MouseEventHandler<HTMLButtonElement> | undefined;

	return (
		<div
			ref={containerRef}
			className={clsx(
				styles.root,
				styles.voiceRoot,
				(isAnyContextMenuOpen || isInboxPopoutOpen || isStatsOpen) && styles.contextMenuActive,
				keyboardModeEnabled && styles.keyboardModeActive,
			)}
		>
			<output className={styles.srOnly} aria-live="polite" aria-atomic="true">
				{participantCount === 1
					? t`${participantCount} participant in call`
					: t`${participantCount} participants in call`}
			</output>

			<NativeDragRegion className={clsx(channelHeaderStyles.headerContainer, styles.voiceChrome, styles.voiceHeader)}>
				<div className={channelHeaderStyles.headerLeftSection}>
					{isMobile ? (
						<FocusRing offset={-2}>
							<button type="button" className={channelHeaderStyles.backButton} onClick={handleBackClick}>
								<ArrowLeftIcon className={channelHeaderStyles.backIconBold} weight="bold" />
							</button>
						</FocusRing>
					) : (
						<FocusRing offset={-2}>
							<button type="button" className={channelHeaderStyles.backButtonDesktop} onClick={handleBackClick}>
								<ListIcon className={channelHeaderStyles.backIcon} />
							</button>
						</FocusRing>
					)}

					<div className={channelHeaderStyles.leftContentContainer}>
						<div className={channelHeaderStyles.channelInfoContainer}>
							{ChannelUtils.getIcon(channel, {className: channelHeaderStyles.channelIcon})}
							<span className={channelHeaderStyles.channelName}>{channel.name ?? ''}</span>
						</div>
					</div>
				</div>

				<div className={channelHeaderStyles.headerRightSection}>
					{channel && !isMobile && AccessibilityStore.showFavorites && (
						<ChannelHeaderIcon
							icon={FavoriteIcon}
							label={isFavorited ? t`Remove from Favorites` : t`Add to Favorites`}
							isSelected={isFavorited}
							onClick={handleToggleFavorite}
						/>
					)}

					{connectionStateText && (
						<div
							className={clsx(
								styles.connectionStatusContainer,
								connectionState === ConnectionState.Connecting && styles.statusConnecting,
								connectionState === ConnectionState.Reconnecting && styles.statusReconnecting,
								connectionState === ConnectionState.Disconnected && styles.statusDisconnected,
								connectionState === ConnectionState.Connected && styles.statusConnected,
							)}
						>
							<div className={styles.connectionStatusDot} />
							{connectionStateText}
						</div>
					)}

					<ChannelHeaderIcon
						ref={statsRefs.setReference}
						icon={ChartBarIcon}
						label={t`Connection Stats`}
						isSelected={isStatsOpen}
						onClick={statsOnClick}
						aria-expanded={isStatsOpen}
						{...statsReferenceProps}
					/>

					{isMobile && (
						<ChannelHeaderIcon icon={PhoneIcon} label={t`View call controls`} onClick={handleOpenCallSheet} />
					)}

					<ChannelHeaderIcon
						icon={isFullscreen ? XIcon : ArrowsOutIcon}
						label={isFullscreen ? t`Exit Fullscreen` : t`Fullscreen`}
						isSelected={isFullscreen}
						onClick={toggleFullscreen}
					/>

					{!isMobile && <InboxButton />}
				</div>
			</NativeDragRegion>

			<div className={styles.mainContent}>{mainContentNode}</div>

			<div className={clsx(styles.controlBarContainer, styles.voiceChrome)}>
				<VoiceControlBar />
			</div>

			{isStatsOpen &&
				(isMobile ? (
					<BottomSheet
						isOpen={isStatsOpen}
						onClose={() => setIsStatsOpen(false)}
						title={t`Connection Stats`}
						snapPoints={[0.3, 0.65, 0.9]}
					>
						<VoiceStatsOverlay onClose={() => setIsStatsOpen(false)} />
					</BottomSheet>
				) : (
					<FloatingFocusManager context={statsContext} modal={false}>
						<div ref={statsRefs.setFloating} style={{...statsFloatingStyles, zIndex: 30}} {...statsFloatingProps}>
							<VoiceStatsOverlay onClose={() => setIsStatsOpen(false)} />
						</div>
					</FloatingFocusManager>
				))}

			{isMobile && (
				<BottomSheet
					isOpen={isCallSheetOpen}
					onClose={handleCloseCallSheet}
					title={channel.name ?? t`Voice call`}
					snapPoints={[0.35, 0.65, 0.95]}
					disablePadding
					surface="primary"
				>
					<div className={styles.voiceCallSheetContent}>
						<CompactVoiceCallView channel={channel} className={styles.voiceCallSheetCompact} />
					</div>
				</BottomSheet>
			)}
		</div>
	);
});

export const VoiceCallView = observer(({channel}: VoiceCallViewProps) => <VoiceCallViewInner channel={channel} />);
