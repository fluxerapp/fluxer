// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {
	createVoiceCallLayoutPresentationSnapshot,
	selectVoiceCallLayoutPresentationModel,
	transitionVoiceCallLayoutPresentationSnapshot,
	type VoiceCallLayoutPresentationInput,
} from '@app/features/voice/components/VoiceCallLayoutPresentationStateMachine';
import styles from '@app/features/voice/components/VoiceCallView.module.css';
import {VoiceGridLayout} from '@app/features/voice/components/VoiceGridLayout';
import type {VoiceGridEntry} from '@app/features/voice/components/VoiceParticipantConsolidation';
import {VoiceParticipantTile} from '@app/features/voice/components/VoiceParticipantTile';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ParticipantContext, TrackRefContext, type TrackReferenceOrPlaceholder} from '@livekit/components-react';
import {CaretDownIcon, CaretUpIcon, UsersIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type {Participant} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

function trackToEntry(trackRef: TrackReferenceOrPlaceholder, index: number): VoiceGridEntry {
	const key =
		trackRef.source != null
			? `${trackRef.participant.identity}-${trackRef.source}`
			: `placeholder-${trackRef.participant.identity}-${index}`;
	return {
		kind: 'track',
		key,
		trackRef,
		hiddenConnectionCount: 0,
		deviceConnectionCount: 1,
		isDeviceGroupExpanded: false,
		userId: null,
	};
}

const HIDE_PARTICIPANTS_DESCRIPTOR = msg({
	message: 'Hide participants',
	comment: 'Tooltip / aria label on the participants-panel toggle in the voice call view (currently visible).',
});
const SHOW_PARTICIPANTS_DESCRIPTOR = msg({
	message: 'Show participants',
	comment: 'Tooltip / aria label on the participants-panel toggle in the voice call view (currently hidden).',
});
const HIDE_MEMBERS_DESCRIPTOR = msg({
	message: 'Hide members',
	comment: 'Tooltip / aria label on the members-row toggle pill above the voice control bar (currently visible).',
});
const SHOW_MEMBERS_DESCRIPTOR = msg({
	message: 'Show members',
	comment: 'Tooltip / aria label on the members-row toggle pill above the voice control bar (currently hidden).',
});

type LayoutMode = 'grid' | 'focus';

interface VoiceCallLayoutContentProps {
	channel: Channel;
	layoutMode: LayoutMode;
	focusMainTrack: TrackReferenceOrPlaceholder | null;
	carouselTracks: Array<TrackReferenceOrPlaceholder>;
	filteredCameraTracks: Array<TrackReferenceOrPlaceholder>;
	gridEntries: Array<VoiceGridEntry>;
	hasScreenShare: boolean;
	pinnedParticipantIdentity: string | null;
	compact?: boolean;
	isVoiceCallAppFullscreen?: boolean;
	onGridCapacityChange?: (info: {visibleTileCount: number; totalTileCount: number; overflow: boolean}) => void;
	onExpandUser: (userId: string) => void;
}

interface FocusLayoutStyle extends React.CSSProperties {
	'--content-padding'?: string;
	'--focus-mini-tile-max-width'?: string;
	'--focus-mini-grid-gap'?: string;
}

interface FocusMainStyle extends React.CSSProperties {
	'--focus-main-aspect-ratio-value'?: string;
}

const COMPACT_FOCUS_STYLE: FocusLayoutStyle = {
	'--content-padding': '0.5rem',
	'--focus-mini-tile-max-width': '212px',
	'--focus-mini-grid-gap': '0.5rem',
};
const DEFAULT_FOCUS_MAIN_ASPECT_RATIO = 16 / 9;
export const VoiceCallLayoutContent = observer(function VoiceCallLayoutContent({
	channel,
	layoutMode,
	focusMainTrack,
	carouselTracks,
	filteredCameraTracks,
	gridEntries,
	hasScreenShare,
	pinnedParticipantIdentity,
	compact = false,
	isVoiceCallAppFullscreen = false,
	onGridCapacityChange,
	onExpandUser,
}: VoiceCallLayoutContentProps) {
	const {i18n} = useLingui();
	const focusLayoutStyle = useMemo(() => (compact ? COMPACT_FOCUS_STYLE : undefined), [compact]);
	const focusMainAspectRatio = DEFAULT_FOCUS_MAIN_ASPECT_RATIO;
	const focusMainStyle = useMemo<FocusMainStyle>(() => {
		return {
			'--focus-main-aspect-ratio-value': `${focusMainAspectRatio}`,
		};
	}, [focusMainAspectRatio]);
	const secondaryFocusTracks = useMemo(() => {
		if (!focusMainTrack) return carouselTracks;
		return carouselTracks.filter(
			(trackRef) =>
				trackRef.participant.identity !== focusMainTrack.participant.identity ||
				trackRef.source !== focusMainTrack.source,
		);
	}, [carouselTracks, focusMainTrack]);
	const membersRowVisible = VoiceCallLayout.focusMembersRowVisible;
	const presentationInput = useMemo<VoiceCallLayoutPresentationInput>(
		() => ({
			layoutMode,
			hasFocusMainTrack: focusMainTrack != null,
			secondaryFocusTrackCount: secondaryFocusTracks.length,
			compact,
			isVoiceCallAppFullscreen,
			membersRowVisible,
			hasScreenShare,
		}),
		[
			layoutMode,
			focusMainTrack,
			secondaryFocusTracks.length,
			compact,
			isVoiceCallAppFullscreen,
			membersRowVisible,
			hasScreenShare,
		],
	);
	const [presentationSnapshot, setPresentationSnapshot] = useState(() =>
		createVoiceCallLayoutPresentationSnapshot(presentationInput),
	);
	const currentPresentationSnapshot = useMemo(
		() =>
			transitionVoiceCallLayoutPresentationSnapshot(presentationSnapshot, {
				type: 'presentation.update',
				input: presentationInput,
			}),
		[presentationInput, presentationSnapshot],
	);
	useEffect(() => {
		setPresentationSnapshot((snapshot) =>
			transitionVoiceCallLayoutPresentationSnapshot(snapshot, {
				type: 'presentation.update',
				input: presentationInput,
			}),
		);
	}, [presentationInput]);
	const presentationModel = useMemo(
		() => selectVoiceCallLayoutPresentationModel(currentPresentationSnapshot),
		[currentPresentationSnapshot],
	);
	const mainLayout = presentationModel.mainLayout;
	const canShowParticipantsGridPanel = presentationModel.canShowParticipantsGridPanel;
	const visibleCameraTracks = filteredCameraTracks;
	const isParticipantsExpanded = presentationModel.isParticipantsExpanded;
	const isFullscreenFocusLayout = presentationModel.isFullscreenFocusLayout;
	const focusScrollerOverflow = presentationModel.focusScrollerOverflow;
	const showMembersRow = presentationModel.showMembersRow;
	const showMembersToggle = presentationModel.showMembersToggle;
	const shouldRenderFocusFallbackGrid = presentationModel.shouldRenderFocusFallbackGrid;
	const shouldWrapScreenShareGrid = presentationModel.shouldWrapScreenShareGrid;
	const handleToggleParticipantsGrid = useCallback(() => {
		setPresentationSnapshot((snapshot) => {
			const latestSnapshot = transitionVoiceCallLayoutPresentationSnapshot(snapshot, {
				type: 'presentation.update',
				input: presentationInput,
			});
			return transitionVoiceCallLayoutPresentationSnapshot(latestSnapshot, {type: 'participants.toggle'});
		});
	}, [presentationInput]);
	const participantsToggleLabel = isParticipantsExpanded
		? i18n._(HIDE_PARTICIPANTS_DESCRIPTOR)
		: i18n._(SHOW_PARTICIPANTS_DESCRIPTOR);
	const membersRowToggleLabel = membersRowVisible ? i18n._(HIDE_MEMBERS_DESCRIPTOR) : i18n._(SHOW_MEMBERS_DESCRIPTOR);
	const handleToggleMembersRow = useCallback(() => {
		VoiceCallLayout.toggleFocusMembersRowVisible();
	}, []);
	const membersRowNode = useMemo(() => {
		if (!showMembersRow) return null;
		return (
			<div
				className={styles.focusLayoutMembersRow}
				data-flx="voice.voice-call-layout-content.focus-layout-node.members-row"
			>
				<div
					className={styles.focusLayoutMembersRowInner}
					data-flx="voice.voice-call-layout-content.focus-layout-node.members-row-inner"
				>
					{secondaryFocusTracks.map((trackRef, index) => {
						const key =
							trackRef.source != null
								? `${trackRef.participant.identity}-${trackRef.source}`
								: `focus-members-row-placeholder-${trackRef.participant.identity}-${index}`;
						const isFocusedTrackMirror =
							focusMainTrack != null &&
							trackRef.participant.identity === focusMainTrack.participant.identity &&
							trackRef.source === focusMainTrack.source;
						return (
							<div
								key={key}
								className={styles.focusLayoutMembersRowTile}
								data-flx="voice.voice-call-layout-content.focus-layout-node.members-row-tile"
							>
								<TrackRefContext.Provider value={trackRef}>
									<ParticipantContext.Provider value={trackRef.participant as Participant}>
										<VoiceParticipantTile
											guildId={channel.guildId}
											channelId={channel.id}
											showFocusIndicator
											allowAutoSubscribe={!isFocusedTrackMirror}
											renderFocusedPlaceholder={isFocusedTrackMirror}
											presentation="focus-secondary"
											data-flx="voice.voice-call-layout-content.focus-layout-node.members-row-participant-tile"
										/>
									</ParticipantContext.Provider>
								</TrackRefContext.Provider>
							</div>
						);
					})}
				</div>
			</div>
		);
	}, [channel.guildId, channel.id, focusMainTrack, secondaryFocusTracks, showMembersRow]);
	const focusLayoutNode = useMemo(() => {
		if (shouldRenderFocusFallbackGrid) {
			return (
				<div
					className={clsx(styles.gridLayoutWrapper, compact && styles.gridLayoutWrapperCompact)}
					data-flx="voice.voice-call-layout-content.focus-layout-node.grid-layout-wrapper"
				>
					<VoiceGridLayout
						entries={visibleCameraTracks.map(trackToEntry)}
						compact={compact}
						onExpandUser={onExpandUser}
						data-flx="voice.voice-call-layout-content.focus-layout-node.voice-grid-layout"
					>
						<VoiceParticipantTile
							guildId={channel.guildId}
							channelId={channel.id}
							data-flx="voice.voice-call-layout-content.focus-layout-node.voice-participant-tile"
						/>
					</VoiceGridLayout>
				</div>
			);
		}
		return (
			<div
				className={clsx(
					styles.focusLayoutContent,
					!compact && styles.focusLayoutContentFullscreen,
					isFullscreenFocusLayout && styles.focusLayoutContentAppFullscreen,
					!compact && showMembersRow && styles.focusLayoutContentFullscreenMembersVisible,
					compact && styles.focusLayoutContentCompact,
					!isParticipantsExpanded && styles.focusLayoutContentNoParticipants,
					isParticipantsExpanded && styles.focusLayoutParticipantsExpanded,
				)}
				style={focusLayoutStyle}
				data-flx="voice.voice-call-layout-content.focus-layout-node.focus-layout"
			>
				<Scroller
					orientation="vertical"
					fade
					className={styles.focusLayoutScroller}
					contentClassName={styles.focusLayoutScrollerContent}
					overflow={focusScrollerOverflow}
					key="voice-call-focus-layout-scroller"
					data-flx="voice.voice-call-layout-content.focus-layout-node.focus-layout-scroller"
				>
					{isParticipantsExpanded ? (
						<div
							className={styles.focusExpandedScrollBody}
							data-flx="voice.voice-call-layout-content.focus-layout-node.focus-expanded-scroll-body"
						>
							{focusMainTrack && (
								<div
									className={styles.focusExpandedMainSection}
									data-flx="voice.voice-call-layout-content.focus-layout-node.focus-expanded-main-section"
								>
									<div
										className={styles.focusExpandedMainGridViewport}
										data-flx="voice.voice-call-layout-content.focus-layout-node.focus-expanded-main-grid-viewport"
									>
										<VoiceGridLayout
											entries={[focusMainTrack as TrackReferenceOrPlaceholder].map(trackToEntry)}
											onExpandUser={onExpandUser}
											data-flx="voice.voice-call-layout-content.focus-layout-node.voice-grid-layout--2"
										>
											<VoiceParticipantTile
												guildId={channel.guildId}
												channelId={channel.id}
												isPinned={
													(focusMainTrack as TrackReferenceOrPlaceholder).participant.identity ===
													pinnedParticipantIdentity
												}
												showFocusIndicator={false}
												presentation="focus-main"
												data-flx="voice.voice-call-layout-content.focus-layout-node.voice-participant-tile--2"
											/>
										</VoiceGridLayout>
									</div>
								</div>
							)}
							{canShowParticipantsGridPanel && (
								<>
									<div
										className={styles.focusExpandedToggleRow}
										data-flx="voice.voice-call-layout-content.focus-layout-node.focus-expanded-toggle-row"
									>
										<FocusRing
											offset={-2}
											className={styles.carouselToggleFocusRing}
											data-flx="voice.voice-call-layout-content.focus-layout-node.carousel-toggle-focus-ring"
										>
											<Tooltip
												text={participantsToggleLabel}
												data-flx="voice.voice-call-layout-content.focus-layout-node.tooltip"
											>
												<button
													type="button"
													className={styles.carouselToggle}
													onClick={handleToggleParticipantsGrid}
													aria-expanded={isParticipantsExpanded}
													aria-controls="voice-focus-participants-grid"
													aria-label={participantsToggleLabel}
													data-flx="voice.voice-call-layout-content.focus-layout-node.carousel-toggle.toggle-participants-grid.button"
												>
													{compact ? (
														<UsersIcon
															weight="bold"
															className={styles.iconMedium}
															data-flx="voice.voice-call-layout-content.focus-layout-node.icon-medium"
														/>
													) : isParticipantsExpanded ? (
														<CaretDownIcon
															weight="bold"
															className={styles.iconMedium}
															data-flx="voice.voice-call-layout-content.focus-layout-node.icon-medium--2"
														/>
													) : (
														<CaretUpIcon
															weight="bold"
															className={styles.iconMedium}
															data-flx="voice.voice-call-layout-content.focus-layout-node.icon-medium--3"
														/>
													)}
												</button>
											</Tooltip>
										</FocusRing>
									</div>
									<div
										id="voice-focus-participants-grid"
										className={styles.focusMiniGridSection}
										data-flx="voice.voice-call-layout-content.focus-layout-node.voice-focus-participants-grid"
									>
										<div
											className={styles.focusMiniGrid}
											data-flx="voice.voice-call-layout-content.focus-layout-node.focus-mini-grid"
										>
											{secondaryFocusTracks.map((trackRef, index) => {
												const key =
													trackRef.source != null
														? `${trackRef.participant.identity}-${trackRef.source}`
														: `focus-mini-placeholder-${trackRef.participant.identity}-${index}`;
												const isFocusedTrackMirror =
													focusMainTrack != null &&
													trackRef.participant.identity === focusMainTrack.participant.identity &&
													trackRef.source === focusMainTrack.source;
												return (
													<div
														key={key}
														className={styles.focusMiniGridTile}
														data-flx="voice.voice-call-layout-content.focus-layout-node.focus-mini-grid-tile"
													>
														<TrackRefContext.Provider value={trackRef}>
															<ParticipantContext.Provider value={trackRef.participant as Participant}>
																<VoiceParticipantTile
																	guildId={channel.guildId}
																	channelId={channel.id}
																	showFocusIndicator
																	allowAutoSubscribe={!isFocusedTrackMirror}
																	renderFocusedPlaceholder={isFocusedTrackMirror}
																	presentation="focus-secondary"
																	data-flx="voice.voice-call-layout-content.focus-layout-node.voice-participant-tile--3"
																/>
															</ParticipantContext.Provider>
														</TrackRefContext.Provider>
													</div>
												);
											})}
										</div>
									</div>
								</>
							)}
						</div>
					) : (
						<div
							className={styles.focusLayoutScrollBody}
							data-flx="voice.voice-call-layout-content.focus-layout-node.focus-layout-scroll-body"
						>
							<div
								className={styles.focusLayoutMainWrapper}
								data-flx="voice.voice-call-layout-content.focus-layout-node.focus-layout-main-wrapper"
							>
								{focusMainTrack && (
									<div
										className={styles.focusLayoutMain}
										style={focusMainStyle}
										data-flx="voice.voice-call-layout-content.focus-layout-node.focus-layout-main"
									>
										<TrackRefContext.Provider value={focusMainTrack as TrackReferenceOrPlaceholder}>
											<ParticipantContext.Provider
												value={(focusMainTrack as TrackReferenceOrPlaceholder).participant as Participant}
											>
												<VoiceParticipantTile
													guildId={channel.guildId}
													channelId={channel.id}
													isPinned={
														(focusMainTrack as TrackReferenceOrPlaceholder).participant.identity ===
														pinnedParticipantIdentity
													}
													showFocusIndicator={false}
													presentation="focus-main"
													data-flx="voice.voice-call-layout-content.focus-layout-node.voice-participant-tile--4"
												/>
											</ParticipantContext.Provider>
										</TrackRefContext.Provider>
									</div>
								)}
							</div>
							{canShowParticipantsGridPanel && (
								<div
									className={styles.carouselToggleWrap}
									data-flx="voice.voice-call-layout-content.focus-layout-node.carousel-toggle-wrap"
								>
									<FocusRing
										offset={-2}
										className={styles.carouselToggleFocusRing}
										data-flx="voice.voice-call-layout-content.focus-layout-node.carousel-toggle-focus-ring--2"
									>
										<Tooltip
											text={participantsToggleLabel}
											data-flx="voice.voice-call-layout-content.focus-layout-node.tooltip--2"
										>
											<button
												type="button"
												className={styles.carouselToggle}
												onClick={handleToggleParticipantsGrid}
												aria-expanded={isParticipantsExpanded}
												aria-controls="voice-focus-participants-grid"
												aria-label={participantsToggleLabel}
												data-flx="voice.voice-call-layout-content.focus-layout-node.carousel-toggle.toggle-participants-grid.button--2"
											>
												{compact ? (
													<UsersIcon
														weight="bold"
														className={styles.iconMedium}
														data-flx="voice.voice-call-layout-content.focus-layout-node.icon-medium--4"
													/>
												) : isParticipantsExpanded ? (
													<CaretDownIcon
														weight="bold"
														className={styles.iconMedium}
														data-flx="voice.voice-call-layout-content.focus-layout-node.icon-medium--5"
													/>
												) : (
													<CaretUpIcon
														weight="bold"
														className={styles.iconMedium}
														data-flx="voice.voice-call-layout-content.focus-layout-node.icon-medium--6"
													/>
												)}
											</button>
										</Tooltip>
									</FocusRing>
								</div>
							)}
						</div>
					)}
				</Scroller>
				{showMembersRow && !isParticipantsExpanded && membersRowNode}
				{showMembersToggle && (
					<div
						className={styles.focusMembersToggleWrap}
						data-flx="voice.voice-call-layout-content.focus-layout-node.members-toggle-wrap"
					>
						<FocusRing
							offset={-2}
							className={styles.focusMembersToggleFocusRing}
							data-flx="voice.voice-call-layout-content.focus-layout-node.members-toggle-focus-ring"
						>
							<Tooltip
								text={membersRowToggleLabel}
								data-flx="voice.voice-call-layout-content.focus-layout-node.members-toggle-tooltip"
							>
								<button
									type="button"
									className={styles.focusMembersToggle}
									onClick={handleToggleMembersRow}
									aria-expanded={membersRowVisible}
									aria-label={membersRowToggleLabel}
									data-flx="voice.voice-call-layout-content.focus-layout-node.members-toggle-button"
								>
									<UsersIcon
										weight="bold"
										className={styles.focusMembersToggleIcon}
										data-flx="voice.voice-call-layout-content.focus-layout-node.members-toggle-users-icon"
									/>
									{membersRowVisible ? (
										<CaretDownIcon
											weight="bold"
											className={styles.focusMembersToggleIcon}
											data-flx="voice.voice-call-layout-content.focus-layout-node.members-toggle-caret-down"
										/>
									) : (
										<CaretUpIcon
											weight="bold"
											className={styles.focusMembersToggleIcon}
											data-flx="voice.voice-call-layout-content.focus-layout-node.members-toggle-caret-up"
										/>
									)}
								</button>
							</Tooltip>
						</FocusRing>
					</div>
				)}
			</div>
		);
	}, [
		channel.guildId,
		channel.id,
		compact,
		focusMainStyle,
		focusLayoutStyle,
		focusMainTrack,
		canShowParticipantsGridPanel,
		focusScrollerOverflow,
		isFullscreenFocusLayout,
		isParticipantsExpanded,
		handleToggleParticipantsGrid,
		handleToggleMembersRow,
		membersRowNode,
		membersRowToggleLabel,
		membersRowVisible,
		participantsToggleLabel,
		onExpandUser,
		pinnedParticipantIdentity,
		secondaryFocusTracks,
		shouldRenderFocusFallbackGrid,
		showMembersToggle,
		showMembersRow,
		visibleCameraTracks,
	]);
	const gridLayoutNode = useMemo(() => {
		const gridLayoutBody = (
			<div
				className={clsx(styles.gridLayoutScrollBody, compact && styles.gridLayoutScrollBodyCompact)}
				data-flx="voice.voice-call-layout-content.grid-layout-node.grid-layout-scroll-body"
			>
				<div
					className={clsx(styles.gridLayoutWrapper, compact && styles.gridLayoutWrapperCompact)}
					data-flx="voice.voice-call-layout-content.grid-layout-node.grid-layout-wrapper"
				>
					{shouldWrapScreenShareGrid ? (
						<div
							className={styles.screenshareGridLayout}
							data-flx="voice.voice-call-layout-content.grid-layout-node.screenshare-grid-layout"
						>
							<VoiceGridLayout
								entries={gridEntries}
								compact={compact}
								onCapacityChange={onGridCapacityChange}
								onExpandUser={onExpandUser}
								data-flx="voice.voice-call-layout-content.grid-layout-node.voice-grid-layout"
							>
								<VoiceParticipantTile
									guildId={channel.guildId}
									channelId={channel.id}
									data-flx="voice.voice-call-layout-content.grid-layout-node.voice-participant-tile"
								/>
							</VoiceGridLayout>
						</div>
					) : (
						<VoiceGridLayout
							entries={gridEntries}
							compact={compact}
							onCapacityChange={onGridCapacityChange}
							onExpandUser={onExpandUser}
							data-flx="voice.voice-call-layout-content.grid-layout-node.voice-grid-layout--2"
						>
							<VoiceParticipantTile
								guildId={channel.guildId}
								channelId={channel.id}
								data-flx="voice.voice-call-layout-content.grid-layout-node.voice-participant-tile--2"
							/>
						</VoiceGridLayout>
					)}
				</div>
			</div>
		);
		return (
			<Scroller
				orientation="vertical"
				fade
				className={styles.gridLayoutScroller}
				contentClassName={styles.gridLayoutScrollerContent}
				overflow="hidden"
				data-flx="voice.voice-call-layout-content.grid-layout-node.grid-layout-scroller"
			>
				{gridLayoutBody}
			</Scroller>
		);
	}, [
		channel.guildId,
		channel.id,
		compact,
		gridEntries,
		onExpandUser,
		onGridCapacityChange,
		shouldWrapScreenShareGrid,
	]);
	const mainContentNode = useMemo(() => {
		switch (mainLayout) {
			case 'focus':
				return focusLayoutNode;
			default:
				return gridLayoutNode;
		}
	}, [focusLayoutNode, gridLayoutNode, mainLayout]);
	return <>{mainContentNode}</>;
});
