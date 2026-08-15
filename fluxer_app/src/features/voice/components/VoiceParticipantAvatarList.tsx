// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {VoiceParticipantContextMenu} from '@app/features/ui/action_menu/VoiceParticipantContextMenu';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import {AvatarWithPresence} from '@app/features/ui/avatars/AvatarWithPresence';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import {UserProfilePopout} from '@app/features/user/components/popouts/UserProfilePopout';
import {useUserProfileHoverPreload} from '@app/features/user/hooks/useUserProfileHoverPreload';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import styles from '@app/features/voice/components/VoiceParticipantAvatarList.module.css';
import {resolveVoiceParticipantAvatarEntryVoiceState} from '@app/features/voice/components/VoiceParticipantDisplayState';
import {
	createVoiceParticipantSortSnapshot,
	sortVoiceParticipantItemsWithSnapshot,
} from '@app/features/voice/components/VoiceParticipantSortUtils';
import MediaEngine, {useMediaEngineVersion, useVoiceEngineV2Model} from '@app/features/voice/engine/MediaEngineFacade';
import {selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {isParticipantVoicePermissionMuted} from '@app/features/voice/utils/VoicePermissionUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useRef, useState} from 'react';

const VIEW_PROFILE_FOR_DESCRIPTOR = msg({
	message: 'View profile for {displayName}',
	comment:
		"Aria label on an avatar tile in the voice participants list. {displayName} is the participant's display name.",
});
const VOICE_PARTICIPANTS_DESCRIPTOR = msg({
	message: 'Voice participants',
	comment: 'Aria label on the voice participants avatar list container.',
});
const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback voice participant display name when the user record has no usable name.',
});
const DEFAULT_WRAPPED_AVATAR_GAP_PX = 12;
const WRAPPED_AVATAR_SLOT_TRANSITION = {
	width: {duration: 0.22, ease: [0.22, 1, 0.36, 1]},
	flexBasis: {duration: 0.22, ease: [0.22, 1, 0.36, 1]},
} as const;
const WRAPPED_AVATAR_POP_TRANSITION = {
	opacity: {duration: 0.12, ease: 'easeOut'},
	scale: {type: 'spring', stiffness: 520, damping: 34, mass: 0.6},
} as const;
const WRAPPED_AVATAR_GAP_EPSILON = 0.25;

type WrappedAvatarSlotPhase = 'entering' | 'present' | 'exiting';

interface WrappedAvatarSlot {
	key: string;
	entry: VoiceParticipantAvatarEntry;
	phase: WrappedAvatarSlotPhase;
}

interface WrappedAvatarSlotState {
	slots: Array<WrappedAvatarSlot>;
	onSlotAnimationComplete: (key: string, phase: WrappedAvatarSlotPhase) => void;
}

export interface VoiceParticipantAvatarEntry {
	user: User;
	userId: string;
	connectionId: string;
	speaking: boolean;
	hasCamera: boolean;
	hasScreenShare: boolean;
	isLocal: boolean;
	selfMute: boolean;
	selfDeaf: boolean;
}

function getWrappedAvatarEntryKey(entry: VoiceParticipantAvatarEntry): string {
	return `${entry.userId}:${entry.connectionId}`;
}

function createPresentWrappedAvatarSlots(
	entries: ReadonlyArray<VoiceParticipantAvatarEntry>,
): Array<WrappedAvatarSlot> {
	return entries.map((entry) => ({
		key: getWrappedAvatarEntryKey(entry),
		entry,
		phase: 'present',
	}));
}

function reconcileWrappedAvatarSlots(
	previousSlots: ReadonlyArray<WrappedAvatarSlot>,
	targetEntries: ReadonlyArray<VoiceParticipantAvatarEntry>,
): Array<WrappedAvatarSlot> {
	const previousSlotsByKey = new Map(previousSlots.map((slot, index) => [slot.key, {slot, index}]));
	const targetKeys = new Set(targetEntries.map(getWrappedAvatarEntryKey));
	const nextSlots: Array<WrappedAvatarSlot> = targetEntries.map((entry) => {
		const key = getWrappedAvatarEntryKey(entry);
		const previousSlot = previousSlotsByKey.get(key)?.slot;
		return {
			key,
			entry,
			phase: previousSlot?.phase === 'exiting' ? 'entering' : previousSlot ? 'present' : 'entering',
		} satisfies WrappedAvatarSlot;
	});
	const exitingSlots = previousSlots
		.filter((slot) => !targetKeys.has(slot.key))
		.map(
			(slot) =>
				({
					...slot,
					phase: 'exiting',
				}) satisfies WrappedAvatarSlot,
		)
		.sort((first, second) => {
			const firstIndex = previousSlotsByKey.get(first.key)?.index ?? 0;
			const secondIndex = previousSlotsByKey.get(second.key)?.index ?? 0;
			return firstIndex - secondIndex;
		});
	for (const slot of exitingSlots) {
		const previousIndex = previousSlotsByKey.get(slot.key)?.index ?? nextSlots.length;
		nextSlots.splice(Math.min(previousIndex, nextSlots.length), 0, slot);
	}
	return nextSlots;
}

function parseLayoutPixelValue(value: string): number | null {
	const parsed = Number.parseFloat(value);
	if (!Number.isFinite(parsed)) return null;
	return parsed;
}

function useResolvedWrappedAvatarGapPx(containerRef: React.RefObject<HTMLElement | null>): number {
	const [gapPx, setGapPx] = useState(DEFAULT_WRAPPED_AVATAR_GAP_PX);
	const measureGap = useCallback(() => {
		const container = containerRef.current;
		if (!container) return;
		const ownerWindow = container.ownerDocument.defaultView ?? window;
		const computedStyle = ownerWindow.getComputedStyle(container);
		const measuredGap = parseLayoutPixelValue(computedStyle.rowGap);
		if (measuredGap == null) return;
		setGapPx((previousGap) =>
			Math.abs(previousGap - measuredGap) < WRAPPED_AVATAR_GAP_EPSILON ? previousGap : measuredGap,
		);
	}, [containerRef]);
	useLayoutEffect(() => {
		measureGap();
		const container = containerRef.current;
		if (!container) return;
		const ownerWindow = container.ownerDocument.defaultView ?? window;
		if (typeof ownerWindow.ResizeObserver === 'undefined') {
			ownerWindow.addEventListener('resize', measureGap);
			return () => ownerWindow.removeEventListener('resize', measureGap);
		}
		const resizeObserver = new ownerWindow.ResizeObserver(measureGap);
		resizeObserver.observe(container);
		ownerWindow.addEventListener('resize', measureGap);
		return () => {
			resizeObserver.disconnect();
			ownerWindow.removeEventListener('resize', measureGap);
		};
	}, [containerRef, measureGap]);
	return gapPx;
}

function useWrappedAvatarSlots(
	entries: ReadonlyArray<VoiceParticipantAvatarEntry>,
	shouldAnimate: boolean,
): WrappedAvatarSlotState {
	const targetSlots = useMemo(() => createPresentWrappedAvatarSlots(entries), [entries]);
	const [animatedSlots, setAnimatedSlots] = useState<Array<WrappedAvatarSlot>>(() => targetSlots);
	const onSlotAnimationComplete = useCallback((key: string, phase: WrappedAvatarSlotPhase) => {
		if (phase === 'present') return;
		setAnimatedSlots((previousSlots) => {
			const currentSlot = previousSlots.find((slot) => slot.key === key);
			if (!currentSlot || currentSlot.phase !== phase) return previousSlots;
			if (phase === 'exiting') return previousSlots.filter((slot) => slot.key !== key);
			return previousSlots.map((slot) => (slot.key === key ? {...slot, phase: 'present'} : slot));
		});
	}, []);
	useLayoutEffect(() => {
		if (!shouldAnimate) {
			setAnimatedSlots(targetSlots);
			return;
		}
		setAnimatedSlots((previousSlots) => reconcileWrappedAvatarSlots(previousSlots, entries));
	}, [entries, shouldAnimate, targetSlots]);
	return {
		slots: shouldAnimate ? animatedSlots : targetSlots,
		onSlotAnimationComplete,
	};
}

export function useVoiceParticipantAvatarEntries({
	guildId = null,
	channelId = null,
}: {
	guildId?: string | null;
	channelId?: string | null;
} = {}): Array<VoiceParticipantAvatarEntry> {
	useMediaEngineVersion();
	const voiceEngineV2Model = useVoiceEngineV2Model();
	const participantSnapshots = MediaEngine.participants;
	const connectionVoiceStates = MediaEngine.connectionVoiceStates;
	const localEffectiveSelfMute = selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload(voiceEngineV2Model);
	const localSelfDeaf = LocalVoiceState.getSelfDeaf();
	const sortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	return useMemo(() => {
		const nextEntries: Array<VoiceParticipantAvatarEntry> = [];
		for (const snapshot of Object.values(participantSnapshots)) {
			if (!snapshot.userId || !snapshot.connectionId) continue;
			const user = Users.getUser(snapshot.userId);
			if (!user) continue;
			const voiceState = connectionVoiceStates[snapshot.connectionId] ?? null;
			const permissionMuted = isParticipantVoicePermissionMuted({
				voiceState,
				guildId,
				channelId,
				isCurrentUser: snapshot.isLocal,
			});
			const entryVoiceState = resolveVoiceParticipantAvatarEntryVoiceState({
				snapshot,
				voiceState,
				permissionMuted,
				localEffectiveSelfMute,
				localSelfDeaf,
			});
			nextEntries.push({
				user,
				userId: snapshot.userId,
				connectionId: snapshot.connectionId,
				speaking: entryVoiceState.speaking,
				hasCamera: snapshot.isCameraEnabled,
				hasScreenShare: snapshot.isScreenShareEnabled,
				isLocal: snapshot.isLocal,
				selfMute: entryVoiceState.selfMute,
				selfDeaf: entryVoiceState.selfDeaf,
			});
		}
		return sortVoiceParticipantItemsWithSnapshot(nextEntries, {
			snapshot: sortSnapshotRef.current,
			getParticipantKey: (entry) => `${entry.userId}:${entry.connectionId}`,
			getUserId: (entry) => entry.userId,
			guildId,
			channelId,
			getTieBreaker: (entry) => entry.connectionId,
		});
	}, [channelId, connectionVoiceStates, guildId, localEffectiveSelfMute, localSelfDeaf, participantSnapshots]);
}

interface VoiceParticipantPopoutRowProps {
	entry: VoiceParticipantAvatarEntry;
	guildId?: string | null;
	channelId?: string | null;
}

function VoiceParticipantPopoutRow({entry, guildId, channelId}: VoiceParticipantPopoutRowProps) {
	const {i18n} = useLingui();
	const displayName = NicknameUtils.getNickname(entry.user, guildId ?? undefined, channelId ?? undefined);
	const participantName = displayName || i18n._(UNKNOWN_USER_DESCRIPTOR);
	const {scheduleProfilePreload, cancelProfilePreload} = useUserProfileHoverPreload({
		userId: entry.user.id,
		guildId: guildId ?? undefined,
	});
	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={entry.user}
					participantName={participantName}
					onClose={onClose}
					guildId={guildId ?? undefined}
					connectionId={entry.connectionId}
					data-flx="voice.voice-participant-avatar-list.handle-context-menu.voice-participant-context-menu"
				/>
			));
		},
		[channelId, entry.connectionId, entry.user, guildId, participantName],
	);
	return (
		<Popout
			render={({popoutKey, onClose}) => (
				<UserProfilePopout
					key={`${entry.user.id}:${guildId ?? 'global'}:user`}
					popoutKey={popoutKey}
					user={entry.user}
					isWebhook={false}
					guildId={guildId ?? undefined}
					onClose={onClose}
					data-flx="voice.voice-participant-avatar-list.voice-participant-popout-row.user-profile-popout"
				/>
			)}
			position="left-start"
			animationType="profile-slide"
			constrainHeight={false}
			freezePosition
			keepOpenOnTargetUnmount
			data-flx="voice.voice-participant-avatar-list.voice-participant-popout-row.popout"
		>
			<div
				className={styles.popoutRow}
				role="button"
				tabIndex={0}
				onMouseEnter={scheduleProfilePreload}
				onMouseLeave={cancelProfilePreload}
				onContextMenu={handleContextMenu}
				aria-label={i18n._(VIEW_PROFILE_FOR_DESCRIPTOR, {displayName})}
				data-flx="voice.voice-participant-avatar-list.voice-participant-popout-row.popout-row.context-menu"
			>
				<div
					className={styles.popoutRowAvatar}
					data-flx="voice.voice-participant-avatar-list.voice-participant-popout-row.popout-row-avatar"
				>
					<AvatarWithPresence
						user={entry.user}
						size={24}
						speaking={entry.speaking}
						muted={entry.selfMute}
						deafened={entry.selfDeaf}
						guildId={guildId}
						data-flx="voice.voice-participant-avatar-list.voice-participant-popout-row.avatar-with-presence"
					/>
				</div>
				<span
					className={styles.popoutRowName}
					data-flx="voice.voice-participant-avatar-list.voice-participant-popout-row.popout-row-name"
				>
					{displayName}
				</span>
			</div>
		</Popout>
	);
}

interface VoiceParticipantSpeakingAvatarStackProps {
	entries: ReadonlyArray<VoiceParticipantAvatarEntry>;
	guildId?: string | null;
	channelId?: string | null;
	size?: number;
	maxVisible?: number;
	className?: string;
	enableProfileModal?: boolean;
	showTooltips?: boolean;
	deduplicateUsers?: boolean;
}

function deduplicateEntriesByUser(
	entries: ReadonlyArray<VoiceParticipantAvatarEntry>,
): Array<VoiceParticipantAvatarEntry> {
	const seen = new Map<string, VoiceParticipantAvatarEntry>();
	for (const entry of entries) {
		const existing = seen.get(entry.userId);
		if (!existing) {
			seen.set(entry.userId, entry);
		} else if (entry.speaking && !existing.speaking) {
			seen.set(entry.userId, entry);
		}
	}
	return Array.from(seen.values());
}

export const VoiceParticipantSpeakingAvatarStack: React.FC<VoiceParticipantSpeakingAvatarStackProps> = observer(
	function VoiceParticipantSpeakingAvatarStack({
		entries,
		guildId,
		channelId,
		size = 24,
		maxVisible = 5,
		className,
		enableProfileModal = true,
		showTooltips = true,
		deduplicateUsers = false,
	}) {
		const {i18n} = useLingui();
		const entrySortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
		const rawSortedEntries = useMemo(
			() =>
				sortVoiceParticipantItemsWithSnapshot(entries.slice(), {
					snapshot: entrySortSnapshotRef.current,
					getParticipantKey: (entry) => `${entry.userId}:${entry.connectionId}`,
					getUserId: (entry) => entry.userId,
					guildId,
					channelId,
					getTieBreaker: (entry) => entry.connectionId,
				}),
			[channelId, entries, guildId],
		);
		const sortedEntries = useMemo(
			() => (deduplicateUsers ? deduplicateEntriesByUser(rawSortedEntries) : rawSortedEntries),
			[deduplicateUsers, rawSortedEntries],
		);
		const users = useMemo(() => sortedEntries.map((entry) => entry.user), [sortedEntries]);
		const remainingCount = Math.max(0, sortedEntries.length - maxVisible);
		const handleUserContextMenu = useCallback(
			(event: React.MouseEvent<HTMLElement>, user: User, index: number) => {
				event.preventDefault();
				event.stopPropagation();
				const entry = sortedEntries[index];
				if (!entry) return;
				const displayName = NicknameUtils.getNickname(user, guildId ?? undefined, channelId ?? undefined);
				const participantName = displayName || i18n._(UNKNOWN_USER_DESCRIPTOR);
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<VoiceParticipantContextMenu
						user={user}
						participantName={participantName}
						onClose={onClose}
						guildId={guildId ?? undefined}
						connectionId={entry.connectionId}
						data-flx="voice.voice-participant-avatar-list.handle-user-context-menu.voice-participant-context-menu"
					/>
				));
			},
			[channelId, guildId, i18n, sortedEntries],
		);
		const renderAvatar = useCallback(
			(user: User, avatarSize: number, index: number) => {
				const entry = sortedEntries[index];
				const speaking = entry?.speaking ?? false;
				return (
					<div
						className={clsx(styles.stackAvatar, speaking && styles.stackAvatarSpeaking)}
						data-speaking={speaking}
						data-flx="voice.voice-participant-avatar-list.render-avatar.stack-avatar"
					>
						<AvatarWithPresence
							user={user}
							size={avatarSize}
							speaking={speaking}
							muted={entry?.selfMute}
							deafened={entry?.selfDeaf}
							guildId={guildId}
							data-flx="voice.voice-participant-avatar-list.render-avatar.avatar-with-presence"
						/>
					</div>
				);
			},
			[guildId, sortedEntries],
		);
		const remainingContent = useMemo(() => {
			if (remainingCount === 0) return null;
			return (
				<Popout
					render={() => (
						<div
							className={styles.popoutContainer}
							data-flx="voice.voice-participant-avatar-list.remaining-content.popout-container"
						>
							<div
								className={styles.popoutList}
								role="group"
								aria-label={i18n._(VOICE_PARTICIPANTS_DESCRIPTOR)}
								data-flx="voice.voice-participant-avatar-list.remaining-content.popout-list"
							>
								{sortedEntries.map((entry) => (
									<VoiceParticipantPopoutRow
										key={`${entry.userId}_${entry.connectionId}`}
										entry={entry}
										guildId={guildId}
										channelId={channelId}
										data-flx="voice.voice-participant-avatar-list.remaining-content.voice-participant-popout-row"
									/>
								))}
							</div>
						</div>
					)}
					position="top-start"
					hoverDelay={200}
					hoverCloseDelay={200}
					data-flx="voice.voice-participant-avatar-list.remaining-content.popout"
				>
					<div
						className={styles.remainingCount}
						data-flx="voice.voice-participant-avatar-list.remaining-content.remaining-count"
					>
						+{remainingCount}
					</div>
				</Popout>
			);
		}, [channelId, guildId, sortedEntries, remainingCount, i18n.locale]);
		return (
			<AvatarStack
				users={users}
				size={size}
				maxVisible={maxVisible}
				overlap={0}
				className={className}
				guildId={guildId ?? undefined}
				channelId={channelId ?? undefined}
				renderAvatar={renderAvatar}
				enableProfileModal={enableProfileModal}
				showTooltips={showTooltips}
				remainingContent={remainingContent}
				onUserContextMenu={handleUserContextMenu}
				data-flx="voice.voice-participant-avatar-list.voice-participant-speaking-avatar-stack.avatar-stack"
			/>
		);
	},
);

interface VoiceParticipantWrappedAvatarListProps {
	entries: ReadonlyArray<VoiceParticipantAvatarEntry>;
	guildId?: string | null;
	channelId?: string | null;
	className?: string;
}

export const VoiceParticipantWrappedAvatarList: React.FC<VoiceParticipantWrappedAvatarListProps> = observer(
	function VoiceParticipantWrappedAvatarList({entries, guildId, channelId, className}) {
		const {i18n} = useLingui();
		const containerRef = useRef<HTMLDivElement | null>(null);
		const entrySortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
		const sortedEntries = useMemo(
			() =>
				sortVoiceParticipantItemsWithSnapshot(entries.slice(), {
					snapshot: entrySortSnapshotRef.current,
					getParticipantKey: (entry) => `${entry.userId}:${entry.connectionId}`,
					getUserId: (entry) => entry.userId,
					guildId,
					channelId,
					getTieBreaker: (entry) => entry.connectionId,
				}),
			[channelId, entries, guildId],
		);
		const participantCount = sortedEntries.length;
		const avatarSize = useMemo(() => {
			if (participantCount >= 12) return 50;
			if (participantCount >= 8) return 56;
			if (participantCount >= 5) return 64;
			return 72;
		}, [participantCount]);
		const listStyle = useMemo(
			() =>
				({
					'--voice-participant-avatar-size': remFromPx(avatarSize),
				}) as React.CSSProperties,
			[avatarSize],
		);
		const shouldAnimateAvatarChanges = !Accessibility.useReducedMotion;
		const {slots: animatedSlots, onSlotAnimationComplete} = useWrappedAvatarSlots(
			sortedEntries,
			shouldAnimateAvatarChanges,
		);
		const gapPx = useResolvedWrappedAvatarGapPx(containerRef);
		const slotSize = avatarSize * getAppZoomFactor() + gapPx;
		const handleContextMenu = useCallback(
			(event: React.MouseEvent<HTMLElement>, entry: VoiceParticipantAvatarEntry) => {
				event.preventDefault();
				event.stopPropagation();
				const displayName = NicknameUtils.getNickname(entry.user, guildId ?? undefined, channelId ?? undefined);
				const participantName = displayName || i18n._(UNKNOWN_USER_DESCRIPTOR);
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<VoiceParticipantContextMenu
						user={entry.user}
						participantName={participantName}
						onClose={onClose}
						guildId={guildId ?? undefined}
						connectionId={entry.connectionId}
						data-flx="voice.voice-participant-avatar-list.voice-participant-wrapped-avatar-list.handle-context-menu.voice-participant-context-menu"
					/>
				));
			},
			[channelId, guildId, i18n],
		);
		const renderAvatar = useCallback(
			(entry: VoiceParticipantAvatarEntry) => (
				<AvatarWithPresence
					user={entry.user}
					size={avatarSize}
					speaking={entry.speaking}
					muted={entry.selfMute}
					deafened={entry.selfDeaf}
					guildId={guildId}
					data-flx="voice.voice-participant-avatar-list.voice-participant-wrapped-avatar-list.avatar-with-presence"
				/>
			),
			[avatarSize, guildId],
		);
		const finalAvatarNodes = sortedEntries.map((entry) => (
			// biome-ignore lint/a11y/noStaticElementInteractions: wrapped voice avatars expose a pointer-only context menu.
			<div
				key={`${entry.userId}:${entry.connectionId}`}
				className={styles.wrapAvatarSlot}
				onContextMenu={(event) => handleContextMenu(event, entry)}
				data-flx="voice.voice-participant-avatar-list.voice-participant-wrapped-avatar-list.wrap-avatar-slot"
			>
				<div
					className={styles.wrapAvatar}
					data-flx="voice.voice-participant-avatar-list.voice-participant-wrapped-avatar-list.wrap-avatar"
				>
					{renderAvatar(entry)}
				</div>
			</div>
		));
		return (
			<div
				ref={containerRef}
				className={clsx(styles.wrapContainer, className)}
				style={listStyle}
				data-flx="voice.voice-participant-avatar-list.voice-participant-wrapped-avatar-list.wrap-container"
			>
				{shouldAnimateAvatarChanges
					? animatedSlots.map((slot) => {
							const isCollapsed = slot.phase === 'exiting';
							return (
								<motion.div
									key={slot.key}
									className={clsx(styles.wrapAvatarSlot, styles.wrapAvatarSlotAnimated)}
									onContextMenu={(event) => handleContextMenu(event, slot.entry)}
									initial={slot.phase === 'entering' ? {width: 0, flexBasis: 0} : false}
									animate={{
										width: isCollapsed ? 0 : slotSize,
										flexBasis: isCollapsed ? 0 : slotSize,
									}}
									transition={WRAPPED_AVATAR_SLOT_TRANSITION}
									onAnimationComplete={() => onSlotAnimationComplete(slot.key, slot.phase)}
									data-flx="voice.voice-participant-avatar-list.voice-participant-wrapped-avatar-list.wrap-avatar-slot.motion"
								>
									<motion.div
										className={clsx(styles.wrapAvatar, styles.wrapAvatarAnimated)}
										initial={slot.phase === 'entering' ? {opacity: 0, scale: 0.92} : false}
										animate={{
											opacity: isCollapsed ? 0 : 1,
											scale: isCollapsed ? 0.9 : 1,
										}}
										transition={WRAPPED_AVATAR_POP_TRANSITION}
										data-flx="voice.voice-participant-avatar-list.voice-participant-wrapped-avatar-list.wrap-avatar.motion"
									>
										{renderAvatar(slot.entry)}
									</motion.div>
								</motion.div>
							);
						})
					: finalAvatarNodes}
			</div>
		);
	},
);
