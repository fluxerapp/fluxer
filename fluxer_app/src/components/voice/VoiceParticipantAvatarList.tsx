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

import * as ContextMenuActionCreators from '@app/actions/ContextMenuActionCreators';
import {UserProfilePopout} from '@app/components/popouts/UserProfilePopout';
import {AvatarStack} from '@app/components/uikit/avatars/AvatarStack';
import {AvatarWithPresence} from '@app/components/uikit/avatars/AvatarWithPresence';
import {VoiceParticipantContextMenu} from '@app/components/uikit/context_menu/VoiceParticipantContextMenu';
import {Popout} from '@app/components/uikit/popout/Popout';
import styles from '@app/components/voice/VoiceParticipantAvatarList.module.css';
import {
	createVoiceParticipantSortSnapshot,
	sortVoiceParticipantItemsWithSnapshot,
} from '@app/components/voice/VoiceParticipantSortUtils';
import {isVoiceParticipantActuallySpeaking} from '@app/components/voice/VoiceParticipantSpeakingUtils';
import type {UserRecord} from '@app/records/UserRecord';
import UserStore from '@app/stores/UserStore';
import MediaEngineStore from '@app/stores/voice/MediaEngineFacade';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef} from 'react';

export interface VoiceParticipantAvatarEntry {
	user: UserRecord;
	userId: string;
	connectionId: string;
	speaking: boolean;
	hasCamera: boolean;
	hasScreenShare: boolean;
	isLocal: boolean;
	selfMute: boolean;
	selfDeaf: boolean;
}

export function mergeVoiceParticipantEntriesByUser(
	entries: ReadonlyArray<VoiceParticipantAvatarEntry>,
): Array<VoiceParticipantAvatarEntry> {
	const merged = new Map<string, VoiceParticipantAvatarEntry>();

	for (const entry of entries) {
		const existing = merged.get(entry.userId);
		if (!existing) {
			merged.set(entry.userId, {...entry});
			continue;
		}

		merged.set(entry.userId, {
			...existing,
			speaking: existing.speaking || entry.speaking,
			hasCamera: existing.hasCamera || entry.hasCamera,
			hasScreenShare: existing.hasScreenShare || entry.hasScreenShare,
			isLocal: existing.isLocal || entry.isLocal,
			selfMute: existing.selfMute || entry.selfMute,
			selfDeaf: existing.selfDeaf || entry.selfDeaf,
		});
	}

	return Array.from(merged.values());
}

export function useVoiceParticipantAvatarEntries({
	guildId = null,
	channelId = null,
}: {
	guildId?: string | null;
	channelId?: string | null;
} = {}): Array<VoiceParticipantAvatarEntry> {
	const participantSnapshots = MediaEngineStore.participants;
	const sortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());

	return useMemo(() => {
		const nextEntries: Array<VoiceParticipantAvatarEntry> = [];

		for (const snapshot of Object.values(participantSnapshots)) {
			if (!snapshot.userId || !snapshot.connectionId) continue;

			const user = UserStore.getUser(snapshot.userId);
			if (!user) continue;

			const voiceState = MediaEngineStore.getVoiceStateByConnectionId(snapshot.connectionId);
			const speaking = isVoiceParticipantActuallySpeaking({
				isSpeaking: snapshot.isSpeaking,
				voiceState,
				isMicrophoneEnabled: snapshot.isMicrophoneEnabled,
			});

			nextEntries.push({
				user,
				userId: snapshot.userId,
				connectionId: snapshot.connectionId,
				speaking,
				hasCamera: snapshot.isCameraEnabled,
				hasScreenShare: snapshot.isScreenShareEnabled,
				isLocal: snapshot.isLocal,
				selfMute: voiceState?.self_mute ?? false,
				selfDeaf: voiceState?.self_deaf ?? false,
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
	}, [channelId, guildId, participantSnapshots]);
}

interface VoiceParticipantPopoutRowProps {
	entry: VoiceParticipantAvatarEntry;
	guildId?: string | null;
	channelId?: string | null;
}

function VoiceParticipantPopoutRow({entry, guildId, channelId}: VoiceParticipantPopoutRowProps) {
	const displayName =
		NicknameUtils.getNickname(entry.user, guildId ?? undefined, channelId ?? undefined) || entry.user.username;
	const participantName = displayName ?? entry.user.username ?? 'User';

	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={entry.user}
					participantName={participantName}
					onClose={onClose}
					guildId={guildId ?? undefined}
					connectionId={entry.connectionId}
				/>
			));
		},
		[channelId, entry.connectionId, entry.user, guildId, participantName],
	);

	return (
		<Popout
			render={({popoutKey}) => (
				<UserProfilePopout
					key={`${entry.user.id}:${guildId ?? 'global'}:user`}
					popoutKey={popoutKey}
					user={entry.user}
					isWebhook={false}
					guildId={guildId ?? undefined}
				/>
			)}
			position="left-start"
		>
			<div className={styles.popoutRow} role="option" tabIndex={0} onContextMenu={handleContextMenu}>
				<div className={styles.popoutRowAvatar}>
					<AvatarWithPresence
						user={entry.user}
						size={24}
						speaking={entry.speaking}
						muted={entry.selfMute}
						deafened={entry.selfDeaf}
						guildId={guildId}
					/>
				</div>
				<span className={styles.popoutRowName}>{displayName}</span>
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
	}) {
		const mergedEntrySortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
		const mergedEntries = useMemo(
			() =>
				sortVoiceParticipantItemsWithSnapshot(mergeVoiceParticipantEntriesByUser(entries), {
					snapshot: mergedEntrySortSnapshotRef.current,
					getParticipantKey: (entry) => entry.userId,
					getUserId: (entry) => entry.userId,
					guildId,
					channelId,
					getTieBreaker: (entry) => entry.connectionId,
				}),
			[channelId, entries, guildId],
		);
		const users = useMemo(() => mergedEntries.map((entry) => entry.user), [mergedEntries]);
		const speakingUserIds = useMemo(
			() => new Set(mergedEntries.filter((entry) => entry.speaking).map((entry) => entry.userId)),
			[mergedEntries],
		);
		const remainingEntries = useMemo(() => mergedEntries.slice(maxVisible), [mergedEntries, maxVisible]);
		const remainingCount = remainingEntries.length;

		const handleUserContextMenu = useCallback(
			(event: React.MouseEvent<HTMLElement>, user: UserRecord) => {
				event.preventDefault();
				event.stopPropagation();
				const entry = mergedEntries.find((e) => e.userId === user.id);
				if (!entry) return;
				const displayName =
					NicknameUtils.getNickname(user, guildId ?? undefined, channelId ?? undefined) || user.username;
				const participantName = displayName ?? user.username ?? 'User';
				ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
					<VoiceParticipantContextMenu
						user={user}
						participantName={participantName}
						onClose={onClose}
						guildId={guildId ?? undefined}
						connectionId={entry.connectionId}
					/>
				));
			},
			[channelId, guildId, mergedEntries],
		);

		const renderAvatar = useCallback(
			(user: UserRecord, avatarSize: number) => {
				const entry = mergedEntries.find((e) => e.userId === user.id);
				return (
					<div
						className={clsx(styles.stackAvatar, speakingUserIds.has(user.id) && styles.stackAvatarSpeaking)}
						data-speaking={speakingUserIds.has(user.id)}
					>
						<AvatarWithPresence
							user={user}
							size={avatarSize}
							speaking={speakingUserIds.has(user.id)}
							muted={entry?.selfMute}
							deafened={entry?.selfDeaf}
							guildId={guildId}
						/>
					</div>
				);
			},
			[guildId, mergedEntries, speakingUserIds],
		);

		const remainingContent = useMemo(() => {
			if (remainingCount === 0) return null;
			return (
				<Popout
					render={() => (
						<div className={styles.popoutContainer}>
							<div className={styles.popoutList} role="listbox">
								{mergedEntries.map((entry) => (
									<VoiceParticipantPopoutRow
										key={`${entry.userId}_${entry.connectionId}`}
										entry={entry}
										guildId={guildId}
										channelId={channelId}
									/>
								))}
							</div>
						</div>
					)}
					position="top-start"
					hoverDelay={200}
					hoverCloseDelay={200}
				>
					<div className={styles.remainingCount}>+{remainingCount}</div>
				</Popout>
			);
		}, [channelId, guildId, mergedEntries, remainingCount]);

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
		const mergedEntrySortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
		const mergedEntries = useMemo(
			() =>
				sortVoiceParticipantItemsWithSnapshot(mergeVoiceParticipantEntriesByUser(entries), {
					snapshot: mergedEntrySortSnapshotRef.current,
					getParticipantKey: (entry) => entry.userId,
					getUserId: (entry) => entry.userId,
					guildId,
					channelId,
					getTieBreaker: (entry) => entry.connectionId,
				}),
			[channelId, entries, guildId],
		);
		const participantCount = mergedEntries.length;
		const avatarSize = useMemo(() => {
			if (participantCount >= 12) return 50;
			if (participantCount >= 8) return 56;
			if (participantCount >= 5) return 64;
			return 72;
		}, [participantCount]);

		const listStyle = useMemo(
			() =>
				({
					'--voice-participant-avatar-size': `${avatarSize}px`,
				}) as React.CSSProperties,
			[avatarSize],
		);

		return (
			<div className={clsx(styles.wrapContainer, className)} style={listStyle}>
				{mergedEntries.map((entry) => (
					<div key={entry.userId} className={styles.wrapAvatar}>
						<AvatarWithPresence
							user={entry.user}
							size={avatarSize}
							speaking={entry.speaking}
							muted={entry.selfMute}
							deafened={entry.selfDeaf}
							guildId={guildId}
						/>
					</div>
				))}
			</div>
		);
	},
);
