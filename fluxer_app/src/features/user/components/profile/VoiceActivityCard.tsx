// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import type {Guild} from '@app/features/guild/models/Guild';
import {WATCH_STREAM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Permission from '@app/features/permissions/state/Permission';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import {Button} from '@app/features/ui/button/Button';
import {Avatar} from '@app/features/ui/components/Avatar';
import {LiveBadge} from '@app/features/ui/components/LiveBadge';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/profile/VoiceActivityCard.module.css';
import {
	VOICE_ACTIVITY_AVATAR_MAX_VISIBLE,
	VOICE_ACTIVITY_AVATAR_SIZE_PX,
} from '@app/features/user/components/profile/VoiceActivityCardMetrics';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as VoiceStreamWatchCommands from '@app/features/voice/commands/VoiceStreamWatchCommands';
import {useStreamPreview} from '@app/features/voice/components/useStreamPreview';
import {useStreamWatchState} from '@app/features/voice/components/useStreamWatchState';
import {
	createVoiceParticipantSortSnapshot,
	sortVoiceParticipantItemsWithSnapshot,
} from '@app/features/voice/components/VoiceParticipantSortUtils';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {usePendingVoiceConnection} from '@app/features/voice/hooks/usePendingVoiceConnection';
import type {UserVoiceActivity} from '@app/features/voice/hooks/useUserVoiceActivities';
import {useVoiceJoinEligibility} from '@app/features/voice/hooks/useVoiceJoinEligibility';
import {canViewStreamPreview} from '@app/features/voice/utils/StreamPreviewPermissionUtils';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {ME} from '@fluxer/constants/src/AppConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretRightIcon, PhoneIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef} from 'react';

const PARTICIPANTS_DESCRIPTOR = msg({
	message: 'Participants',
	comment: 'Short label in the user settings voice activity card. Keep it concise.',
});
const PARTICIPANTS_2_DESCRIPTOR = msg({
	message: 'Participants: {names}',
	comment:
		'Short label in the user settings voice activity card. Keep it concise. Preserve {names}; it is inserted by code.',
});
const YOU_DON_T_HAVE_PERMISSION_TO_JOIN_THIS_DESCRIPTOR = msg({
	message: "You can't join this voice channel",
	comment: 'Label in the user settings voice activity card. Keep the tone plain and specific.',
});
const OPEN_VOICE_DESCRIPTOR = msg({
	message: 'Open voice',
	comment: 'Button or menu action label in the user settings voice activity card. Keep it concise.',
});
const JOIN_VOICE_DESCRIPTOR = msg({
	message: 'Join voice',
	comment: 'Button or menu action label in the user settings voice activity card. Keep it concise.',
});
const WATCHING_DESCRIPTOR = msg({
	message: 'Watching',
	comment: 'Short label in the user settings voice activity card. Keep it concise.',
});
const OPEN_DESCRIPTOR = msg({
	message: 'Open {contextName}',
	comment:
		'Button or menu action label in the user settings voice activity card. Keep it concise. Preserve {contextName}; it is inserted by code.',
});
const OPEN_2_DESCRIPTOR = msg({
	message: 'Open {displayName}',
	comment:
		'Button or menu action label in the user settings voice activity card. Keep it concise. Preserve {displayName}; it is inserted by code.',
});
const OPEN_3_DESCRIPTOR = msg({
	message: 'Open @{displayName}',
	comment:
		'Button or menu action label in the user settings voice activity card. Keep it concise. Preserve {displayName}; it is inserted by code.',
});
const STREAM_PREVIEW_DESCRIPTOR = msg({
	message: 'Stream preview',
	comment: 'Short label in the user settings voice activity card. Keep it concise.',
});
const NO_PREVIEW_YET_DESCRIPTOR = msg({
	message: 'No preview yet',
	comment: 'Empty-state text in the user settings voice activity card.',
});

interface VoiceActivityCardProps {
	activity: UserVoiceActivity;
	onNavigate?: () => void;
}

export const VoiceActivityCard: React.FC<VoiceActivityCardProps> = observer(({activity, onNavigate}) => {
	const {i18n} = useLingui();
	const {voiceState, connectionId, guildId, channelId, channel, guild, isStreaming, streamKey, participantUsers} =
		activity;
	const isConnectedToChannel = useMemo(() => {
		if (!channelId) return false;
		return MediaEngine.channelId === channelId && MediaEngine.guildId === (guildId ?? null);
	}, [channelId, guildId]);
	const {canJoin} = useVoiceJoinEligibility({
		guildId: guildId ?? null,
		channelId,
	});
	const canFetchStreamPreview = canViewStreamPreview({
		guildId,
		channelId,
		hasConnectPermission: () =>
			Permission.can(Permissions.CONNECT, {guildId: guildId ?? undefined, channelId: channelId ?? undefined}),
	});
	const {previewUrl, isPreviewLoading} = useStreamPreview(
		isStreaming && !!streamKey && canFetchStreamPreview,
		streamKey ?? '',
	);
	const streamWatchArgs = useMemo(
		() => ({
			streamKey: streamKey ?? '',
			guildId: guildId ?? null,
			channelId,
		}),
		[streamKey, guildId, channelId],
	);
	const {isWatching, isPendingJoin, canWatch, startWatching} = useStreamWatchState(streamWatchArgs);
	const handleVoiceConnected = useCallback(() => {
		NavigationCommands.selectChannel(guildId ?? ME, channelId);
		onNavigate?.();
	}, [guildId, channelId, onNavigate]);
	const {isPending: isJoining, startConnection: startJoinConnection} = usePendingVoiceConnection({
		guildId,
		channelId,
		onConnected: handleVoiceConnected,
	});
	const handleWatchConnected = useCallback(() => {
		const streamingUser = Users.getUser(voiceState.user_id);
		if (streamingUser) {
			VoiceStreamWatchCommands.applyStreamWatchFocus({
				participantIdentity: buildVoiceParticipantIdentity(streamingUser.id, connectionId),
				guildId: guildId ?? null,
				channelId: channelId ?? null,
			});
		}
		onNavigate?.();
	}, [voiceState.user_id, connectionId, guildId, channelId, onNavigate]);
	const {isPending: isWatchingStarting, markPending: markWatchPending} = usePendingVoiceConnection({
		guildId,
		channelId,
		onConnected: handleWatchConnected,
	});
	const handleWatchStream = useCallback(
		(event: React.SyntheticEvent) => {
			event.stopPropagation();
			const streamingUser = Users.getUser(voiceState.user_id);
			if (!streamingUser) return;
			VoiceStreamWatchCommands.openAndWatchStream(
				{
					streamKey: streamKey ?? '',
					guildId: guildId ?? null,
					channelId: channelId ?? null,
					userId: streamingUser.id,
					connectionId,
				},
				{startWatching, markPending: markWatchPending},
			);
		},
		[startWatching, voiceState.user_id, connectionId, guildId, channelId, streamKey, markWatchPending],
	);
	const handleJoinOrOpenVoice = useCallback(() => {
		if (isConnectedToChannel) {
			NavigationCommands.selectChannel(guildId ?? ME, channelId);
			onNavigate?.();
		} else if (canJoin && channelId) {
			startJoinConnection({skipConfirm: true});
		}
	}, [isConnectedToChannel, canJoin, guildId, channelId, onNavigate, startJoinConnection]);
	const avatarSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	const avatarStackUsers = useMemo(() => {
		const ownerUser = Users.getUser(voiceState.user_id);
		const userMap = new Map<string, User>();
		if (ownerUser) {
			userMap.set(ownerUser.id, ownerUser);
		}
		for (const user of participantUsers) {
			userMap.set(user.id, user);
		}
		return sortVoiceParticipantItemsWithSnapshot(Array.from(userMap.values()), {
			snapshot: avatarSortSnapshotRef.current,
			getParticipantKey: (user) => user.id,
			getUserId: (user) => user.id,
			guildId,
			channelId,
		});
	}, [voiceState.user_id, participantUsers, guildId, channelId]);
	const participantLabel = useMemo(() => {
		if (avatarStackUsers.length === 0) {
			return i18n._(PARTICIPANTS_DESCRIPTOR);
		}
		const names = avatarStackUsers
			.map((user) => NicknameUtils.getNickname(user, guildId ?? null, channelId ?? undefined))
			.join(', ');
		return i18n._(PARTICIPANTS_2_DESCRIPTOR, {names});
	}, [avatarStackUsers, guildId, channelId, i18n.locale]);
	const cannotJoinReason = useMemo(() => {
		if (isConnectedToChannel || canJoin) return null;
		return i18n._(YOU_DON_T_HAVE_PERMISSION_TO_JOIN_THIS_DESCRIPTOR);
	}, [isConnectedToChannel, canJoin, i18n.locale]);
	const buttonLabel = useMemo(() => {
		if (isConnectedToChannel) return i18n._(OPEN_VOICE_DESCRIPTOR);
		return i18n._(JOIN_VOICE_DESCRIPTOR);
	}, [isConnectedToChannel, i18n.locale]);
	return (
		<div className={styles.card} data-flx="user.profile.voice-activity-card.card">
			<div className={styles.headerContextGroup} data-flx="user.profile.voice-activity-card.header-context-group">
				<div className={styles.headerRow} data-flx="user.profile.voice-activity-card.header-row">
					<div className={styles.headerLeft} data-flx="user.profile.voice-activity-card.header-left">
						{isStreaming ? (
							<>
								<span
									className={clsx(styles.activityLabel, styles.streamingLabel)}
									data-flx="user.profile.voice-activity-card.activity-label"
								>
									<Trans>Streaming</Trans>
								</span>
								<LiveBadge showTooltip={false} data-flx="user.profile.voice-activity-card.live-badge" />
							</>
						) : (
							<span className={styles.activityLabel} data-flx="user.profile.voice-activity-card.activity-label--2">
								<Trans>In voice</Trans>
							</span>
						)}
					</div>
				</div>
				<VoiceActivityContext
					channel={channel}
					guild={guild}
					guildId={guildId}
					voiceState={voiceState}
					data-flx="user.profile.voice-activity-card.voice-activity-context"
				/>
			</div>
			{avatarStackUsers.length > 0 && (
				<div
					className={styles.participantsRow}
					role="group"
					aria-label={participantLabel}
					data-flx="user.profile.voice-activity-card.participants-row"
				>
					<AvatarStack
						size={VOICE_ACTIVITY_AVATAR_SIZE_PX}
						maxVisible={VOICE_ACTIVITY_AVATAR_MAX_VISIBLE}
						users={avatarStackUsers}
						guildId={guildId}
						channelId={channelId}
						data-flx="user.profile.voice-activity-card.avatar-stack"
					/>
				</div>
			)}
			{isStreaming && streamKey && (
				<StreamPreviewSection
					previewUrl={previewUrl}
					isPreviewLoading={isPreviewLoading}
					isWatching={isWatching}
					isPendingJoin={isPendingJoin}
					canWatch={canWatch}
					onWatch={handleWatchStream}
					watchLabel={isWatching ? i18n._(WATCHING_DESCRIPTOR) : i18n._(WATCH_STREAM_DESCRIPTOR)}
					isSubmitting={isWatchingStarting}
					data-flx="user.profile.voice-activity-card.stream-preview-section"
				/>
			)}
			<div className={styles.actionRow} data-flx="user.profile.voice-activity-card.action-row">
				{cannotJoinReason ? (
					<Tooltip text={cannotJoinReason} maxWidth="xl" data-flx="user.profile.voice-activity-card.tooltip">
						<div style={{width: '100%'}} data-flx="user.profile.voice-activity-card.div">
							<Button
								compact
								fitContainer
								disabled={true}
								leftIcon={
									<PhoneIcon
										weight="fill"
										className={styles.actionIcon}
										data-flx="user.profile.voice-activity-card.action-icon"
									/>
								}
								className={styles.actionButton}
								data-flx="user.profile.voice-activity-card.action-button"
							>
								{buttonLabel}
							</Button>
						</div>
					</Tooltip>
				) : (
					<Button
						compact
						fitContainer
						onClick={handleJoinOrOpenVoice}
						leftIcon={
							<PhoneIcon
								weight="fill"
								className={styles.actionIcon}
								data-flx="user.profile.voice-activity-card.action-icon--2"
							/>
						}
						className={styles.actionButton}
						submitting={isJoining}
						data-flx="user.profile.voice-activity-card.action-button.join-or-open-voice"
					>
						{buttonLabel}
					</Button>
				)}
			</div>
		</div>
	);
});

interface VoiceActivityContextProps {
	channel: Channel | undefined;
	guild: Guild | undefined;
	guildId: string | null;
	voiceState: VoiceState;
}

const VoiceActivityContext: React.FC<VoiceActivityContextProps> = observer(({channel, guild, guildId, voiceState}) => {
	const {i18n} = useLingui();
	const channelId = channel?.id;
	const handleGuildNavigate = useCallback(() => {
		if (!guildId || !channelId) return;
		NavigationCommands.selectChannel(guildId, channelId);
	}, [channelId, guildId]);
	const handleDMNavigate = useCallback(() => {
		if (!channelId) return;
		NavigationCommands.selectChannel(ME, channelId);
	}, [channelId]);
	if (!channel || !channelId) return null;
	if (guild && guildId) {
		const guildName = guild.name ?? '';
		const channelName = channel.name ?? '';
		const contextName = guildName && channelName ? `${guildName} · ${channelName}` : (guildName || channelName).trim();
		return (
			<button
				type="button"
				className={styles.contextButton}
				onClick={handleGuildNavigate}
				aria-label={i18n._(OPEN_DESCRIPTOR, {contextName})}
				data-flx="user.profile.voice-activity-card.voice-activity-context.context-button.guild-navigate"
			>
				<Tooltip text={guild.name ?? ''} data-flx="user.profile.voice-activity-card.voice-activity-context.tooltip">
					<div data-flx="user.profile.voice-activity-card.voice-activity-context.div">
						<GuildIcon
							id={guild.id}
							name={guild.name}
							icon={guild.icon}
							className={styles.contextGuildIcon}
							sizePx={16}
							data-flx="user.profile.voice-activity-card.voice-activity-context.context-guild-icon"
						/>
					</div>
				</Tooltip>
				<CaretRightIcon
					weight="bold"
					className={styles.contextChevron}
					data-flx="user.profile.voice-activity-card.voice-activity-context.context-chevron"
				/>
				<SpeakerHighIcon
					weight="fill"
					className={styles.contextIcon}
					data-flx="user.profile.voice-activity-card.voice-activity-context.context-icon"
				/>
				<span
					className={styles.contextChannelName}
					data-flx="user.profile.voice-activity-card.voice-activity-context.context-channel-name"
				>
					{channel.name}
				</span>
			</button>
		);
	}
	if (channel.isGroupDM()) {
		const displayName = ChannelUtils.getDMDisplayName(channel);
		return (
			<button
				type="button"
				className={styles.contextButton}
				onClick={handleDMNavigate}
				aria-label={i18n._(OPEN_2_DESCRIPTOR, {displayName})}
				data-flx="user.profile.voice-activity-card.voice-activity-context.context-button.dm-navigate"
			>
				<Tooltip text={displayName} data-flx="user.profile.voice-activity-card.voice-activity-context.tooltip--2">
					<div
						className={styles.contextDmAvatar}
						data-flx="user.profile.voice-activity-card.voice-activity-context.context-dm-avatar"
					>
						<GroupDMAvatar
							channel={channel}
							size={16}
							data-flx="user.profile.voice-activity-card.voice-activity-context.group-dm-avatar"
						/>
					</div>
				</Tooltip>
				<span
					className={styles.contextChannelName}
					data-flx="user.profile.voice-activity-card.voice-activity-context.context-channel-name--2"
				>
					{displayName}
				</span>
			</button>
		);
	}
	if (channel.isDM()) {
		const recipientId = channel.recipientIds.find((id) => id !== voiceState.user_id);
		const recipientUser = recipientId ? Users.getUser(recipientId) : undefined;
		if (recipientUser) {
			const displayName = NicknameUtils.getNickname(recipientUser, null);
			return (
				<button
					type="button"
					className={styles.contextButton}
					onClick={handleDMNavigate}
					aria-label={i18n._(OPEN_3_DESCRIPTOR, {displayName})}
					data-flx="user.profile.voice-activity-card.voice-activity-context.context-button.dm-navigate--2"
				>
					<Tooltip text={displayName} data-flx="user.profile.voice-activity-card.voice-activity-context.tooltip--3">
						<div
							className={styles.contextDmAvatar}
							data-flx="user.profile.voice-activity-card.voice-activity-context.context-dm-avatar--2"
						>
							<Avatar
								user={recipientUser}
								size={16}
								data-flx="user.profile.voice-activity-card.voice-activity-context.avatar"
							/>
						</div>
					</Tooltip>
					<span
						className={styles.contextChannelName}
						data-flx="user.profile.voice-activity-card.voice-activity-context.context-channel-name--3"
					>
						@{displayName}
					</span>
				</button>
			);
		}
	}
	return null;
});

interface StreamPreviewSectionProps {
	previewUrl: string | null;
	isPreviewLoading: boolean;
	isWatching: boolean;
	isPendingJoin: boolean;
	canWatch: boolean;
	onWatch: (event: React.SyntheticEvent) => void;
	watchLabel: string;
	isSubmitting?: boolean;
}

const StreamPreviewSection: React.FC<StreamPreviewSectionProps> = observer(
	({previewUrl, isPreviewLoading, isWatching, isPendingJoin, canWatch, onWatch, watchLabel, isSubmitting}) => {
		const {i18n} = useLingui();
		const isDisabled = !canWatch || isPendingJoin || isWatching || isSubmitting;
		const handleClick = useCallback(
			(event: React.MouseEvent) => {
				if (isDisabled) return;
				onWatch(event);
			},
			[isDisabled, onWatch],
		);
		return (
			<div
				className={clsx(styles.previewContainer, isSubmitting && styles.previewSubmitting)}
				onClick={handleClick}
				role="button"
				tabIndex={isDisabled ? -1 : 0}
				aria-label={watchLabel}
				aria-disabled={isDisabled}
				onKeyDown={(e) => {
					if (isKeyboardActivationKey(e.key) && !isDisabled) {
						e.preventDefault();
						onWatch(e);
					}
				}}
				data-flx="user.profile.voice-activity-card.stream-preview-section.preview-container.click"
			>
				{previewUrl ? (
					<img
						src={previewUrl}
						alt={i18n._(STREAM_PREVIEW_DESCRIPTOR)}
						className={styles.previewImage}
						data-flx="user.profile.voice-activity-card.stream-preview-section.preview-image"
					/>
				) : (
					<div
						className={styles.previewFallback}
						data-flx="user.profile.voice-activity-card.stream-preview-section.preview-fallback"
					>
						{isPreviewLoading ? (
							<Spinner size="small" data-flx="user.profile.voice-activity-card.stream-preview-section.spinner" />
						) : (
							i18n._(NO_PREVIEW_YET_DESCRIPTOR)
						)}
					</div>
				)}
				{!isDisabled && (
					<div
						className={styles.previewHoverOverlay}
						data-flx="user.profile.voice-activity-card.stream-preview-section.preview-hover-overlay"
					>
						<span
							className={styles.previewHoverText}
							data-flx="user.profile.voice-activity-card.stream-preview-section.preview-hover-text"
						>
							{watchLabel}
						</span>
					</div>
				)}
			</div>
		);
	},
);
