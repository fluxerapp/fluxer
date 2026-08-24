// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useFrozenUnreadOrder} from '@app/features/app/components/floating/UnreadChannelOrder';
import styles from '@app/features/app/components/floating/UnreadChannelsContent.module.css';
import {
	BULK_PREVIEW_CHANNEL_BATCH_SIZE,
	UNREAD_PREVIEW_MESSAGE_LIMIT,
} from '@app/features/app/components/floating/UnreadPreviewBudget';
import previewStyles from '@app/features/app/components/shared/MessagePreview.module.css';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {renderChannelStream} from '@app/features/channel/components/ChannelMessageStream';
import {ChannelNotificationSettingsDropdown} from '@app/features/channel/components/channel_header_components/ChannelNotificationSettingsDropdown';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import Guilds from '@app/features/guild/state/Guilds';
import {
	DIRECT_MESSAGES_DESCRIPTOR,
	JUMP_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InboxCommands from '@app/features/inbox/commands/InboxCommands';
import {ensureMembersForMessages} from '@app/features/messaging/commands/MessageCommands';
import {InboxMessageHeader} from '@app/features/messaging/components/popouts/InboxMessageHeader';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessages} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import {NearViewportSurfaceContext} from '@app/features/messaging/hooks/useNearViewport';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import {createChannelStream} from '@app/features/messaging/utils/MessageGroupingUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import LocalUserSpamOverride from '@app/features/moderation/state/LocalUserSpamOverride';
import UnreadChannels from '@app/features/notification/state/UnreadChannels';
import {http} from '@app/features/platform/transport/RestTransport';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings from '@app/features/user/state/UserSettings';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {compare as compareSnowflakes, extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {BellIcon, BellSlashIcon, CaretDownIcon, CheckIcon, SparkleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const MENTIONS_DESCRIPTOR = msg({
	message: '{channelHeading}, {mentionCount, plural, one {# mention} other {# mentions}}',
	comment:
		'Short label in the unread channels content. Preserve placeholders {channelHeading}, {mentionCount}; they are inserted by code.',
});
const EXPAND_DESCRIPTOR = msg({
	message: 'Expand',
	comment: 'Short label in the unread channels content.',
});
const COLLAPSE_DESCRIPTOR = msg({
	message: 'Collapse',
	comment: 'Short label in the unread channels content.',
});
const EXPAND_UNREAD_MESSAGES_FOR_DESCRIPTOR = msg({
	message: 'Expand unread messages for {channelHeading}',
	comment: 'Short label in the unread channels content. Preserve {channelHeading}; it is inserted by code.',
});
const COLLAPSE_UNREAD_MESSAGES_FOR_DESCRIPTOR = msg({
	message: 'Collapse unread messages for {channelHeading}',
	comment: 'Short label in the unread channels content. Preserve {channelHeading}; it is inserted by code.',
});
const VIEW_ALL_UNREAD_DESCRIPTOR = msg({
	message: 'View all unread',
	comment: 'Short label in the unread channels content.',
});
const UNKNOWN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Unknown community',
	comment: 'Short label in the unread channels content.',
});
const NO_UNREAD_MESSAGES_DESCRIPTOR = msg({
	message: 'No unread messages',
	comment: 'Short label in the unread channels content.',
});
const YOU_RE_ALL_CAUGHT_UP_DESCRIPTOR = msg({
	message: "You're all caught up.",
	comment: 'Body text in the unread channels content.',
});

export function getUnreadChannels(): Array<Channel> {
	const channelIds = ReadStates.getChannelIds();
	const channels: Array<Channel> = [];
	for (const channelId of channelIds) {
		const channel = Channels.getChannel(channelId);
		if (!channel) continue;
		const hasUnread = ReadStates.hasUnread(channel.id);
		const hasMentions = ReadStates.getMentionCount(channel.id) > 0;
		if (!hasUnread && !hasMentions) continue;
		if (
			!UserGuildSettings.shouldShowChannelInUnreadInbox(
				{
					id: channel.id,
					guildId: channel.guildId ?? undefined,
					parentId: channel.parentId ?? undefined,
					type: channel.type,
				},
				{hasUnread, hasMentions},
			)
		) {
			continue;
		}
		if (GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null})) continue;
		channels.push(channel);
	}
	return channels.sort((a, b) => {
		const aLast = ReadStates.lastMessageId(a.id) ?? a.lastMessageId ?? null;
		const bLast = ReadStates.lastMessageId(b.id) ?? b.lastMessageId ?? null;
		const aTimestamp = aLast ? extractTimestamp(aLast) : a.createdAt.getTime();
		const bTimestamp = bLast ? extractTimestamp(bLast) : b.createdAt.getTime();
		return bTimestamp - aTimestamp;
	});
}

interface ChannelPreviewData {
	channel: Channel;
	messages: Array<Message>;
	oldestUnreadMessageId: string | null;
}

const INITIAL_VISIBLE_CHANNELS = 10;
const LOAD_MORE_CHUNK = 10;

interface FrozenUnreadCounts {
	mentionCount: number;
	unreadCount: number;
}

interface FrozenUnreadChannels {
	order: Map<string, number>;
	counts: Map<string, FrozenUnreadCounts>;
}

function freezeUnreadChannels(): FrozenUnreadChannels {
	const order = new Map<string, number>();
	const counts = new Map<string, FrozenUnreadCounts>();
	for (const [index, channel] of getUnreadChannels().entries()) {
		order.set(channel.id, index);
		counts.set(channel.id, {
			mentionCount: ReadStates.getMentionCount(channel.id),
			unreadCount: ReadStates.getUnreadCount(channel.id),
		});
	}
	return {order, counts};
}

interface CacheEntry {
	cacheKey: string;
	data: ChannelPreviewData;
}

const previewCache = new Map<string, CacheEntry>();
const inFlightRequests = new Map<string, Promise<void>>();

interface BulkPreviewRequest {
	channel_id: string;
	limit: number;
	after?: string;
	around?: string;
}

interface BulkPreviewResponse {
	channels: Array<{
		channel_id: string;
		messages: Array<WireMessage>;
	}>;
}

interface PreviewFetchPlan {
	channel: Channel;
	cacheKey: string;
	oldestUnreadMessageId: string | null;
	request: BulkPreviewRequest;
}

function computeCacheKey(channelId: string): string {
	const oldestUnread =
		ReadStates.getOldestUnreadMessageId(channelId) ?? ReadStates.getVisualUnreadMessageId(channelId) ?? '';
	const ack = ReadStates.ackMessageId(channelId) ?? '';
	return `${oldestUnread}:${ack}`;
}

function buildEmptyPreview(channel: Channel): ChannelPreviewData {
	return {channel, messages: [], oldestUnreadMessageId: null};
}

function buildPreviewFetchPlan(channel: Channel, cacheKey: string): PreviewFetchPlan | null {
	if (GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null})) {
		previewCache.set(channel.id, {cacheKey, data: buildEmptyPreview(channel)});
		return null;
	}
	const oldestUnreadMessageId =
		ReadStates.getOldestUnreadMessageId(channel.id) ?? ReadStates.getVisualUnreadMessageId(channel.id);
	if (oldestUnreadMessageId) {
		return {
			channel,
			cacheKey,
			oldestUnreadMessageId,
			request: {
				channel_id: channel.id,
				limit: UNREAD_PREVIEW_MESSAGE_LIMIT * 2,
				around: oldestUnreadMessageId,
			},
		};
	}
	const ackMessageId = ReadStates.ackMessageId(channel.id);
	if (!ackMessageId) {
		previewCache.set(channel.id, {cacheKey, data: buildEmptyPreview(channel)});
		return null;
	}
	return {
		channel,
		cacheKey,
		oldestUnreadMessageId: null,
		request: {
			channel_id: channel.id,
			limit: UNREAD_PREVIEW_MESSAGE_LIMIT,
			after: ackMessageId,
		},
	};
}

function buildPreviewDataFromMessages(
	plan: PreviewFetchPlan,
	messages: ReadonlyArray<WireMessage>,
): ChannelPreviewData {
	const records = messages.map((message) => new Message(message));
	if (plan.oldestUnreadMessageId) {
		const ordered = [...records].reverse();
		const startIndex = ordered.findIndex((message) => message.id === plan.oldestUnreadMessageId);
		if (startIndex !== -1) {
			return {
				channel: plan.channel,
				messages: ordered.slice(startIndex, startIndex + UNREAD_PREVIEW_MESSAGE_LIMIT),
				oldestUnreadMessageId: plan.oldestUnreadMessageId,
			};
		}
	}
	const sorted = [...records].sort((a, b) => compareSnowflakes(a.id, b.id));
	const latest = sorted.slice(Math.max(0, sorted.length - UNREAD_PREVIEW_MESSAGE_LIMIT));
	return {
		channel: plan.channel,
		messages: latest,
		oldestUnreadMessageId: latest[0]?.id ?? null,
	};
}

async function fetchPreviewBatch(plans: Array<PreviewFetchPlan>): Promise<void> {
	if (plans.length === 0) return;
	const response = await http.post<BulkPreviewResponse>(Endpoints.CHANNEL_MESSAGES_BULK, {
		body: {requests: plans.map((plan) => plan.request)},
	});
	const responseChannels = new Map((response.body?.channels ?? []).map((entry) => [entry.channel_id, entry.messages]));
	const retryPlans: Array<PreviewFetchPlan> = [];
	for (const plan of plans) {
		const messages = responseChannels.get(plan.channel.id) ?? [];
		const isAnchored = plan.request.around != null || plan.request.after != null;
		if (messages.length === 0 && isAnchored) {
			retryPlans.push({
				channel: plan.channel,
				cacheKey: plan.cacheKey,
				oldestUnreadMessageId: null,
				request: {channel_id: plan.channel.id, limit: UNREAD_PREVIEW_MESSAGE_LIMIT},
			});
			continue;
		}
		previewCache.set(plan.channel.id, {
			cacheKey: plan.cacheKey,
			data: buildPreviewDataFromMessages(plan, messages),
		});
	}
	if (retryPlans.length === 0) return;
	const retryResponse = await http.post<BulkPreviewResponse>(Endpoints.CHANNEL_MESSAGES_BULK, {
		body: {requests: retryPlans.map((plan) => plan.request)},
	});
	const retryChannels = new Map(
		(retryResponse.body?.channels ?? []).map((entry) => [entry.channel_id, entry.messages]),
	);
	for (const plan of retryPlans) {
		previewCache.set(plan.channel.id, {
			cacheKey: plan.cacheKey,
			data: buildPreviewDataFromMessages(plan, retryChannels.get(plan.channel.id) ?? []),
		});
	}
}

function chunkPlans(plans: Array<PreviewFetchPlan>): Array<Array<PreviewFetchPlan>> {
	const chunks: Array<Array<PreviewFetchPlan>> = [];
	for (let index = 0; index < plans.length; index += BULK_PREVIEW_CHANNEL_BATCH_SIZE) {
		chunks.push(plans.slice(index, index + BULK_PREVIEW_CHANNEL_BATCH_SIZE));
	}
	return chunks;
}

async function prefetchChannelPreviews(channels: ReadonlyArray<Channel>): Promise<void> {
	const plans: Array<PreviewFetchPlan> = [];
	const waiting: Array<Promise<void>> = [];
	for (const channel of channels) {
		const cacheKey = computeCacheKey(channel.id);
		const cached = previewCache.get(channel.id);
		if (cached && cached.cacheKey === cacheKey) continue;
		const inFlight = inFlightRequests.get(channel.id);
		if (inFlight) {
			waiting.push(inFlight);
			continue;
		}
		const plan = buildPreviewFetchPlan(channel, cacheKey);
		if (plan) plans.push(plan);
	}
	const batches = chunkPlans(plans).map((batch) => {
		const promise = fetchPreviewBatch(batch).finally(() => {
			for (const plan of batch) {
				if (inFlightRequests.get(plan.channel.id) === promise) {
					inFlightRequests.delete(plan.channel.id);
				}
			}
		});
		for (const plan of batch) {
			inFlightRequests.set(plan.channel.id, promise);
		}
		return promise;
	});
	await Promise.all([...waiting, ...batches]);
}

function useBulkChannelPreviews(channels: ReadonlyArray<Channel>): Map<string, ChannelPreviewData | null> {
	const [previewVersion, setPreviewVersion] = useState(0);
	const previewSignature = useMemo(
		() => channels.map((channel) => `${channel.id}:${computeCacheKey(channel.id)}`).join('|'),
		[channels],
	);
	useEffect(() => {
		let active = true;
		void prefetchChannelPreviews(channels).finally(() => {
			if (active) setPreviewVersion((version) => version + 1);
		});
		return () => {
			active = false;
		};
	}, [channels, previewSignature]);
	return useMemo(() => {
		const previews = new Map<string, ChannelPreviewData | null>();
		for (const channel of channels) {
			const cacheKey = computeCacheKey(channel.id);
			const cached = previewCache.get(channel.id);
			previews.set(channel.id, cached && cached.cacheKey === cacheKey ? cached.data : null);
		}
		return previews;
	}, [channels, previewVersion]);
}

function getChannelHeadingText(channel: Channel): string {
	if (channel.isPrivate()) return ChannelUtils.getDMDisplayName(channel);
	return channel.name?.trim() || ChannelUtils.getName(channel);
}

const UnreadChannelCard = observer(function UnreadChannelCard({
	channel,
	headingId,
	previewData,
	mentionCount,
	unreadCount,
}: {
	channel: Channel;
	headingId: string;
	previewData: ChannelPreviewData | null;
	mentionCount: number;
	unreadCount: number;
}) {
	const {i18n} = useLingui();
	const messageDisplayCompact = UserSettings.getMessageDisplayCompact();
	const messageGroupSpacing = Accessibility.getMessageGroupSpacingValue(messageDisplayCompact);
	const isCollapsed = UnreadChannels.isCollapsed(channel.id);
	const isMuted = UserGuildSettings.isMutedAtAnyLevel(channel.guildId ?? null, channel.id);
	const previewMessages = previewData?.messages ?? [];
	const oldestUnreadMessageId = previewData?.oldestUnreadMessageId ?? null;
	const hasMoreUnreadThanShown =
		oldestUnreadMessageId != null && unreadCount > 0 && unreadCount > previewMessages.length;
	const spammerOverrideVersion = LocalUserSpamOverride.version;
	useEffect(() => {
		if (previewMessages.length === 0) return;
		void ensureMembersForMessages(previewMessages);
	}, [previewMessages]);
	const previewMessageState = useMemo(() => {
		const container = new ChannelMessages(channel.id);
		return previewMessages.length > 0 ? container.reset(previewMessages) : container;
	}, [channel.id, previewMessages]);
	const channelStream = useMemo(() => {
		if (!oldestUnreadMessageId || previewMessages.length === 0) return [];
		return createChannelStream({
			channel,
			messages: previewMessageState,
			oldestUnreadMessageId,
			treatSpam: true,
		});
	}, [channel, previewMessageState, previewMessages.length, oldestUnreadMessageId, spammerOverrideVersion]);
	const handleJumpToMessage = useCallback(
		(messageId: string) => {
			goToMessage(channel.id, messageId);
			InboxCommands.closeInboxAndFocusChannelTextarea(channel.id);
		},
		[channel.id],
	);
	const handleHeaderClick = useCallback(() => {
		if (oldestUnreadMessageId) {
			goToMessage(channel.id, oldestUnreadMessageId);
			InboxCommands.closeInboxAndFocusChannelTextarea(channel.id);
		}
	}, [channel.id, oldestUnreadMessageId]);
	const handleToggleCollapse = useCallback(() => {
		UnreadChannels.toggleCollapsed(channel.id);
	}, [channel.id]);
	const handleMarkAsRead = useCallback(() => {
		ReadStateCommands.ack(channel.id, true);
	}, [channel.id]);
	const handleOpenNotificationSettings = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => (
				<ChannelNotificationSettingsDropdown
					channel={channel}
					onClose={onClose}
					data-flx="app.floating.unread-channels-content.handle-open-notification-settings.channel-notification-settings-dropdown"
				/>
			));
		},
		[channel],
	);
	const renderMessageActions = useCallback(
		(message: Message) => (
			<FocusRing offset={-2} data-flx="app.floating.unread-channels-content.render-message-actions.focus-ring">
				<button
					type="button"
					className={clsx(previewStyles.actionButton, styles.jumpButton)}
					onClick={() => handleJumpToMessage(message.id)}
					data-flx="app.floating.unread-channels-content.render-message-actions.jump-button.jump-to-message"
				>
					{i18n._(JUMP_DESCRIPTOR)}
				</button>
			</FocusRing>
		),
		[handleJumpToMessage, i18n.locale],
	);
	const getMessageHeadingActivate = useCallback(
		(message: Message) => () => handleJumpToMessage(message.id),
		[handleJumpToMessage],
	);
	const streamMarkup = useMemo(() => {
		if (previewMessages.length === 0) return null;
		return renderChannelStream({
			channelStream,
			messages: previewMessageState,
			channel,
			highlightedMessageId: null,
			messageDisplayCompact,
			messageGroupSpacing,
			unblurredMessageId: null,
			onMessageEdit: undefined,
			onReveal: undefined,
			messageRowClassName: styles.messageRow,
			messageActionsClassName: styles.messageActions,
			renderMessageActions,
			suppressMessageActions: true,
			previewContext: MessagePreviewContext.LIST_POPOUT,
			dateDividerClassName: styles.previewDateDivider,
			suppressUnreadIndicator: true,
			getMessageHeadingActivate,
		});
	}, [
		channelStream,
		previewMessageState,
		channel,
		messageDisplayCompact,
		messageGroupSpacing,
		previewMessages.length,
		renderMessageActions,
		getMessageHeadingActivate,
		i18n.locale,
	]);
	const BellComponent = isMuted ? BellSlashIcon : BellIcon;
	const channelHeading = getChannelHeadingText(channel);
	return (
		<FocusRing offset={2} data-flx="app.floating.unread-channels-content.unread-channel-card.focus-ring">
			<section
				className={styles.channelCard}
				aria-labelledby={headingId}
				data-inbox-channel-section=""
				tabIndex={-1}
				data-flx="app.floating.unread-channels-content.unread-channel-card.channel-card"
			>
				<h2
					id={headingId}
					className={styles.srOnly}
					data-flx="app.floating.unread-channels-content.unread-channel-card.sr-only"
				>
					<button
						type="button"
						tabIndex={-1}
						className={styles.channelHeadingButton}
						onClick={handleHeaderClick}
						disabled={!oldestUnreadMessageId}
						data-flx="app.floating.unread-channels-content.unread-channel-card.channel-heading-button.header-click"
					>
						{mentionCount > 0 ? i18n._(MENTIONS_DESCRIPTOR, {channelHeading, mentionCount}) : channelHeading}
					</button>
				</h2>
				<InboxMessageHeader
					channel={channel}
					className={styles.stickyChannelHeader}
					onClick={handleHeaderClick}
					mentionCount={mentionCount}
					leftAdornment={
						<Tooltip
							text={isCollapsed ? i18n._(EXPAND_DESCRIPTOR) : i18n._(COLLAPSE_DESCRIPTOR)}
							position="top"
							data-flx="app.floating.unread-channels-content.unread-channel-card.tooltip"
						>
							<FocusRing offset={-2} data-flx="app.floating.unread-channels-content.unread-channel-card.focus-ring--2">
								<button
									type="button"
									className={clsx(styles.collapseButton, isCollapsed && styles.collapseButtonCollapsed)}
									onClick={handleToggleCollapse}
									aria-label={
										isCollapsed
											? i18n._(EXPAND_UNREAD_MESSAGES_FOR_DESCRIPTOR, {channelHeading})
											: i18n._(COLLAPSE_UNREAD_MESSAGES_FOR_DESCRIPTOR, {channelHeading})
									}
									aria-expanded={!isCollapsed}
									data-flx="app.floating.unread-channels-content.unread-channel-card.collapse-button.toggle-collapse"
								>
									<CaretDownIcon
										className={styles.collapseIcon}
										weight="bold"
										data-flx="app.floating.unread-channels-content.unread-channel-card.collapse-icon"
									/>
								</button>
							</FocusRing>
						</Tooltip>
					}
					rightActions={
						<>
							<Tooltip
								text={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
								position="top"
								data-flx="app.floating.unread-channels-content.unread-channel-card.tooltip--2"
							>
								<FocusRing
									offset={-2}
									data-flx="app.floating.unread-channels-content.unread-channel-card.focus-ring--3"
								>
									<button
										type="button"
										className={styles.headerIconButton}
										onClick={handleOpenNotificationSettings}
										aria-label={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
										data-flx="app.floating.unread-channels-content.unread-channel-card.header-icon-button.open-notification-settings"
									>
										<BellComponent
											className={styles.headerIcon}
											weight="fill"
											data-flx="app.floating.unread-channels-content.unread-channel-card.header-icon"
										/>
									</button>
								</FocusRing>
							</Tooltip>
							<Tooltip
								text={i18n._(MARK_AS_READ_DESCRIPTOR)}
								position="top"
								data-flx="app.floating.unread-channels-content.unread-channel-card.tooltip--3"
							>
								<FocusRing
									offset={-2}
									data-flx="app.floating.unread-channels-content.unread-channel-card.focus-ring--4"
								>
									<button
										type="button"
										className={styles.headerIconButton}
										onClick={handleMarkAsRead}
										aria-label={i18n._(MARK_AS_READ_DESCRIPTOR)}
										data-flx="app.floating.unread-channels-content.unread-channel-card.header-icon-button.mark-as-read"
									>
										<CheckIcon
											className={styles.headerIcon}
											weight="bold"
											data-flx="app.floating.unread-channels-content.unread-channel-card.header-icon--2"
										/>
									</button>
								</FocusRing>
							</Tooltip>
						</>
					}
					data-flx="app.floating.unread-channels-content.unread-channel-card.inbox-message-header.header-click"
				/>
				<AnimatePresence
					initial={false}
					data-flx="app.floating.unread-channels-content.unread-channel-card.animate-presence"
				>
					{!isCollapsed && (
						<motion.div
							key="preview"
							className={styles.collapseContainer}
							initial={{height: 0, opacity: 0}}
							animate={{height: 'auto', opacity: 1}}
							exit={{height: 0, opacity: 0}}
							transition={{
								duration: Accessibility.useReducedMotion ? 0 : 0.22,
								ease: [0.4, 0, 0.2, 1],
							}}
							data-flx="app.floating.unread-channels-content.unread-channel-card.collapse-container"
						>
							<div
								className={previewStyles.previewCard}
								data-flx="app.floating.unread-channels-content.unread-channel-card.div"
							>
								{previewData == null ? (
									<div
										className={styles.cardPlaceholder}
										data-flx="app.floating.unread-channels-content.unread-channel-card.card-placeholder"
									>
										<Spinner data-flx="app.floating.unread-channels-content.unread-channel-card.spinner" />
									</div>
								) : (
									<div
										className={styles.messageStream}
										data-flx="app.floating.unread-channels-content.unread-channel-card.message-stream"
									>
										{streamMarkup}
									</div>
								)}
								{hasMoreUnreadThanShown && (
									<div
										className={styles.previewFooter}
										data-flx="app.floating.unread-channels-content.unread-channel-card.preview-footer"
									>
										<FocusRing
											offset={-2}
											data-flx="app.floating.unread-channels-content.unread-channel-card.focus-ring--5"
										>
											<button
												type="button"
												className={styles.viewAllButton}
												onClick={handleHeaderClick}
												data-flx="app.floating.unread-channels-content.unread-channel-card.view-all-button.header-click"
											>
												{i18n._(VIEW_ALL_UNREAD_DESCRIPTOR)}
											</button>
										</FocusRing>
									</div>
								)}
							</div>
						</motion.div>
					)}
				</AnimatePresence>
			</section>
		</FocusRing>
	);
});
export const UnreadChannelsContent = observer(function UnreadChannelsContent() {
	const {i18n} = useLingui();
	const scrollerRef = useRef<ScrollerHandle | null>(null);
	const resolveInboxScrollSurface = useMemo(() => () => scrollerRef.current?.getViewportElement() ?? null, []);
	const readStateVersion = ReadStates.version;
	const settingsVersion = UserGuildSettings.version;
	const liveUnreadChannels = useMemo(() => getUnreadChannels(), [readStateVersion, settingsVersion]);
	const {snapshot: frozenUnread, channels: allUnreadChannels} = useFrozenUnreadOrder(
		freezeUnreadChannels,
		liveUnreadChannels,
	);
	const [loadedCount, setLoadedCount] = useState(INITIAL_VISIBLE_CHANNELS);
	const visibleChannels = useMemo(() => allUnreadChannels.slice(0, loadedCount), [allUnreadChannels, loadedCount]);
	const channelPreviews = useBulkChannelPreviews(visibleChannels);
	const selectionCopyMessages = useMemo(() => {
		const messages: Array<Message> = [];
		for (const preview of channelPreviews.values()) {
			if (preview) {
				messages.push(...preview.messages);
			}
		}
		return messages;
	}, [channelPreviews]);
	const onCopySelectedMessages = useMessageSelectionCopyForMessages<HTMLDivElement>(selectionCopyMessages);
	const groupedChannels = useMemo(() => {
		const groups: Array<{key: string; label: string; channels: Array<Channel>}> = [];
		const groupIndex = new Map<string, number>();
		const directMessagesLabel = i18n._(DIRECT_MESSAGES_DESCRIPTOR);
		for (const channel of visibleChannels) {
			const preview = channelPreviews.get(channel.id);
			if (preview !== undefined && preview !== null && preview.messages.length === 0) continue;
			const guildId = channel.guildId ?? null;
			const key = guildId ?? '__dm__';
			let idx = groupIndex.get(key);
			if (idx === undefined) {
				const label = guildId
					? (Guilds.getGuild(guildId)?.name ?? i18n._(UNKNOWN_COMMUNITY_DESCRIPTOR))
					: directMessagesLabel;
				idx = groups.length;
				groups.push({key, label, channels: []});
				groupIndex.set(key, idx);
			}
			groups[idx].channels.push(channel);
		}
		return groups;
	}, [visibleChannels, channelPreviews, i18n.locale]);
	const unreadRows = useMemo(() => {
		const rows: Array<{
			channel: Channel;
			endsGroup: boolean;
			groupLabel: string;
			key: string;
			mentionCount: number;
			unreadCount: number;
		}> = [];
		for (const group of groupedChannels) {
			for (const [index, channel] of group.channels.entries()) {
				const counts = frozenUnread.counts.get(channel.id);
				rows.push({
					channel,
					endsGroup: index === group.channels.length - 1,
					groupLabel: group.label,
					key: channel.id,
					mentionCount: counts?.mentionCount ?? 0,
					unreadCount: counts?.unreadCount ?? 0,
				});
			}
		}
		return rows;
	}, [groupedChannels, frozenUnread]);
	const hasMore = loadedCount < allUnreadChannels.length;
	const handleScroll = useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			if (!hasMore) return;
			const target = event.currentTarget;
			const scrollPercentage = (target.scrollTop + target.offsetHeight) / target.scrollHeight;
			if (scrollPercentage > 0.8) {
				setLoadedCount((current) =>
					current >= allUnreadChannels.length ? current : Math.min(current + LOAD_MORE_CHUNK, allUnreadChannels.length),
				);
			}
		},
		[hasMore, allUnreadChannels.length],
	);
	useMessageListKeyboardNavigation({
		containerRef: scrollerRef,
	});
	useEffect(() => {
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key !== 'd' && event.key !== 'D') return;
			if (event.ctrlKey || event.metaKey || event.altKey) return;
			const target = event.target as Element | null;
			if (target instanceof HTMLTextAreaElement || target instanceof HTMLInputElement) return;
			if (target instanceof HTMLElement && target.isContentEditable) return;
			const scrollerNode = scrollerRef.current?.getViewportElement() ?? null;
			if (!scrollerNode) return;
			if (target && !scrollerNode.contains(target) && document.activeElement !== document.body) return;
			const sections = Array.from(scrollerNode.querySelectorAll<HTMLElement>('[data-inbox-channel-section]'));
			if (sections.length === 0) return;
			const activeSection = (
				target instanceof HTMLElement ? target.closest('[data-inbox-channel-section]') : null
			) as HTMLElement | null;
			const currentIndex = activeSection ? sections.indexOf(activeSection) : -1;
			const direction = event.shiftKey ? -1 : 1;
			let nextIndex: number;
			if (currentIndex === -1) {
				nextIndex = direction === 1 ? 0 : sections.length - 1;
			} else {
				nextIndex = (currentIndex + direction + sections.length) % sections.length;
			}
			event.preventDefault();
			event.stopPropagation();
			const nextSection = sections[nextIndex];
			nextSection.focus({preventScroll: true});
			nextSection.scrollIntoView({block: 'start', behavior: Accessibility.useSmoothScrolling ? 'smooth' : 'auto'});
		};
		window.addEventListener('keydown', handleKeyDown, true);
		return () => window.removeEventListener('keydown', handleKeyDown, true);
	}, []);
	if (allUnreadChannels.length === 0) {
		return (
			<div className={previewStyles.emptyState} data-flx="app.floating.unread-channels-content.div">
				<div className={previewStyles.emptyStateContent} data-flx="app.floating.unread-channels-content.div--2">
					<SparkleIcon
						className={previewStyles.emptyStateIcon}
						data-flx="app.floating.unread-channels-content.sparkle-icon"
					/>
					<div className={previewStyles.emptyStateTextContainer} data-flx="app.floating.unread-channels-content.div--3">
						<h3 className={previewStyles.emptyStateTitle} data-flx="app.floating.unread-channels-content.h3">
							{i18n._(NO_UNREAD_MESSAGES_DESCRIPTOR)}
						</h3>
						<p className={previewStyles.emptyStateDescription} data-flx="app.floating.unread-channels-content.p">
							{i18n._(YOU_RE_ALL_CAUGHT_UP_DESCRIPTOR)}
						</p>
					</div>
				</div>
			</div>
		);
	}
	return (
		<NearViewportSurfaceContext.Provider value={resolveInboxScrollSurface}>
			<Scroller
				key="unread-channels-scroller"
				className={styles.scroller}
				ref={scrollerRef}
				onScroll={handleScroll}
				onCopy={onCopySelectedMessages}
				data-message-selection-root="true"
				data-flx="app.floating.unread-channels-content.scroller"
			>
				<div className={styles.list} role="list" data-flx="app.floating.unread-channels-content.list">
					{unreadRows.map((row, rowIndex) => (
						<div
							key={row.key}
							className={clsx(styles.row, row.endsGroup && styles.rowGroupEnd)}
							role="listitem"
							aria-posinset={rowIndex + 1}
							aria-setsize={unreadRows.length}
							data-flx="app.floating.unread-channels-content.row"
						>
							<section
								className={styles.guildGroup}
								aria-label={row.groupLabel}
								data-flx="app.floating.unread-channels-content.guild-group"
							>
								<UnreadChannelCard
									channel={row.channel}
									headingId={`inbox-unread-channel-${row.channel.id}`}
									previewData={channelPreviews.get(row.channel.id) ?? null}
									mentionCount={row.mentionCount}
									unreadCount={row.unreadCount}
									data-flx="app.floating.unread-channels-content.unread-channel-card"
								/>
							</section>
						</div>
					))}
				</div>
			</Scroller>
		</NearViewportSurfaceContext.Provider>
	);
});
