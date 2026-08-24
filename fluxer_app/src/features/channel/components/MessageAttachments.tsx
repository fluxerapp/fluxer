// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import {SpoilerOverlay} from '@app/features/app/components/shared/SpoilerOverlay';
import {Attachment} from '@app/features/channel/components/embeds/attachments/Attachment';
import {AttachmentMosaic} from '@app/features/channel/components/embeds/attachments/AttachmentMosaic';
import {Embed} from '@app/features/channel/components/embeds/ChannelEmbed';
import {GiftEmbed} from '@app/features/channel/components/GiftEmbed';
import {InviteEmbed} from '@app/features/channel/components/InviteEmbed';
import {getAttachmentRenderingState} from '@app/features/channel/components/MessageAttachmentStateUtils';
import styles from '@app/features/channel/components/MessageAttachments.module.css';
import {MessageReactions} from '@app/features/channel/components/MessageReactions';
import {useMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {ThemeEmbed} from '@app/features/channel/components/ThemeEmbed';
import {TimestampWithTooltip} from '@app/features/channel/components/TimestampWithTooltip';
import type {Channel} from '@app/features/channel/models/Channel';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import * as GiftCodeUtils from '@app/features/gift/utils/GiftCodeUtils';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {useMatureMedia} from '@app/features/messaging/hooks/useMatureMedia';
import {useMessageReactions as useMessageReactionsSnapshot} from '@app/features/messaging/hooks/useMessageReactionStore';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {extractEmbeddableCodeLinkContent} from '@app/features/messaging/utils/EmbeddableCodeLinkContent';
import {useForwardedMessageContext} from '@app/features/messaging/utils/ForwardedMessageUtils';
import {buildMessageSnapshotCopyText} from '@app/features/messaging/utils/MessageCopyTextUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import {canonicalizeMediaUrl, useSpoilerState} from '@app/features/messaging/utils/SpoilerUtils';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import matureStyles from '@app/features/theme/styles/MatureBlur.module.css';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import * as ThemeUtils from '@app/features/theme/utils/ThemeUtils';
import {StickerInlineMenuItems} from '@app/features/ui/action_menu/items/StickerContextMenuItems';
import {MessageContextMenu} from '@app/features/ui/action_menu/MessageContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import UserSettings from '@app/features/user/state/UserSettings';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {
	MessageAttachment,
	MessageSnapshot,
	MessageStickerItem,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowBendUpRightIcon, CaretRightIcon, HashIcon, NotePencilIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

interface SpoileredCodeLinkMatch {
	code: string;
	matchedText: string;
}

type SpoileredCodeLinkMap = ReadonlyMap<string, ReadonlyArray<string>>;

const EMPTY_SPOILER_KEYS: ReadonlyArray<string> = Object.freeze([]);

function getCodeLinkSpoilerSyncKeys(match: SpoileredCodeLinkMatch, keyPrefix: string): Array<string> {
	const canonicalUrl = canonicalizeMediaUrl(match.matchedText) ?? canonicalizeMediaUrl(`https://${match.matchedText}`);
	return canonicalUrl ? [`${keyPrefix}:${match.code}`, canonicalUrl] : [`${keyPrefix}:${match.code}`];
}

function buildSpoileredCodeLinkMap(
	matches: ReadonlyArray<SpoileredCodeLinkMatch>,
	keyPrefix: string,
): SpoileredCodeLinkMap {
	const linksByCode = new Map<string, Array<string>>();
	for (const match of matches) {
		const existing = linksByCode.get(match.code) ?? [];
		for (const key of getCodeLinkSpoilerSyncKeys(match, keyPrefix)) {
			if (!existing.includes(key)) {
				existing.push(key);
			}
		}
		linksByCode.set(match.code, existing);
	}
	return linksByCode;
}

interface SpoileredUrlEmbedProps {
	channelId: string;
	spoilerKeys?: ReadonlyArray<string>;
	children: React.ReactElement;
}

const SpoileredUrlEmbed = observer(function SpoileredUrlEmbed({
	channelId,
	spoilerKeys,
	children,
}: SpoileredUrlEmbedProps) {
	const syncKeys = spoilerKeys ?? EMPTY_SPOILER_KEYS;
	const isSpoiler = syncKeys.length > 0;
	const {hidden, reveal} = useSpoilerState(isSpoiler, channelId, syncKeys);
	if (!isSpoiler) {
		return children;
	}
	return (
		<SpoilerOverlay
			hidden={hidden}
			onReveal={reveal}
			className={styles.urlEmbedSpoiler}
			data-flx="channel.message-attachments.spoilered-url-embed"
		>
			{children}
		</SpoilerOverlay>
	);
});
const ForwardedFromSource = observer(({message}: {message: Message}) => {
	const {sourceChannel, sourceGuild, sourceUser, hasAccessToSource, displayName} = useForwardedMessageContext(message);
	const handleJumpToOriginal = useCallback(() => {
		if (message.messageReference && sourceChannel) {
			goToMessage(message.messageReference.channel_id, message.messageReference.message_id, {
				returnToMessageId: message.id,
				returnChannelId: message.channelId,
			});
		}
	}, [message.id, message.messageReference, sourceChannel]);
	const renderChannelIcon = useCallback(() => {
		if (!sourceChannel) return null;
		const iconSize = 16;
		if (sourceChannel.type === ChannelTypes.DM_PERSONAL_NOTES) {
			return (
				<NotePencilIcon
					className={styles.forwardedSourceIcon}
					weight="fill"
					size={iconSize}
					data-flx="channel.message-attachments.render-channel-icon.forwarded-source-icon"
				/>
			);
		}
		if (sourceChannel.type === ChannelTypes.DM && sourceUser) {
			return (
				<div
					className={styles.forwardedSourceAvatar}
					data-flx="channel.message-attachments.render-channel-icon.forwarded-source-avatar"
				>
					<Avatar
						user={sourceUser}
						size={iconSize}
						status={null}
						data-flx="channel.message-attachments.render-channel-icon.avatar"
					/>
				</div>
			);
		}
		if (sourceChannel.type === ChannelTypes.GROUP_DM) {
			return (
				<div
					className={styles.forwardedSourceAvatar}
					data-flx="channel.message-attachments.render-channel-icon.forwarded-source-avatar--2"
				>
					<GroupDMAvatar
						channel={sourceChannel}
						size={iconSize}
						data-flx="channel.message-attachments.render-channel-icon.group-dm-avatar"
					/>
				</div>
			);
		}
		if (sourceChannel.type === ChannelTypes.GUILD_VOICE) {
			return (
				<SpeakerHighIcon
					className={styles.forwardedSourceIcon}
					weight="fill"
					size={iconSize}
					data-flx="channel.message-attachments.render-channel-icon.forwarded-source-icon--2"
				/>
			);
		}
		return (
			<HashIcon
				className={styles.forwardedSourceIcon}
				weight="bold"
				size={iconSize}
				data-flx="channel.message-attachments.render-channel-icon.forwarded-source-icon--3"
			/>
		);
	}, [sourceChannel, sourceUser]);
	if (!hasAccessToSource || !sourceChannel || !displayName || !message.messageReference) {
		return null;
	}
	if (
		sourceChannel.type === ChannelTypes.DM ||
		sourceChannel.type === ChannelTypes.GROUP_DM ||
		sourceChannel.type === ChannelTypes.DM_PERSONAL_NOTES
	) {
		return (
			<FocusRing data-flx="channel.message-attachments.forwarded-from-source.focus-ring">
				<button
					type="button"
					onClick={handleJumpToOriginal}
					className={styles.forwardedSourceButton}
					data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-button.jump-to-original"
				>
					<span
						className={styles.forwardedSourceLabel}
						data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-label"
					>
						<Trans>Forwarded from</Trans>
					</span>
					<span
						className={styles.forwardedSourceInfo}
						data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-info"
					>
						{renderChannelIcon()}
						<span
							className={styles.forwardedSourceName}
							data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-name"
						>
							{displayName}
						</span>
					</span>
				</button>
			</FocusRing>
		);
	}
	if (sourceGuild) {
		return (
			<FocusRing data-flx="channel.message-attachments.forwarded-from-source.focus-ring--2">
				<button
					type="button"
					onClick={handleJumpToOriginal}
					className={styles.forwardedSourceButton}
					data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-button.jump-to-original--2"
				>
					<span
						className={styles.forwardedSourceLabel}
						data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-label--2"
					>
						<Trans>Forwarded from</Trans>
					</span>
					<span
						className={styles.forwardedSourceInfo}
						data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-info--2"
					>
						<GuildIcon
							id={sourceGuild.id}
							name={sourceGuild.name}
							icon={sourceGuild.icon}
							className={styles.forwardedSourceGuildIcon}
							sizePx={16}
							data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-guild-icon"
						/>
						<span
							className={styles.forwardedSourceName}
							data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-name--2"
						>
							{sourceGuild.name}
						</span>
						<CaretRightIcon
							className={styles.forwardedSourceChevron}
							weight="bold"
							size={12}
							data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-chevron"
						/>
						{renderChannelIcon()}
						<span
							className={styles.forwardedSourceName}
							data-flx="channel.message-attachments.forwarded-from-source.forwarded-source-name--3"
						>
							{displayName}
						</span>
					</span>
				</button>
			</FocusRing>
		);
	}
	return null;
});

interface ForwardedMessageContentProps {
	message: Message;
	snapshot: MessageSnapshot;
	onDelete?: (bypassConfirm?: boolean) => void;
}

export const ForwardedMessageContent = observer(({message, snapshot, onDelete}: ForwardedMessageContentProps) => {
	const {i18n} = useLingui();
	const snapshotIndex = 0;
	const snapshotEditedTimestamp = snapshot.edited_timestamp ? new Date(snapshot.edited_timestamp) : null;
	const copyText = useMemo(
		() =>
			buildMessageSnapshotCopyText(snapshot, {
				channelId: message.channelId,
				messageId: message.id,
				i18n,
			}),
		[i18n.locale, message.channelId, message.id, snapshot],
	);
	return (
		<div
			className={styles.forwardedContainer}
			data-message-copy-block={copyText ? 'true' : undefined}
			data-message-copy-text={copyText || undefined}
			data-flx="channel.message-attachments.forwarded-message-content.forwarded-container"
		>
			<div
				className={styles.forwardedBar}
				data-flx="channel.message-attachments.forwarded-message-content.forwarded-bar"
			/>
			<div
				className={styles.forwardedContent}
				data-flx="channel.message-attachments.forwarded-message-content.forwarded-content"
			>
				<div
					className={styles.forwardedHeader}
					data-flx="channel.message-attachments.forwarded-message-content.forwarded-header"
				>
					<ArrowBendUpRightIcon
						className={styles.forwardedIcon}
						weight="bold"
						data-flx="channel.message-attachments.forwarded-message-content.forwarded-icon"
					/>
					<span
						className={styles.forwardedLabel}
						data-flx="channel.message-attachments.forwarded-message-content.forwarded-label"
					>
						<Trans>Forwarded</Trans>
					</span>
				</div>
				{snapshot.content && (
					<div
						className={clsx(markupStyles.markup)}
						data-search-highlight-scope="message"
						data-flx="channel.message-attachments.forwarded-message-content.div"
					>
						<SafeMarkdown
							content={snapshot.content}
							options={{
								context: MarkdownContext.STANDARD_WITH_JUMBO,
								messageId: message.id,
								channelId: message.channelId,
								mentionChannels: snapshot.mention_channels,
							}}
							data-flx="channel.message-attachments.forwarded-message-content.safe-markdown"
						/>
						{snapshotEditedTimestamp && (
							<TimestampWithTooltip
								date={snapshotEditedTimestamp}
								className={messageStyles.editedTimestamp}
								data-flx="channel.message-attachments.forwarded-message-content.timestamp-with-tooltip"
							>
								<span
									className={messageStyles.editedLabel}
									data-flx="channel.message-attachments.forwarded-message-content.span"
								>
									{' '}
									<Trans>(edited)</Trans>
								</span>
							</TimestampWithTooltip>
						)}
					</div>
				)}
				{snapshot.attachments && snapshot.attachments.length > 0 && (
					<div
						className={styles.attachmentsContainer}
						data-flx="channel.message-attachments.forwarded-message-content.attachments-container"
					>
						{(() => {
							const {enrichedAttachments, mediaAttachments, shouldUseMosaic} = getAttachmentRenderingState(
								snapshot.attachments,
							);
							return (
								<>
									{shouldUseMosaic && (
										<AttachmentMosaic
											attachments={mediaAttachments}
											message={message}
											snapshotIndex={snapshotIndex}
											onDelete={onDelete}
											data-flx="channel.message-attachments.forwarded-message-content.attachment-mosaic"
										/>
									)}
									{enrichedAttachments.map((attachment: MessageAttachment) => (
										<Attachment
											key={attachment.id}
											attachment={attachment}
											snapshotIndex={snapshotIndex}
											message={message}
											renderInMosaic={shouldUseMosaic}
											onDelete={onDelete}
											data-flx="channel.message-attachments.forwarded-message-content.attachment"
										/>
									))}
								</>
							);
						})()}
					</div>
				)}
				{snapshot.embeds && snapshot.embeds.length > 0 && UserSettings.getRenderEmbeds() && (
					<div
						className={styles.attachmentsContainer}
						data-flx="channel.message-attachments.forwarded-message-content.attachments-container--2"
					>
						{snapshot.embeds.map((embed: MessageEmbed, index: number) => {
							const embedKey = `${embed.id}-${index}`;
							return (
								<Embed
									embed={embed}
									key={embedKey}
									message={message}
									embedIndex={index}
									contextualEmbeds={snapshot.embeds}
									onDelete={onDelete}
									isPreview={true}
									data-flx="channel.message-attachments.forwarded-message-content.embed"
								/>
							);
						})}
					</div>
				)}
				{snapshot.stickers && snapshot.stickers.length > 0 && (
					<div
						className={styles.stickersContainer}
						data-flx="channel.message-attachments.forwarded-message-content.stickers-container"
					>
						{snapshot.stickers.map((sticker: MessageStickerItem) => (
							<StickerItem
								key={sticker.id}
								sticker={sticker}
								message={message}
								handleDelete={onDelete}
								data-flx="channel.message-attachments.forwarded-message-content.sticker-item"
							/>
						))}
					</div>
				)}
				<ForwardedFromSource
					message={message}
					data-flx="channel.message-attachments.forwarded-message-content.forwarded-from-source"
				/>
			</div>
		</div>
	);
});

interface StickerItemProps {
	sticker: MessageStickerItem;
	message: Message;
	sourceChannel?: Channel | null;
	handleDelete?: (bypassConfirm?: boolean) => void;
}

const StickerItem = observer(({sticker, message, sourceChannel, handleDelete}: StickerItemProps) => {
	const {shouldAnimate, interactionHandlers} = useStickerAnimation({isAnimated: sticker.animated});
	const stickerUrl = AvatarUtils.getStickerURL({
		id: sticker.id,
		animated: shouldAnimate,
		isAnimatable: sticker.animated,
		size: 320,
	});
	const stickerRecord = Sticker.getStickerById(sticker.id);
	const guild = stickerRecord?.guildId ? Guilds.getGuild(stickerRecord.guildId) : null;
	const isMature = !!stickerRecord?.nsfw || !!sticker.nsfw;
	const {shouldBlur, shouldBlock, canReveal, reveal} = useMatureMedia(isMature, message.channelId);
	const tooltipContent = () => (
		<div className={styles.stickerTooltip} data-flx="channel.message-attachments.tooltip-content.sticker-tooltip">
			<span className={styles.stickerName} data-flx="channel.message-attachments.tooltip-content.sticker-name">
				{sticker.name}
			</span>
			{guild && (
				<div
					className={styles.stickerGuildInfo}
					data-flx="channel.message-attachments.tooltip-content.sticker-guild-info"
				>
					<GuildIcon
						id={guild.id}
						name={guild.name}
						icon={guild.icon}
						className={styles.stickerGuildIcon}
						sizePx={16}
						data-flx="channel.message-attachments.tooltip-content.sticker-guild-icon"
					/>
					<span
						className={styles.stickerGuildName}
						data-flx="channel.message-attachments.tooltip-content.sticker-guild-name"
					>
						{guild.name}
					</span>
				</div>
			)}
		</div>
	);
	const handleContextMenu = (e: React.MouseEvent) => {
		e.preventDefault();
		e.stopPropagation();
		const stickerForMenu = stickerRecord ?? {
			id: sticker.id,
			guildId: '',
			name: sticker.name,
			description: '',
			tags: [],
			url: stickerUrl,
			animated: sticker.animated,
			nsfw: Boolean(sticker.nsfw),
			user: undefined,
		};
		ContextMenuCommands.openFromEvent(e, ({onClose}) => (
			<MessageContextMenu
				message={message}
				sourceChannel={sourceChannel}
				onClose={onClose}
				onDelete={handleDelete!}
				inlineStickerOrEmojiItems={
					<StickerInlineMenuItems
						sticker={stickerForMenu}
						onClose={onClose}
						data-flx="channel.message-attachments.handle-context-menu.sticker-inline-menu-items"
					/>
				}
				data-flx="channel.message-attachments.handle-context-menu.message-context-menu"
			/>
		));
	};
	const handleRevealClick = useCallback(
		(e: React.MouseEvent) => {
			if (shouldBlur && canReveal) {
				e.preventDefault();
				e.stopPropagation();
				reveal();
			}
		},
		[shouldBlur, canReveal, reveal],
	);
	if (shouldBlock) {
		return null;
	}
	const stickerImage = (
		<img
			src={stickerUrl}
			alt={stickerRecord?.description || sticker.name}
			className={clsx(styles.stickerImage, shouldBlur && matureStyles.matureStickerBlurred)}
			width="160"
			height="160"
			data-flx="channel.message-attachments.sticker-item.sticker-image"
		/>
	);
	return (
		<Tooltip key={sticker.id} text={tooltipContent} data-flx="channel.message-attachments.sticker-item.tooltip">
			<FocusRing data-flx="channel.message-attachments.sticker-item.focus-ring">
				<button
					type="button"
					aria-label={stickerRecord?.description || sticker.name}
					className={styles.stickerWrapper}
					data-message-sticker="true"
					onContextMenu={handleContextMenu}
					onClick={handleRevealClick}
					data-flx="channel.message-attachments.sticker-item.sticker-wrapper.reveal-click"
					{...interactionHandlers}
				>
					{stickerImage}
				</button>
			</FocusRing>
		</Tooltip>
	);
});
export const MessageAttachments = observer(() => {
	const {channel, message, handleDelete, previewContext, onPopoutToggle, suppressMessageActions} =
		useMessageViewContext();
	const isPreview = Boolean(previewContext);
	const reactionsIsPreview = isPreview || Boolean(suppressMessageActions);
	const reactions = useMessageReactionsSnapshot(message.id);
	const spoileredUrlEmbeds = useMemo(() => {
		const embeddableCodeLinkContent = extractEmbeddableCodeLinkContent(message.content);
		return {
			invites: buildSpoileredCodeLinkMap(InviteUtils.findSpoileredInvites(embeddableCodeLinkContent), 'invite'),
			themes: buildSpoileredCodeLinkMap(ThemeUtils.findSpoileredThemes(embeddableCodeLinkContent), 'theme'),
			gifts: buildSpoileredCodeLinkMap(GiftCodeUtils.findSpoileredGifts(embeddableCodeLinkContent), 'gift'),
		};
	}, [message.content]);
	return (
		<>
			{message.messageSnapshots && message.messageSnapshots.length > 0 && (
				<ForwardedMessageContent
					message={message}
					snapshot={message.messageSnapshots[0]}
					onDelete={handleDelete}
					data-flx="channel.message-attachments.forwarded-message-content"
				/>
			)}
			{message.invites.map((code) => (
				<SpoileredUrlEmbed
					key={code}
					channelId={message.channelId}
					spoilerKeys={spoileredUrlEmbeds.invites.get(code)}
					data-flx="channel.message-attachments.spoilered-url-embed"
				>
					<FocusRing data-flx="channel.message-attachments.focus-ring">
						<InviteEmbed
							code={code}
							message={message}
							sourceChannel={channel}
							onDelete={handleDelete}
							data-flx="channel.message-attachments.invite-embed"
						/>
					</FocusRing>
				</SpoileredUrlEmbed>
			))}
			{message.themes.map((themeId) => (
				<SpoileredUrlEmbed
					key={themeId}
					channelId={message.channelId}
					spoilerKeys={spoileredUrlEmbeds.themes.get(themeId)}
					data-flx="channel.message-attachments.spoilered-url-embed--2"
				>
					<FocusRing data-flx="channel.message-attachments.focus-ring--2">
						<ThemeEmbed themeId={themeId} data-flx="channel.message-attachments.theme-embed" />
					</FocusRing>
				</SpoileredUrlEmbed>
			))}
			{message.gifts.map((code) => (
				<SpoileredUrlEmbed
					key={code}
					channelId={message.channelId}
					spoilerKeys={spoileredUrlEmbeds.gifts.get(code)}
					data-flx="channel.message-attachments.spoilered-url-embed--3"
				>
					<FocusRing data-flx="channel.message-attachments.focus-ring--3">
						<GiftEmbed code={code} data-flx="channel.message-attachments.gift-embed" />
					</FocusRing>
				</SpoileredUrlEmbed>
			))}
			{message.stickers && message.stickers.length > 0 && (
				<div className={styles.stickersContainer} data-flx="channel.message-attachments.stickers-container">
					{message.stickers.map((sticker: MessageStickerItem) => (
						<StickerItem
							key={sticker.id}
							sticker={sticker}
							message={message}
							sourceChannel={channel}
							handleDelete={handleDelete}
							data-flx="channel.message-attachments.sticker-item"
						/>
					))}
				</div>
			)}
			{(() => {
				const {enrichedAttachments, mediaAttachments, shouldUseMosaic} = getAttachmentRenderingState(
					message.attachments,
				);
				return (
					<>
						{shouldUseMosaic && (
							<AttachmentMosaic
								attachments={mediaAttachments}
								message={message}
								isPreview={isPreview}
								onDelete={handleDelete}
								data-flx="channel.message-attachments.attachment-mosaic"
							/>
						)}
						{enrichedAttachments.map((attachment) => (
							<Attachment
								key={attachment.id}
								attachment={attachment}
								isPreview={isPreview}
								message={message}
								renderInMosaic={shouldUseMosaic}
								onDelete={handleDelete}
								data-flx="channel.message-attachments.attachment"
							/>
						))}
					</>
				);
			})()}
			{UserSettings.getRenderEmbeds() &&
				!message.suppressEmbeds &&
				message.embeds.map((embed, index) => {
					const embedKey = `${embed.id}-${index}`;
					return (
						<Embed
							embed={embed}
							key={embedKey}
							message={message}
							embedIndex={index}
							onDelete={handleDelete}
							isPreview={isPreview}
							data-flx="channel.message-attachments.embed"
						/>
					);
				})}
			{UserSettings.getRenderReactions() && reactions.length > 0 && (
				<MessageReactions
					message={message}
					isPreview={reactionsIsPreview}
					onPopoutToggle={onPopoutToggle}
					data-flx="channel.message-attachments.message-reactions"
				/>
			)}
		</>
	);
});
