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
import {ArrowBendUpRightIcon, CaretRightIcon, HashIcon, NotePencilIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {ChannelTypes, StickerFormatTypes} from '~/Constants';
import {Attachment} from '~/components/channel/embeds/attachments/Attachment';
import {AttachmentMosaic} from '~/components/channel/embeds/attachments/AttachmentMosaic';
import {Embed} from '~/components/channel/embeds/Embed';
import {GiftEmbed} from '~/components/channel/GiftEmbed';
import {InviteEmbed} from '~/components/channel/InviteEmbed';
import {MessageReactions} from '~/components/channel/MessageReactions';
import {getAttachmentRenderingState} from '~/components/channel/messageAttachmentStateUtils';
import {ThemeEmbed} from '~/components/channel/ThemeEmbed';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {Avatar} from '~/components/uikit/Avatar';
import {MediaContextMenu} from '~/components/uikit/ContextMenu/MediaContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';
import type {
	MessageAttachment,
	MessageEmbed,
	MessageRecord,
	MessageSnapshot,
	MessageStickerItem,
} from '~/records/MessageRecord';
import GuildStore from '~/stores/GuildStore';
import StickerStore from '~/stores/StickerStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import markupStyles from '~/styles/Markup.module.css';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {useForwardedMessageContext} from '~/utils/forwardedMessageUtils';
import {goToMessage} from '~/utils/MessageNavigator';
import styles from './MessageAttachments.module.css';
import {useMessageViewContext} from './MessageViewContext';

const ForwardedFromSource = observer(({message}: {message: MessageRecord}) => {
	const {sourceChannel, sourceGuild, sourceUser, hasAccessToSource, displayName} = useForwardedMessageContext(message);

	const handleJumpToOriginal = React.useCallback(() => {
		if (message.messageReference && sourceChannel) {
			goToMessage(message.messageReference.channel_id, message.messageReference.message_id);
		}
	}, [message.messageReference, sourceChannel]);

	if (!hasAccessToSource || !sourceChannel || !displayName || !message.messageReference) {
		return null;
	}

	const renderChannelIcon = () => {
		const iconSize = 16;

		if (sourceChannel.type === ChannelTypes.DM_PERSONAL_NOTES) {
			return <NotePencilIcon className={styles.forwardedSourceIcon} weight="fill" size={iconSize} />;
		}
		if (sourceChannel.type === ChannelTypes.DM && sourceUser) {
			return (
				<div className={styles.forwardedSourceAvatar}>
					<Avatar user={sourceUser} size={iconSize} status={null} />
				</div>
			);
		}
		if (sourceChannel.type === ChannelTypes.GROUP_DM) {
			return (
				<div className={styles.forwardedSourceAvatar}>
					<GroupDMAvatar channel={sourceChannel} size={iconSize} />
				</div>
			);
		}
		if (sourceChannel.type === ChannelTypes.GUILD_VOICE) {
			return <SpeakerHighIcon className={styles.forwardedSourceIcon} weight="fill" size={iconSize} />;
		}
		return <HashIcon className={styles.forwardedSourceIcon} weight="bold" size={iconSize} />;
	};

	if (
		sourceChannel.type === ChannelTypes.DM ||
		sourceChannel.type === ChannelTypes.GROUP_DM ||
		sourceChannel.type === ChannelTypes.DM_PERSONAL_NOTES
	) {
		return (
			<FocusRing>
				<button type="button" onClick={handleJumpToOriginal} className={styles.forwardedSourceButton}>
					<span className={styles.forwardedSourceLabel}>
						<Trans>Forwarded from</Trans>
					</span>
					<span className={styles.forwardedSourceInfo}>
						{renderChannelIcon()}
						<span className={styles.forwardedSourceName}>{displayName}</span>
					</span>
				</button>
			</FocusRing>
		);
	}

	if (sourceGuild) {
		return (
			<FocusRing>
				<button type="button" onClick={handleJumpToOriginal} className={styles.forwardedSourceButton}>
					<span className={styles.forwardedSourceLabel}>
						<Trans>Forwarded from</Trans>
					</span>
					<span className={styles.forwardedSourceInfo}>
						<GuildIcon
							id={sourceGuild.id}
							name={sourceGuild.name}
							icon={sourceGuild.icon}
							className={styles.forwardedSourceGuildIcon}
							sizePx={16}
						/>
						<span className={styles.forwardedSourceName}>{sourceGuild.name}</span>
						<CaretRightIcon className={styles.forwardedSourceChevron} weight="bold" size={12} />
						{renderChannelIcon()}
						<span className={styles.forwardedSourceName}>{displayName}</span>
					</span>
				</button>
			</FocusRing>
		);
	}

	return null;
});

const ForwardedMessageContent = observer(({message, snapshot}: {message: MessageRecord; snapshot: MessageSnapshot}) => {
	const snapshotIsPreview = true;
	return (
		<div className={styles.forwardedContainer}>
			<div className={styles.forwardedBar} />
			<div className={styles.forwardedContent}>
				<div className={styles.forwardedHeader}>
					<ArrowBendUpRightIcon className={styles.forwardedIcon} weight="bold" />
					<span className={styles.forwardedLabel}>
						<Trans>Forwarded</Trans>
					</span>
				</div>

				{snapshot.content && (
					<div className={clsx(markupStyles.markup)}>
						<SafeMarkdown
							content={snapshot.content}
							options={{
								context: MarkdownContext.STANDARD_WITH_JUMBO,
								messageId: message.id,
								channelId: message.channelId,
							}}
						/>
					</div>
				)}

				{snapshot.attachments && snapshot.attachments.length > 0 && (
					<div className={styles.attachmentsContainer}>
						{(() => {
							const {enrichedAttachments, mediaAttachments, shouldUseMosaic} = getAttachmentRenderingState(
								snapshot.attachments,
							);
							return (
								<>
									{shouldUseMosaic && (
										<AttachmentMosaic attachments={mediaAttachments} message={message} isPreview={snapshotIsPreview} />
									)}
									{enrichedAttachments.map((attachment: MessageAttachment) => (
										<Attachment
											key={attachment.id}
											attachment={attachment}
											isPreview={snapshotIsPreview}
											message={message}
											renderInMosaic={shouldUseMosaic}
										/>
									))}
								</>
							);
						})()}
					</div>
				)}

				{snapshot.embeds && snapshot.embeds.length > 0 && UserSettingsStore.getRenderEmbeds() && (
					<div className={styles.attachmentsContainer}>
						{snapshot.embeds.map((embed: MessageEmbed, index: number) => {
							const embedKey = `${embed.id}-${index}`;
							return (
								<Embed
									embed={embed}
									key={embedKey}
									message={message}
									embedIndex={index}
									contextualEmbeds={snapshot.embeds}
									onDelete={() => {}}
									isPreview={snapshotIsPreview}
								/>
							);
						})}
					</div>
				)}

				<ForwardedFromSource message={message} />
			</div>
		</div>
	);
});

export const MessageAttachments = observer(() => {
	const {message, handleDelete, previewContext, onPopoutToggle} = useMessageViewContext();
	const isPreview = Boolean(previewContext);
	return (
		<>
			{message.messageSnapshots && message.messageSnapshots.length > 0 && (
				<ForwardedMessageContent message={message} snapshot={message.messageSnapshots[0]} />
			)}

			{message.invites.map((code) => (
				<FocusRing key={code}>
					<InviteEmbed code={code} />
				</FocusRing>
			))}

			{message.themes.map((themeId) => (
				<FocusRing key={themeId}>
					<ThemeEmbed themeId={themeId} />
				</FocusRing>
			))}

			{message.gifts.map((code) => (
				<FocusRing key={code}>
					<GiftEmbed code={code} />
				</FocusRing>
			))}

			{message.stickers && message.stickers.length > 0 && (
				<div className={styles.stickersContainer}>
					{message.stickers.map((sticker: MessageStickerItem) => {
						const stickerUrl = AvatarUtils.getStickerURL({
							id: sticker.id,
							animated: sticker.format_type === StickerFormatTypes.GIF,
							size: 320,
						});

						const stickerRecord = StickerStore.getStickerById(sticker.id);
						const guild = stickerRecord?.guildId ? GuildStore.getGuild(stickerRecord.guildId) : null;

						const tooltipContent = () => (
							<div className={styles.stickerTooltip}>
								<span className={styles.stickerName}>{sticker.name}</span>
								{guild && (
									<div className={styles.stickerGuildInfo}>
										<GuildIcon
											id={guild.id}
											name={guild.name}
											icon={guild.icon}
											className={styles.stickerGuildIcon}
											sizePx={16}
										/>
										<span className={styles.stickerGuildName}>{guild.name}</span>
									</div>
								)}
							</div>
						);

						const handleContextMenu = (e: React.MouseEvent) => {
							e.preventDefault();
							e.stopPropagation();

							ContextMenuActionCreators.openFromEvent(e, ({onClose}) => (
								<MediaContextMenu
									message={message}
									originalSrc={stickerUrl}
									type="image"
									defaultName={sticker.name}
									onClose={onClose}
									onDelete={handleDelete}
								/>
							));
						};

						return (
							<Tooltip key={sticker.id} text={tooltipContent}>
								<FocusRing>
									<div role="img" className={styles.stickerWrapper} onContextMenu={handleContextMenu}>
										<img
											src={stickerUrl}
											alt={stickerRecord?.description || sticker.name}
											className={styles.stickerImage}
											width="160"
											height="160"
										/>
									</div>
								</FocusRing>
							</Tooltip>
						);
					})}
				</div>
			)}

			{(() => {
				const {enrichedAttachments, mediaAttachments} = getAttachmentRenderingState(message.attachments);
				const inlineMedia = UserSettingsStore.getInlineAttachmentMedia();
				const shouldWrapInMosaic = inlineMedia && mediaAttachments.length > 0;
				return (
					<>
						{shouldWrapInMosaic && (
							<AttachmentMosaic attachments={mediaAttachments} message={message} isPreview={isPreview} />
						)}
						{enrichedAttachments.map((attachment) => (
							<Attachment
								key={attachment.id}
								attachment={attachment}
								isPreview={isPreview}
								message={message}
								renderInMosaic={shouldWrapInMosaic}
							/>
						))}
					</>
				);
			})()}

			{UserSettingsStore.getRenderEmbeds() &&
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
						/>
					);
				})}

			{UserSettingsStore.getRenderReactions() && message.reactions.length > 0 && (
				<MessageReactions message={message} isPreview={isPreview} onPopoutToggle={onPopoutToggle} />
			)}
		</>
	);
});
