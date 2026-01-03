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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {CaretRightIcon, ChatTeardropIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ThemeActionCreators from '~/actions/ThemeActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {ExternalLinkWarningModal} from '~/components/modals/ExternalLinkWarningModal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import ChannelStore from '~/stores/ChannelStore';
import DeveloperModeStore from '~/stores/DeveloperModeStore';
import GuildStore from '~/stores/GuildStore';
import TrustedDomainStore from '~/stores/TrustedDomainStore';
import markupStyles from '~/styles/Markup.module.css';
import {APP_PROTOCOL_PREFIX} from '~/utils/appProtocol';
import {getDMDisplayName, getIcon, getName} from '~/utils/ChannelUtils';
import {
	isInternalChannelHost,
	parseChannelJumpLink,
	parseChannelUrl,
	parseMessageJumpLink,
} from '~/utils/DeepLinkUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import {goToMessage} from '~/utils/MessageNavigator';
import {openExternalUrl} from '~/utils/NativeUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import * as ThemeUtils from '~/utils/ThemeUtils';
import type {LinkNode} from '../parser/types/nodes';
import type {RendererProps} from '.';
import jumpLinkStyles from './MessageJumpLink.module.css';

interface JumpLinkMentionProps {
	channel: ChannelRecord;
	guild: GuildRecord | null;
	messageId?: string;
	i18n: I18n;
	interactive?: boolean;
}

const INLINE_REPLY_CONTEXT = 1;

const JumpLinkMention = observer(function JumpLinkMention({
	channel,
	guild,
	messageId,
	i18n,
	interactive = true,
}: JumpLinkMentionProps) {
	const handleClick = React.useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			event.preventDefault();
			event.stopPropagation();

			if (messageId) {
				goToMessage(channel.id, messageId);
				return;
			}

			const channelPath = channel.guildId
				? Routes.guildChannel(channel.guildId, channel.id)
				: Routes.dmChannel(channel.id);
			RouterUtils.transitionTo(channelPath);
		},
		[channel.id, channel.guildId, messageId],
	);

	const displayName = channel.isPrivate() ? getDMDisplayName(channel) : (channel.name ?? channel.id);
	const labelText = guild ? guild.name : displayName;
	const shouldShowChannelInfo = !messageId && Boolean(channel.guildId);
	const channelDisplayName = channel.name ?? getName(channel);
	const isDMChannel = channel.isPrivate() && !channel.guildId;
	const shouldShowDMIconLabel = isDMChannel && !messageId;
	const hasDetailChunk = Boolean(messageId) || shouldShowChannelInfo;
	const ariaLabel = messageId
		? labelText
			? i18n._(msg`Jump to the message in ${labelText}`)
			: i18n._(msg`Jump to the linked message`)
		: labelText
			? i18n._(msg`Jump to ${labelText}`)
			: i18n._(msg`Jump to the linked channel`);

	const Component = interactive ? 'button' : 'span';

	return (
		<Component
			{...(interactive ? {type: 'button'} : {})}
			className={clsx(markupStyles.mention, interactive && markupStyles.interactive, jumpLinkStyles.jumpLinkButton)}
			onClick={handleClick}
			aria-label={ariaLabel}
			tabIndex={interactive ? 0 : -1}
		>
			<span className={jumpLinkStyles.jumpLinkInfo}>
				{guild ? (
					<span className={jumpLinkStyles.jumpLinkGuild}>
						<GuildIcon
							id={guild.id}
							name={guild.name}
							icon={guild.icon}
							className={jumpLinkStyles.jumpLinkGuildIcon}
							containerProps={{'data-jump-link-guild-icon': ''}}
						/>
						<span className={jumpLinkStyles.jumpLinkGuildName}>{guild.name}</span>
					</span>
				) : shouldShowDMIconLabel ? (
					<span className={jumpLinkStyles.jumpLinkDM}>
						<span className={jumpLinkStyles.jumpLinkChannelIcon}>{getIcon(channel, {size: 12})}</span>
						<span className={jumpLinkStyles.jumpLinkDMName}>{displayName}</span>
					</span>
				) : (
					<span className={jumpLinkStyles.jumpLinkLabel}>{displayName}</span>
				)}
				{hasDetailChunk && (
					<span className={jumpLinkStyles.jumpLinkMessage} aria-hidden="true">
						<CaretRightIcon size={6} weight="bold" className={jumpLinkStyles.jumpLinkCaret} />
						{messageId ? (
							<span className={jumpLinkStyles.jumpLinkMessageIcon}>
								<ChatTeardropIcon size={12} weight="fill" />
							</span>
						) : (
							shouldShowChannelInfo && (
								<span className={jumpLinkStyles.jumpLinkChannel}>
									<span className={jumpLinkStyles.jumpLinkChannelIcon}>{getIcon(channel, {size: 12})}</span>
									<span className={jumpLinkStyles.jumpLinkChannelName}>{channelDisplayName}</span>
								</span>
							)
						)}
					</span>
				)}
			</span>
		</Component>
	);
});

export const LinkRenderer = observer(function LinkRenderer({
	node,
	id,
	renderChildren,
	options,
}: RendererProps<LinkNode>): React.ReactElement {
	const i18n = options.i18n!;
	const {url, text} = node;

	const inviteCode = InviteUtils.findInvite(url);
	const themeCode = ThemeUtils.findTheme(url);
	const messageJumpTarget = parseMessageJumpLink(url);
	const jumpTarget = messageJumpTarget ?? parseChannelJumpLink(url);
	const jumpChannel = jumpTarget ? (ChannelStore.getChannel(jumpTarget.channelId) ?? null) : null;
	const jumpGuild = jumpChannel?.guildId ? (GuildStore.getGuild(jumpChannel.guildId) ?? null) : null;
	const isInlineReplyContext = options.context === INLINE_REPLY_CONTEXT;

	if (jumpTarget && jumpChannel) {
		const mention = (
			<JumpLinkMention
				channel={jumpChannel}
				guild={jumpGuild}
				messageId={messageJumpTarget?.messageId}
				i18n={i18n}
				interactive={!isInlineReplyContext}
			/>
		);

		return isInlineReplyContext ? mention : <FocusRing key={id}>{mention}</FocusRing>;
	}

	const shouldShowAccessDeniedModal = Boolean(jumpTarget && !jumpChannel);

	let isInternal = false;
	let handleClick: ((e: React.MouseEvent) => void) | undefined;

	if (shouldShowAccessDeniedModal) {
		handleClick = (event) => {
			event.preventDefault();
			event.stopPropagation();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(msg`Channel Access Denied`)}
						description={i18n._(msg`You do not have access to the channel where this message was sent.`)}
						primaryText={i18n._(msg`Okay`)}
						primaryVariant="primary"
						secondaryText={false}
						onPrimary={() => {}}
					/>
				)),
			);
		};
		isInternal = true;
	} else if (url === `${APP_PROTOCOL_PREFIX}dev`) {
		handleClick = (e) => {
			e.preventDefault();
			if (DeveloperModeStore.isDeveloper) {
				ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="developer_options" />));
			} else {
				ModalActionCreators.push(
					modal(() => (
						<ConfirmModal
							title="Secret Link Found!"
							description="You found a secret link, but it wasn't meant for you!"
							primaryText="Okay"
							primaryVariant="primary"
							secondaryText={false}
							onPrimary={() => {}}
						/>
					)),
				);
			}
		};
		isInternal = true;
	} else {
		try {
			const parsed = new URL(url);
			isInternal = isInternalChannelHost(parsed.host) && parsed.pathname.startsWith('/channels/');

			if (inviteCode) {
				handleClick = (e) => {
					e.preventDefault();
					InviteActionCreators.acceptAndTransitionToChannel(inviteCode, i18n);
				};
			} else if (themeCode) {
				handleClick = (e) => {
					e.preventDefault();
					ThemeActionCreators.openAcceptModal(themeCode, i18n);
				};
				isInternal = true;
			} else if (isInternal) {
				const channelPath = parseChannelUrl(url);
				if (channelPath) {
					handleClick = (e) => {
						e.preventDefault();
						RouterUtils.transitionTo(channelPath);
					};
				} else {
					isInternal = false;
				}
			}

			if (!isInternal && !inviteCode) {
				const isTrusted = TrustedDomainStore.isTrustedDomain(parsed.hostname);
				if (!isTrusted) {
					handleClick = (e) => {
						e.preventDefault();
						ModalActionCreators.push(modal(() => <ExternalLinkWarningModal url={url} hostname={parsed.hostname} />));
					};
				}
			}
		} catch (_error) {
			console.warn('Invalid URL in link:', url);
		}
	}

	const content = text ? renderChildren([text]) : url;

	return (
		<FocusRing key={id}>
			<a
				href={url}
				target={isInternal ? undefined : '_blank'}
				rel={isInternal ? undefined : 'noopener noreferrer'}
				onClick={(e) => {
					e.stopPropagation();
					if (handleClick) {
						handleClick(e);
						return;
					}
					if (!isInternal) {
						e.preventDefault();
						void openExternalUrl(url);
					}
				}}
				className={markupStyles.link}
			>
				{content}
			</a>
		</FocusRing>
	);
});
