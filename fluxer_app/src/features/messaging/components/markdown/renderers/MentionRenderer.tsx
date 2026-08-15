// SPDX-License-Identifier: AGPL-3.0-or-later

import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {MentionLabel} from '@app/features/messaging/components/markdown/renderers/MentionLabel';
import type {RendererProps} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {GuildNavKind, MentionKind} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {MentionNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import mentionRendererStyles from '@app/features/theme/styles/MentionRenderer.module.css';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import {ChannelContextMenu} from '@app/features/ui/action_menu/ChannelContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useContextMenuTrigger} from '@app/features/ui/hooks/useContextMenuTrigger';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const UNKNOWN_ROLE_DESCRIPTOR = msg({
	message: 'Unknown role',
	comment: 'Short label in the messaging mention renderer. Keep it concise.',
});
const UNKNOWN_CHANNEL_DESCRIPTOR = msg({
	message: 'unknown-channel',
	comment: 'Short label in the messaging mention renderer. Keep it concise.',
});
const CHANNEL_LINK_DESCRIPTOR = msg({
	message: 'channel link',
	comment: 'Short label in the messaging mention renderer. Keep it concise.',
});

interface InteractiveChannelMentionProps {
	channel: Channel;
	roleDescription: string;
}

function InteractiveChannelMention({channel, roleDescription}: InteractiveChannelMentionProps): React.ReactElement {
	const {isOpen, withTracking} = useContextMenuTrigger();
	const activate = () => {
		if (LinkChannelCommands.openLinkChannel(channel)) {
			return;
		}
		const guildId = channel.guildId;
		if (guildId == null) {
			throw new Error('Interactive channel mentions must belong to a guild');
		}
		NavigationCommands.selectChannel(guildId, channel.id);
	};
	return (
		<FocusRing offset={-2} data-flx="messaging.markdown.renderers.mention-renderer.focus-ring--2">
			<span
				role="button"
				tabIndex={0}
				className={clsx(markupStyles.mention, markupStyles.interactive)}
				aria-haspopup="menu"
				aria-expanded={isOpen}
				aria-roledescription={roleDescription}
				onClick={(event) => {
					event.stopPropagation();
					activate();
				}}
				onKeyDown={(event) => {
					if (!isKeyboardActivationKey(event.key)) return;
					event.preventDefault();
					event.stopPropagation();
					activate();
				}}
				onContextMenu={(event) => {
					event.preventDefault();
					event.stopPropagation();
					ContextMenuCommands.openFromEvent(
						event,
						({onClose}) => (
							<ChannelContextMenu
								channel={channel}
								onClose={onClose}
								data-flx="messaging.markdown.renderers.mention-renderer.channel-context-menu"
							/>
						),
						withTracking(),
					);
				}}
				data-flx="messaging.markdown.renderers.mention-renderer.button.stop-propagation--2"
			>
				<MentionLabel icon={ChannelUtils.getIcon(channel, {className: mentionRendererStyles.channelIcon})}>
					{channel.name}
				</MentionLabel>
			</span>
		</FocusRing>
	);
}

export const MentionRenderer = observer(function MentionRenderer({
	node,
	id,
	options,
}: RendererProps<MentionNode>): React.ReactElement {
	const {kind} = node;
	const {channelId} = options;
	const i18n = options.i18n!;
	const shouldDisableInteractions = options.disableInteractions === true;
	switch (kind.kind) {
		case MentionKind.User: {
			const user = kind.id ? Users.getUser(kind.id) : null;
			const channel = channelId ? Channels.getChannel(channelId) : undefined;
			const resolvedGuildId = channel?.guildId || options.guildId || '';
			let name: string | null = null;
			if (user) {
				name = user.displayName;
				if (resolvedGuildId) {
					name = NicknameUtils.getNickname(user, resolvedGuildId) || name;
				}
			}
			const genericMention = (
				<span key={id} className={markupStyles.mention} data-flx="messaging.markdown.renderers.mention-renderer.span">
					<MentionLabel>@{name || kind.id}</MentionLabel>
				</span>
			);
			if (!user) {
				return genericMention;
			}
			if (shouldDisableInteractions) {
				return (
					<span
						key={id}
						className={markupStyles.mention}
						data-flx="messaging.markdown.renderers.mention-renderer.span--2"
					>
						<MentionLabel>@{name || user.displayName}</MentionLabel>
					</span>
				);
			}
			return (
				<PreloadableUserPopout
					key={id}
					user={user}
					isWebhook={false}
					guildId={resolvedGuildId}
					position="right-start"
					data-flx="messaging.markdown.renderers.mention-renderer.preloadable-user-popout"
				>
					<FocusRing offset={-2} data-flx="messaging.markdown.renderers.mention-renderer.focus-ring">
						<span
							role="button"
							tabIndex={0}
							className={clsx(markupStyles.mention, markupStyles.interactive)}
							onClick={(e) => e.stopPropagation()}
							onKeyDown={(e) => {
								if (!isKeyboardActivationKey(e.key)) return;
								e.preventDefault();
								e.stopPropagation();
							}}
							data-flx="messaging.markdown.renderers.mention-renderer.button.stop-propagation"
						>
							<MentionLabel>@{name || user.displayName}</MentionLabel>
						</span>
					</FocusRing>
				</PreloadableUserPopout>
			);
		}
		case MentionKind.Role: {
			const channel = channelId ? Channels.getChannel(channelId) : null;
			const resolvedGuildId = channel?.guildId || options.guildId || SelectedGuild.selectedGuildId;
			const guild = resolvedGuildId != null ? Guilds.getGuild(resolvedGuildId) : null;
			const role = guild?.roles[kind.id];
			if (!role) {
				return (
					<span
						key={id}
						className={markupStyles.mention}
						data-flx="messaging.markdown.renderers.mention-renderer.span--3"
					>
						<MentionLabel>@{i18n._(UNKNOWN_ROLE_DESCRIPTOR)}</MentionLabel>
					</span>
				);
			}
			const style = role.color
				? ({
						'--mention-color': ColorUtils.int2rgb(role.color),
						backgroundColor: ColorUtils.int2rgba(role.color, 0.1),
						boxShadow: `inset 0 0 0 1px ${ColorUtils.int2rgba(role.color, 0.3)}`,
					} as React.CSSProperties)
				: undefined;
			return (
				<span
					key={id}
					className={markupStyles.mention}
					style={style}
					data-flx="messaging.markdown.renderers.mention-renderer.span--4"
				>
					<MentionLabel>@{role.name}</MentionLabel>
				</span>
			);
		}
		case MentionKind.Channel: {
			const fallbackMention = options.mentionChannels?.find((mention) => mention.id === kind.id);
			const unknownMention = (
				<span
					key={id}
					className={markupStyles.mention}
					data-flx="messaging.markdown.renderers.mention-renderer.span--5"
				>
					<MentionLabel
						icon={ChannelUtils.getIcon({type: ChannelTypes.GUILD_TEXT}, {className: mentionRendererStyles.channelIcon})}
					>
						{i18n._(UNKNOWN_CHANNEL_DESCRIPTOR)}
					</MentionLabel>
				</span>
			);
			const channel = Channels.getChannel(kind.id);
			if (!channel) {
				if (fallbackMention) {
					return (
						<span
							key={id}
							className={markupStyles.mention}
							data-flx="messaging.markdown.renderers.mention-renderer.span--6"
						>
							<MentionLabel
								icon={ChannelUtils.getIcon(fallbackMention, {className: mentionRendererStyles.channelIcon})}
							>
								{fallbackMention.name}
							</MentionLabel>
						</span>
					);
				}
				return unknownMention;
			}
			if (channel.type === ChannelTypes.GUILD_CATEGORY) {
				return (
					<span key={id} data-flx="messaging.markdown.renderers.mention-renderer.span--7">
						#{channel.name}
					</span>
				);
			}
			if (
				channel.type !== ChannelTypes.GUILD_TEXT &&
				channel.type !== ChannelTypes.GUILD_VOICE &&
				channel.type !== ChannelTypes.GUILD_LINK
			) {
				return unknownMention;
			}
			if (shouldDisableInteractions) {
				return (
					<span
						key={id}
						className={markupStyles.mention}
						data-flx="messaging.markdown.renderers.mention-renderer.span--8"
					>
						<MentionLabel icon={ChannelUtils.getIcon(channel, {className: mentionRendererStyles.channelIcon})}>
							{channel.name}
						</MentionLabel>
					</span>
				);
			}
			return <InteractiveChannelMention key={id} channel={channel} roleDescription={i18n._(CHANNEL_LINK_DESCRIPTOR)} />;
		}
		case MentionKind.Everyone: {
			return (
				<span
					key={id}
					className={markupStyles.mention}
					data-flx="messaging.markdown.renderers.mention-renderer.span--9"
				>
					<MentionLabel>@everyone</MentionLabel>
				</span>
			);
		}
		case MentionKind.Here: {
			return (
				<span
					key={id}
					className={markupStyles.mention}
					data-flx="messaging.markdown.renderers.mention-renderer.span--10"
				>
					<MentionLabel>@here</MentionLabel>
				</span>
			);
		}
		case MentionKind.Command: {
			const {name, subcommandGroup, subcommand} = kind;
			const commandName = [
				`/${name}`,
				...(subcommandGroup ? [subcommandGroup] : []),
				...(subcommand ? [subcommand] : []),
			].join(' ');
			return (
				<span
					key={id}
					className={markupStyles.mention}
					data-flx="messaging.markdown.renderers.mention-renderer.span--11"
				>
					<MentionLabel>{commandName}</MentionLabel>
				</span>
			);
		}
		case MentionKind.GuildNavigation: {
			const {navigationType} = kind;
			let content: string;
			switch (navigationType) {
				case GuildNavKind.Customize:
					content = '<id:customize>';
					break;
				case GuildNavKind.Browse:
					content = '<id:browse>';
					break;
				case GuildNavKind.Guide:
					content = '<id:guide>';
					break;
				case GuildNavKind.LinkedRoles: {
					const linkedRolesId = (kind as {navigationType: 'LinkedRoles'; id?: string}).id;
					content = linkedRolesId ? `<id:linked-roles:${linkedRolesId}>` : '<id:linked-roles>';
					break;
				}
				default:
					content = `<id:${navigationType}>`;
					break;
			}
			return (
				<span
					key={id}
					className={markupStyles.mention}
					data-flx="messaging.markdown.renderers.mention-renderer.span--12"
				>
					<MentionLabel>{content}</MentionLabel>
				</span>
			);
		}
		default:
			return (
				<span key={id} data-flx="messaging.markdown.renderers.mention-renderer.span--13">
					{'<unknown-mention>'}
				</span>
			);
	}
});
