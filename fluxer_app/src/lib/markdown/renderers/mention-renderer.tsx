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

import {msg} from '@lingui/core/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ChannelTypes, Permissions} from '~/Constants';
import {GenericErrorModal} from '~/components/alerts/GenericErrorModal';
import {PreloadableUserPopout} from '~/components/channel/PreloadableUserPopout';
import {ChannelContextMenu} from '~/components/uikit/ContextMenu/ChannelContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Routes} from '~/Routes';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import PermissionStore from '~/stores/PermissionStore';
import SelectedGuildStore from '~/stores/SelectedGuildStore';
import UserStore from '~/stores/UserStore';
import markupStyles from '~/styles/Markup.module.css';
import mentionRendererStyles from '~/styles/MentionRenderer.module.css';
import * as ChannelUtils from '~/utils/ChannelUtils';
import * as ColorUtils from '~/utils/ColorUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import {GuildNavKind, MentionKind} from '../parser/types/enums';
import type {MentionNode} from '../parser/types/nodes';
import type {RendererProps} from '.';

interface UserInfo {
	userId: string | null;
	name: string | null;
}

function getUserInfo(userId: string, channelId?: string): UserInfo {
	if (!userId) {
		return {userId: null, name: null};
	}

	const user = UserStore.getUser(userId);
	if (!user) {
		return {userId, name: userId};
	}

	let name = user.displayName;

	if (channelId) {
		const channel = ChannelStore.getChannel(channelId);
		if (channel?.guildId) {
			name = NicknameUtils.getNickname(user, channel.guildId) || name;
		}
	}

	return {userId: user.id, name};
}

export const MentionRenderer = observer(function MentionRenderer({
	node,
	id,
	options,
}: RendererProps<MentionNode>): React.ReactElement {
	const {kind} = node;
	const {channelId} = options;
	const i18n = options.i18n!;

	switch (kind.kind) {
		case MentionKind.User: {
			const {userId, name} = getUserInfo(kind.id, channelId);

			const genericMention = (
				<span key={id} className={markupStyles.mention}>
					@{name || kind.id}
				</span>
			);

			if (!userId) {
				return genericMention;
			}

			const user = UserStore.getUser(userId);
			if (!user) {
				return genericMention;
			}

			const channel = channelId ? ChannelStore.getChannel(channelId) : undefined;
			const guildId = channel?.guildId || '';

			return (
				<PreloadableUserPopout key={id} user={user} isWebhook={false} guildId={guildId} position="right-start">
					<FocusRing>
						<span
							role="button"
							tabIndex={0}
							className={clsx(markupStyles.mention, markupStyles.interactive)}
							onClick={(e) => e.stopPropagation()}
							onKeyDown={(e) => {
								if (e.key === 'Enter') {
									e.stopPropagation();
								}
							}}
						>
							@{name || user.displayName}
						</span>
					</FocusRing>
				</PreloadableUserPopout>
			);
		}

		case MentionKind.Role: {
			const selectedGuildId = SelectedGuildStore.selectedGuildId;
			const guild = selectedGuildId != null ? GuildStore.getGuild(selectedGuildId) : null;
			const role = guild?.roles[kind.id];

			if (!role) {
				return (
					<span key={id} className={markupStyles.mention}>
						@{i18n._(msg`Unknown Role`)}
					</span>
				);
			}

			const roleColor = role.color ? ColorUtils.int2rgb(role.color) : undefined;
			const roleBgColor = role.color ? ColorUtils.int2rgba(role.color, 0.1) : undefined;
			const roleBgHoverColor = role.color ? ColorUtils.int2rgba(role.color, 0.2) : undefined;
			const style = {
				color: roleColor,
				backgroundColor: roleBgColor,
				transition: 'background-color var(--transition-fast)',
				'--hover-bg': roleBgHoverColor,
			} as React.CSSProperties;

			return (
				<span key={id} className={markupStyles.mention} style={style}>
					@{role.name}
				</span>
			);
		}

		case MentionKind.Channel: {
			const unknownMention = (
				<span key={id} className={markupStyles.mention}>
					{ChannelUtils.getIcon({type: ChannelTypes.GUILD_TEXT}, {className: mentionRendererStyles.channelIcon})}
					{i18n._(msg`unknown-channel`)}
				</span>
			);

			const channel = ChannelStore.getChannel(kind.id);
			if (!channel) {
				return unknownMention;
			}

			if (channel.type === ChannelTypes.GUILD_CATEGORY) {
				return <span key={id}>#{channel.name}</span>;
			}

			if (
				channel.type !== ChannelTypes.GUILD_TEXT &&
				channel.type !== ChannelTypes.GUILD_VOICE &&
				channel.type !== ChannelTypes.GUILD_LINK
			) {
				return unknownMention;
			}

			return (
				<FocusRing key={id}>
					<button
						className={clsx(markupStyles.mention, markupStyles.interactive)}
						onClick={(e) => {
							e.stopPropagation();
							if (channel.type === ChannelTypes.GUILD_VOICE) {
								const canConnect = PermissionStore.can(Permissions.CONNECT, channel);
								if (!canConnect) {
									ModalActionCreators.push(
										modal(() => (
											<GenericErrorModal
												title={i18n._(msg`Missing Permissions`)}
												message={i18n._(msg`You don't have permission to connect to this voice channel.`)}
											/>
										)),
									);
									return;
								}
							}
							RouterUtils.transitionTo(Routes.guildChannel(channel.guildId!, channel.id));
						}}
						onContextMenu={(event) => {
							event.preventDefault();
							event.stopPropagation();
							ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
								<ChannelContextMenu channel={channel} onClose={onClose} />
							));
						}}
						type="button"
					>
						{ChannelUtils.getIcon(channel, {className: mentionRendererStyles.channelIcon})}
						{channel.name}
					</button>
				</FocusRing>
			);
		}

		case MentionKind.Everyone: {
			return (
				<span key={id} className={clsx(markupStyles.mention, mentionRendererStyles.everyoneMention)}>
					@everyone
				</span>
			);
		}

		case MentionKind.Here: {
			return (
				<span key={id} className={clsx(markupStyles.mention, mentionRendererStyles.hereMention)}>
					@here
				</span>
			);
		}

		case MentionKind.Command: {
			const {name} = kind;
			return (
				<span key={id} className={markupStyles.mention}>
					{name}
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
					const linkedRolesId = (kind as {navigationType: typeof GuildNavKind.LinkedRoles; id?: string}).id;
					content = linkedRolesId ? `<id:linked-roles:${linkedRolesId}>` : '<id:linked-roles>';
					break;
				}

				default:
					content = `<id:${navigationType}>`;
					break;
			}

			return (
				<span key={id} className={markupStyles.mention}>
					{content}
				</span>
			);
		}

		default:
			return <span key={id}>{'<unknown-mention>'}</span>;
	}
});
