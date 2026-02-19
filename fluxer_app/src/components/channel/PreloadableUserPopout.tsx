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
import * as UserProfileActionCreators from '@app/actions/UserProfileActionCreators';
import {LongPressable} from '@app/components/LongPressable';
import {GuildMemberActionsSheet} from '@app/components/modals/guild_tabs/GuildMemberActionsSheet';
import {UserProfilePopout} from '@app/components/popouts/UserProfilePopout';
import {GuildMemberContextMenu} from '@app/components/uikit/context_menu/GuildMemberContextMenu';
import {UserContextMenu} from '@app/components/uikit/context_menu/UserContextMenu';
import {WebhookContextMenu} from '@app/components/uikit/context_menu/WebhookContextMenu';
import type {PopoutPosition} from '@app/components/uikit/popout';
import {Popout} from '@app/components/uikit/popout/Popout';
import type {UserRecord} from '@app/records/UserRecord';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import React, {useCallback, useState} from 'react';

type PreloadableChildProps = React.HTMLAttributes<HTMLElement> & React.RefAttributes<HTMLElement>;

export const PreloadableUserPopout = React.forwardRef<
	HTMLElement,
	{
		user: UserRecord;
		isWebhook: boolean;
		webhookId?: string;
		guildId?: string;
		channelId?: string;
		children: React.ReactNode;
		position?: PopoutPosition;
		tooltip?: string | (() => React.ReactNode);
		disableContextMenu?: boolean;
		disableBackdrop?: boolean;
		onPopoutOpen?: () => void;
		onPopoutClose?: () => void;
		enableLongPressActions?: boolean;
	}
>(
	(
		{
			user,
			isWebhook,
			webhookId,
			guildId,
			channelId,
			children,
			position = 'right-start',
			tooltip,
			disableContextMenu = false,
			disableBackdrop = false,
			onPopoutOpen,
			onPopoutClose,
			enableLongPressActions = false,
		},
		ref,
	) => {
		const mobileLayout = MobileLayoutStore;
		const [showActionsSheet, setShowActionsSheet] = useState(false);
		const child = React.Children.only(children) as React.ReactElement<PreloadableChildProps>;

		const member = guildId ? GuildMemberStore.getMember(guildId, user.id) : null;

		const handleMobileClick = useCallback(() => {
			if (isWebhook) return;

			UserProfileActionCreators.openUserProfile(user.id, guildId);
		}, [user.id, guildId, isWebhook]);

		const handleWebhookContextMenu = useCallback(
			(event: React.MouseEvent<Element>) => {
				if (!webhookId) return;
				event.preventDefault();
				event.stopPropagation();

				ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
					<WebhookContextMenu webhookId={webhookId} onClose={onClose} />
				));
			},
			[webhookId],
		);

		const handleContextMenu = useCallback(
			(event: React.MouseEvent<Element>) => {
				if (isWebhook) {
					handleWebhookContextMenu(event);
					return;
				}

				event.preventDefault();
				event.stopPropagation();

				const isGuildMember = guildId ? GuildMemberStore.getMember(guildId, user.id) : null;

				ContextMenuActionCreators.openFromEvent(event, ({onClose}) =>
					guildId && isGuildMember ? (
						<GuildMemberContextMenu user={user} onClose={onClose} guildId={guildId} channelId={channelId} />
					) : (
						<UserContextMenu user={user} onClose={onClose} guildId={guildId} channelId={channelId} />
					),
				);
			},
			[user, guildId, channelId, isWebhook, handleWebhookContextMenu],
		);

		const handleLongPress = useCallback(() => {
			if (isWebhook) return;
			setShowActionsSheet(true);
		}, [isWebhook]);

		const handleCloseActionsSheet = useCallback(() => {
			setShowActionsSheet(false);
		}, []);

		if (isWebhook) {
			const {onContextMenu: originalOnContextMenu} = child.props;
			return React.cloneElement(child, {
				ref,
				onContextMenu: (event: React.MouseEvent<HTMLElement>) => {
					if (originalOnContextMenu) {
						(originalOnContextMenu as React.MouseEventHandler<HTMLElement>)(event);
					}
					if (!disableContextMenu) {
						handleWebhookContextMenu(event);
					}
				},
			});
		}

		if (mobileLayout.enabled) {
			const {onClick: originalOnClick, onContextMenu: originalOnContextMenu} = child.props;

			const clonedChild = React.cloneElement(child, {
				ref,
				onClick: (event: React.MouseEvent<HTMLElement>) => {
					if (originalOnClick) {
						(originalOnClick as React.MouseEventHandler<HTMLElement>)(event);
					}
					handleMobileClick();
				},
				onContextMenu: (event: React.MouseEvent<HTMLElement>) => {
					if (originalOnContextMenu) {
						(originalOnContextMenu as React.MouseEventHandler<HTMLElement>)(event);
					}
					if (!disableContextMenu) {
						handleContextMenu(event);
					}
				},
			});

			if (enableLongPressActions && member) {
				return (
					<>
						<LongPressable onLongPress={handleLongPress} delay={500}>
							{clonedChild}
						</LongPressable>
						{showActionsSheet && guildId && (
							<GuildMemberActionsSheet
								isOpen={true}
								onClose={handleCloseActionsSheet}
								user={user}
								member={member}
								guildId={guildId}
							/>
						)}
					</>
				);
			}

			return clonedChild;
		}

		return (
			<Popout
				ref={ref}
				render={({popoutKey}) => (
					<UserProfilePopout
						key={`${user.id}:${guildId ?? 'global'}:${isWebhook ? 'webhook' : 'user'}`}
						popoutKey={popoutKey}
						user={user}
						isWebhook={isWebhook}
						guildId={guildId}
					/>
				)}
				position={position}
				tooltip={tooltip}
				disableBackdrop={disableBackdrop}
				onOpen={onPopoutOpen}
				onClose={onPopoutClose}
			>
				{disableContextMenu
					? children
					: React.cloneElement(child, {
							onContextMenu: handleContextMenu,
						})}
			</Popout>
		);
	},
);
