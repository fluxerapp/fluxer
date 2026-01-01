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

import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {LongPressable} from '~/components/LongPressable';
import {GuildMemberActionsSheet} from '~/components/modals/guildTabs/GuildMemberActionsSheet';
import {UserProfilePopout} from '~/components/popouts/UserProfilePopout';
import {GuildMemberContextMenu} from '~/components/uikit/ContextMenu/GuildMemberContextMenu';
import {UserContextMenu} from '~/components/uikit/ContextMenu/UserContextMenu';
import type {PopoutPosition} from '~/components/uikit/Popout';
import {Popout} from '~/components/uikit/Popout/Popout';
import type {UserRecord} from '~/records/UserRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';

type PreloadableChildProps = React.HTMLAttributes<HTMLElement> & React.RefAttributes<HTMLElement>;

export const PreloadableUserPopout = React.forwardRef<
	HTMLElement,
	{
		user: UserRecord;
		isWebhook: boolean;
		guildId?: string;
		channelId?: string;
		children: React.ReactNode;
		position?: PopoutPosition;
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
			guildId,
			channelId,
			children,
			position = 'right-start',
			disableContextMenu = false,
			disableBackdrop = false,
			onPopoutOpen,
			onPopoutClose,
			enableLongPressActions = false,
		},
		ref,
	) => {
		const mobileLayout = MobileLayoutStore;
		const [showActionsSheet, setShowActionsSheet] = React.useState(false);

		const member = guildId ? GuildMemberStore.getMember(guildId, user.id) : null;

		const handleMobileClick = React.useCallback(() => {
			if (isWebhook) return;

			UserProfileActionCreators.openUserProfile(user.id, guildId);
		}, [user.id, guildId, isWebhook]);

		const handleContextMenu = React.useCallback(
			(event: React.MouseEvent<Element>) => {
				if (isWebhook) return;

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
			[user, guildId, channelId, isWebhook],
		);

		const handleLongPress = React.useCallback(() => {
			if (isWebhook) return;
			setShowActionsSheet(true);
		}, [isWebhook]);

		const handleCloseActionsSheet = React.useCallback(() => {
			setShowActionsSheet(false);
		}, []);

		if (mobileLayout.enabled) {
			const child = React.Children.only(children) as React.ReactElement<PreloadableChildProps>;
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
					<UserProfilePopout popoutKey={popoutKey} user={user} isWebhook={isWebhook} guildId={guildId} />
				)}
				position={position}
				disableBackdrop={disableBackdrop}
				onOpen={onPopoutOpen}
				onClose={onPopoutClose}
			>
				{disableContextMenu
					? children
					: React.cloneElement(React.Children.only(children) as React.ReactElement<PreloadableChildProps>, {
							onContextMenu: handleContextMenu,
						})}
			</Popout>
		);
	},
);
