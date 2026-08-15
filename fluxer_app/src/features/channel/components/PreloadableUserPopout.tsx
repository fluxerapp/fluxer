// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import Authentication from '@app/features/auth/state/Authentication';
import {GuildMemberActionsSheet} from '@app/features/guild/components/modals/guild_tabs/GuildMemberActionsSheet';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {GuildMemberContextMenu} from '@app/features/ui/action_menu/GuildMemberContextMenu';
import {UserContextMenu} from '@app/features/ui/action_menu/UserContextMenu';
import {WebhookContextMenu} from '@app/features/ui/action_menu/WebhookContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import type {PopoutAnimationType, PopoutPosition} from '@app/features/ui/popover';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {UserProfileActionsSheet} from '@app/features/user/components/modals/UserProfileActionsSheet';
import {UserProfilePopout} from '@app/features/user/components/popouts/UserProfilePopout';
import {useUserProfileHoverPreload} from '@app/features/user/hooks/useUserProfileHoverPreload';
import type {User} from '@app/features/user/models/User';
import React, {useCallback, useState} from 'react';

type PreloadableChildProps = React.HTMLAttributes<HTMLElement> & React.RefAttributes<HTMLElement>;

export const PreloadableUserPopout = React.forwardRef<
	HTMLElement,
	{
		user: User;
		isWebhook: boolean;
		webhookId?: string;
		guildId?: string;
		guildMember?: GuildMember;
		channelId?: string;
		message?: Message;
		children: React.ReactNode;
		position?: PopoutPosition;
		tooltip?: string | (() => React.ReactNode);
		disableContextMenu?: boolean;
		disableBackdrop?: boolean;
		onPopoutOpen?: () => void;
		onPopoutClose?: () => void;
		enableLongPressActions?: boolean;
		longPressWrapperElement?: 'div' | 'span';
		profilePopoutAnimationType?: PopoutAnimationType;
	}
>(
	(
		{
			user,
			isWebhook,
			webhookId,
			guildId,
			guildMember,
			channelId,
			message,
			children,
			position = 'right-start',
			tooltip,
			disableContextMenu = false,
			disableBackdrop = false,
			onPopoutOpen,
			onPopoutClose,
			enableLongPressActions = false,
			longPressWrapperElement = 'div',
			profilePopoutAnimationType = 'profile-slide',
		},
		ref,
	) => {
		const mobileLayout = MobileLayout;
		const [showActionsSheet, setShowActionsSheet] = useState(false);
		const child = React.Children.only(children) as React.ReactElement<PreloadableChildProps>;
		const member = guildMember ?? (guildId ? GuildMembers.getMember(guildId, user.id) : null);
		const {scheduleProfilePreload, cancelProfilePreload} = useUserProfileHoverPreload({
			userId: user.id,
			guildId,
			enabled: !isWebhook && !mobileLayout.enabled,
		});
		const handleMobileClick = useCallback(() => {
			if (isWebhook) return;
			UserProfileCommands.openUserProfile(user.id, guildId);
		}, [user.id, guildId, isWebhook]);
		const isShiftMentionClick = useCallback(
			(event: React.MouseEvent<HTMLElement>) =>
				!isWebhook && event.shiftKey && !event.metaKey && !event.ctrlKey && !event.altKey && event.button === 0,
			[isWebhook],
		);
		const shouldOpenOnClick = useCallback(
			(event: React.MouseEvent<HTMLElement>) => !isShiftMentionClick(event),
			[isShiftMentionClick],
		);
		const handleWebhookContextMenu = useCallback(
			(event: React.MouseEvent<Element>) => {
				if (!webhookId) return;
				event.preventDefault();
				event.stopPropagation();
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<WebhookContextMenu
						webhookId={webhookId}
						onClose={onClose}
						data-flx="channel.preloadable-user-popout.handle-webhook-context-menu.webhook-context-menu"
					/>
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
				ContextMenuCommands.openFromEvent(event, ({onClose}) =>
					guildId ? (
						<GuildMemberContextMenu
							user={user}
							onClose={onClose}
							guildId={guildId}
							channelId={channelId}
							member={member ?? undefined}
							message={message}
							data-flx="channel.preloadable-user-popout.handle-context-menu.guild-member-context-menu"
						/>
					) : (
						<UserContextMenu
							user={user}
							onClose={onClose}
							guildId={guildId}
							channelId={channelId}
							message={message}
							data-flx="channel.preloadable-user-popout.handle-context-menu.user-context-menu"
						/>
					),
				);
			},
			[user, guildId, channelId, member, isWebhook, handleWebhookContextMenu, message],
		);
		const handleLongPress = useCallback(() => {
			if (isWebhook) return;
			setShowActionsSheet(true);
		}, [isWebhook]);
		const handleCloseActionsSheet = useCallback(() => {
			setShowActionsSheet(false);
		}, []);
		if (mobileLayout.enabled) {
			const {onClick: originalOnClick, onContextMenu: originalOnContextMenu} = child.props;
			const clonedChild = React.cloneElement(child, {
				ref,
				onClick: (event: React.MouseEvent<HTMLElement>) => {
					if (originalOnClick) {
						(originalOnClick as React.MouseEventHandler<HTMLElement>)(event);
					}
					if (isShiftMentionClick(event)) {
						event.preventDefault();
						event.stopPropagation();
						ComponentDispatch.dispatch('INSERT_MENTION', {userId: user.id});
						return;
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
			if (enableLongPressActions) {
				return (
					<>
						<LongPressable
							as={longPressWrapperElement}
							onLongPress={handleLongPress}
							delay={500}
							data-flx="channel.preloadable-user-popout.long-pressable"
						>
							{clonedChild}
						</LongPressable>
						{showActionsSheet &&
							(guildId && member ? (
								<GuildMemberActionsSheet
									isOpen={true}
									onClose={handleCloseActionsSheet}
									user={user}
									member={member}
									guildId={guildId}
									message={message}
									data-flx="channel.preloadable-user-popout.guild-member-actions-sheet"
								/>
							) : (
								<UserProfileActionsSheet
									isOpen={true}
									onClose={handleCloseActionsSheet}
									user={user}
									isCurrentUser={user.id === Authentication.currentUserId}
									guildId={guildId}
									guildMember={member}
									message={message}
									data-flx="channel.preloadable-user-popout.user-profile-actions-sheet"
								/>
							))}
					</>
				);
			}
			return clonedChild;
		}
		const {
			onClick: originalOnClick,
			onMouseEnter: originalOnMouseEnter,
			onMouseLeave: originalOnMouseLeave,
		} = child.props;
		const desktopOnClick = (event: React.MouseEvent<HTMLElement>) => {
			if (isShiftMentionClick(event)) {
				event.preventDefault();
				event.stopPropagation();
				ComponentDispatch.dispatch('INSERT_MENTION', {userId: user.id});
				return;
			}
			if (originalOnClick) {
				(originalOnClick as React.MouseEventHandler<HTMLElement>)(event);
			}
		};
		const desktopOnMouseEnter = (event: React.MouseEvent<HTMLElement>) => {
			scheduleProfilePreload();
			originalOnMouseEnter?.(event);
		};
		const desktopOnMouseLeave = (event: React.MouseEvent<HTMLElement>) => {
			cancelProfilePreload();
			originalOnMouseLeave?.(event);
		};
		const desktopChildProps: React.HTMLAttributes<HTMLElement> = {
			onClick: desktopOnClick,
			onMouseEnter: desktopOnMouseEnter,
			onMouseLeave: desktopOnMouseLeave,
			...(!disableContextMenu ? {onContextMenu: handleContextMenu} : {}),
		};
		return (
			<Popout
				ref={ref}
				render={({popoutKey, onClose}) => (
					<UserProfilePopout
						key={`${user.id}:${guildId ?? 'global'}:${isWebhook ? 'webhook' : 'user'}`}
						popoutKey={popoutKey}
						user={user}
						isWebhook={isWebhook}
						guildId={guildId}
						guildMember={member}
						onClose={onClose}
						data-flx="channel.preloadable-user-popout.user-profile-popout"
					/>
				)}
				position={position}
				animationType={profilePopoutAnimationType}
				constrainHeight={false}
				freezePosition
				keepOpenOnTargetUnmount
				tooltip={tooltip}
				disableBackdrop={disableBackdrop}
				onOpen={onPopoutOpen}
				onClose={onPopoutClose}
				shouldOpenOnClick={shouldOpenOnClick}
				data-flx="channel.preloadable-user-popout.popout"
			>
				{React.cloneElement(child, desktopChildProps)}
			</Popout>
		);
	},
);
