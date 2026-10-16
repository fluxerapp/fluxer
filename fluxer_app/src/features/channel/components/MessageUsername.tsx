// SPDX-License-Identifier: AGPL-3.0-or-later

import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {CrosspostCommunityPopout} from '@app/features/channel/components/CrosspostCommunityPopout';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import type {Guild} from '@app/features/guild/models/Guild';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import styles from '@app/features/theme/styles/Message.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';

export const MessageUsername = observer(
	({
		user,
		message,
		guild,
		member,
		className,
		previewColor,
		previewName,
	}: {
		user: User;
		message: Message;
		guild?: Guild;
		member?: GuildMember;
		className: string;
		isPreview: boolean;
		previewColor?: string;
		previewName?: string;
	}) => {
		const usernameRef = useRef<HTMLSpanElement | null>(null);
		const contextMenuOpen = useContextMenuHoverState(usernameRef);
		const displayName = previewName || NicknameUtils.getNickname(user, guild?.id, message.channelId);
		const color = previewColor || member?.getColorString();
		const onPopoutToggle = useMaybeMessageViewContext()?.onPopoutToggle;
		const handlePopoutOpen = useCallback(() => onPopoutToggle?.(true), [onPopoutToggle]);
		const handlePopoutClose = useCallback(() => onPopoutToggle?.(false), [onPopoutToggle]);
		const handleKeyDown = useCallback((e: React.KeyboardEvent) => {
			if (e.defaultPrevented) return;
			if (!isKeyboardActivationKey(e.key)) return;
			e.preventDefault();
			(e.currentTarget as HTMLElement).click();
		}, []);
		const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
		const usernameNode = (
			<FocusRing data-flx="channel.message-username.focus-ring">
				{/* biome-ignore lint/a11y/noStaticElementInteractions: the username span is only keyboard-interactive in keyboard mode (role="button"/tabIndex set conditionally); pointer/popout/context-menu interactions are handled by the wrapping PreloadableUserPopout. */}
				<span
					className={clsx(className, contextMenuOpen && styles.contextMenuUnderline)}
					style={{color}}
					data-user-id={user.id}
					data-guild-id={guild?.id}
					tabIndex={keyboardModeEnabled ? 0 : undefined}
					role={keyboardModeEnabled ? 'button' : undefined}
					ref={usernameRef}
					onKeyDown={handleKeyDown}
					data-flx="channel.message-username.context-menu-underline.key-down"
				>
					{displayName}
				</span>
			</FocusRing>
		);
		if (message.isCrosspostCopy) {
			return (
				<CrosspostCommunityPopout
					message={message}
					onPopoutOpen={handlePopoutOpen}
					onPopoutClose={handlePopoutClose}
					data-flx="channel.message-username.crosspost-community-popout"
				>
					{usernameNode}
				</CrosspostCommunityPopout>
			);
		}
		return (
			<PreloadableUserPopout
				user={user}
				isWebhook={message.webhookId != null}
				webhookId={message.webhookId ?? undefined}
				guildId={guild?.id}
				guildMember={member}
				channelId={message.channelId}
				message={message}
				enableLongPressActions={true}
				longPressWrapperElement="span"
				onPopoutOpen={handlePopoutOpen}
				onPopoutClose={handlePopoutClose}
				data-flx="channel.message-username.preloadable-user-popout"
			>
				{usernameNode}
			</PreloadableUserPopout>
		);
	},
);
