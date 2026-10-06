// SPDX-License-Identifier: AGPL-3.0-or-later

import {CrosspostCommunityPopout} from '@app/features/channel/components/CrosspostCommunityPopout';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import { makeFallbackPersona, Persona } from '@app/features/personas/models/Persona';
import {Avatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {User} from '@app/features/user/models/User';
import Personas from '@app/features/user/state/Personas';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

export const MessageAvatar = observer(
	({
		user,
		message,
		guildId,
		size,
		className,
		isHovering,
	}: {
		user: User;
		message: Message;
		guildId?: string;
		size: 16 | 24 | 32 | 40 | 48 | 80 | 120;
		className: string;
		isHovering: boolean;
		isPreview: boolean;
	}) => {
		const onPopoutToggle = useMaybeMessageViewContext()?.onPopoutToggle;
		const handlePopoutOpen = useCallback(() => onPopoutToggle?.(true), [onPopoutToggle]);
		const handlePopoutClose = useCallback(() => onPopoutToggle?.(false), [onPopoutToggle]);
		const storedPersona = message.persona?.id ? Personas.getPersona(message.persona.id) : null;
		if (message.isCrosspostCopy) {
			return (
				<CrosspostCommunityPopout
					message={message}
					onPopoutOpen={handlePopoutOpen}
					onPopoutClose={handlePopoutClose}
					data-flx="channel.message-avatar.crosspost-community-popout"
				>
					<FocusRing data-flx="channel.message-avatar.focus-ring--2">
						<Avatar
							user={user}
							size={size}
							className={className}
							forceAnimate={isHovering}
							guildId={guildId}
							data-user-id={user.id}
							data-guild-id={guildId}
							data-flx="channel.message-avatar.crosspost-author-avatar"
						/>
					</FocusRing>
				</CrosspostCommunityPopout>
			);
		}
		return (
			<PreloadableUserPopout
				user={user}
				persona={message.persona?.id ? makeFallbackPersona(message.persona) : undefined}
				isWebhook={message.webhookId != null}
				webhookId={message.webhookId ?? undefined}
				guildId={guildId}
				channelId={message.channelId}
				message={message}
				enableLongPressActions={false}
				onPopoutOpen={handlePopoutOpen}
				onPopoutClose={handlePopoutClose}
				data-flx="channel.message-avatar.preloadable-user-popout"
			>
				<FocusRing data-flx="channel.message-avatar.focus-ring">
					<Avatar
						user={user}
						size={size}
						className={className}
						forceAnimate={isHovering}
						guildId={guildId}
						personaId={message.persona?.id}
						personaAvatar={storedPersona?.avatar || message.persona?.avatar}
						data-user-id={user.id}
						data-guild-id={guildId}
						data-persona-id={message.persona?.id}
						data-flx="channel.message-avatar.avatar"
					/>
				</FocusRing>
			</PreloadableUserPopout>
		);
	},
);
