// SPDX-License-Identifier: AGPL-3.0-or-later

import {LexicalChannelTextareaContent} from '@app/features/channel/components/LexicalChannelTextareaContent';
import type {Channel} from '@app/features/channel/models/Channel';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Drafts from '@app/features/messaging/state/MessagingDrafts';
import Permission from '@app/features/permissions/state/Permission';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {observer} from 'mobx-react-lite';

interface ChannelTextareaProps {
	readonly channel: Channel;
	readonly inputSuppressed?: boolean;
}

export const ChannelTextarea = observer(({channel, inputSuppressed = false}: ChannelTextareaProps) => {
	const draft = Drafts.getDraft(channel.id);
	const draftSegments = Drafts.getDraftSegments(channel.id);
	const forceNoSendMessages = DeveloperOptions.forceNoSendMessages;
	let disabled = false;
	if (channel.isPrivate()) {
		disabled = forceNoSendMessages;
	} else if (forceNoSendMessages || !Permission.can(Permissions.SEND_MESSAGES, channel)) {
		disabled = true;
	} else {
		disabled = GuildMembers.isUserTimedOut(
			channel.guildId == null ? null : channel.guildId,
			Users.currentUser == null ? null : Users.currentUser.id,
		);
	}
	return (
		<LexicalChannelTextareaContent
			key={channel.id}
			channel={channel}
			draft={draft}
			draftSegments={draftSegments}
			disabled={disabled}
			inputSuppressed={inputSuppressed}
			data-flx="channel.channel-textarea.lexical-channel-textarea-content"
		/>
	);
});
