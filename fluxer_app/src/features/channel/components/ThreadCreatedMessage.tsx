// SPDX-License-Identifier: AGPL-3.0-or-later

import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import {ThreadsPopout} from '@app/features/channel/components/popouts/ThreadsPopout';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {ThreadIcon} from '@app/features/ui/components/icons/ThreadIcon';
import styles from '@app/features/theme/styles/Message.module.css';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useRef} from 'react';

const ThreadSystemIcon = ({className}: {className?: string}) => (
	<ThreadIcon size={16} className={className} />
);

interface ThreadCreatedMessageProps {
	message: Message;
}

export const ThreadCreatedMessage = observer(({message}: ThreadCreatedMessageProps) => {
	const {author, channel, guild} = useSystemMessageData(message);
	const linkRef = useRef<HTMLButtonElement>(null);

	const handleThreadsLink = useCallback(() => {
		if (!linkRef.current || !channel) return;
		PopoutCommands.open({
			key: `thread-created-threads-${channel.id}`,
			target: linkRef.current,
			position: 'bottom-start',
			render: ({onClose}) => (
				<ThreadsPopout
					channelId={channel.id}
					guildId={channel.guildId ?? ''}
					onClose={onClose}
				/>
			),
		});
	}, [channel]);

	if (!channel) return null;

	const threadName = message.content;
	const threadsLink = (
		<button
			ref={linkRef}
			type="button"
			className={styles.systemMessageLink}
			onClick={handleThreadsLink}
			data-flx="channel.thread-created-message.threads-link"
		>
			threads
		</button>
	);

	const messageContent = threadName ? (
		<Trans>
			<SystemMessageUsername
				key={author.id}
				author={author}
				guild={guild}
				message={message}
				data-flx="channel.thread-created-message.system-message-username"
			/>{' '}
			started a thread: <strong>{threadName}</strong>. See all {threadsLink}.
		</Trans>
	) : (
		<Trans>
			<SystemMessageUsername
				key={author.id}
				author={author}
				guild={guild}
				message={message}
				data-flx="channel.thread-created-message.system-message-username--2"
			/>{' '}
			started a thread. See all {threadsLink}.
		</Trans>
	);

	return (
		<SystemMessage
			icon={ThreadSystemIcon}
			iconWeight="bold"
			message={message}
			messageContent={messageContent}
			data-flx="channel.thread-created-message.system-message"
		/>
	);
});
