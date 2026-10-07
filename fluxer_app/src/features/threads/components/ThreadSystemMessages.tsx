// SPDX-License-Identifier: AGPL-3.0-or-later

import {Divider} from '@app/features/channel/components/ChannelDivider';
import {Message as MessageComponent} from '@app/features/channel/components/ChannelMessage';
import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import Channels from '@app/features/channel/state/Channels';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageReferences, {MessageReferenceState} from '@app/features/messaging/state/MessageReferences';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import {openThread} from '@app/features/threads/commands/ThreadNavigation';
import styles from '@app/features/threads/components/ThreadSystemMessages.module.css';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {ChatsIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

export const ThreadCreatedMessage = observer(({message}: {message: Message}) => {
	const {i18n} = useLingui();
	const {author, channel, guild} = useSystemMessageData(message);
	const threadId = message.messageReference?.channel_id ?? null;
	const thread = threadId ? ChannelThreads.getThread(threadId) : undefined;
	const threadName = thread?.name ?? message.content;
	const handleOpen = useCallback(() => {
		if (thread) openThread(thread);
	}, [thread]);
	const handleSeeAll = useCallback(() => {
		ComponentBus.dispatch('THREAD_BROWSER_OPEN');
	}, []);
	if (!channel) return null;
	const messageContent = (
		<>
			<SystemMessageUsername
				key={author.id}
				author={author}
				guild={guild}
				message={message}
				data-flx="threads.thread-created-message.system-message-username"
			/>{' '}
			<span data-flx="threads.thread-created-message.started">{i18n._(D.STARTED_A_THREAD_DESCRIPTOR)}</span>{' '}
			{thread ? (
				<button
					type="button"
					className={messageStyles.systemMessageLink}
					onClick={handleOpen}
					data-flx="threads.thread-created-message.system-message-link.open"
				>
					{threadName}
				</button>
			) : (
				<span className={styles.threadName} data-flx="threads.thread-created-message.thread-name">
					{threadName}
				</span>
			)}
			<span data-flx="threads.thread-created-message.separator">{'. '}</span>
			<button
				type="button"
				className={messageStyles.systemMessageLink}
				onClick={handleSeeAll}
				data-flx="threads.thread-created-message.system-message-link.see-all"
			>
				{i18n._(D.SEE_ALL_THREADS_DESCRIPTOR)}
			</button>
		</>
	);
	return (
		<SystemMessage
			icon={ChatsIcon}
			iconWeight="bold"
			message={message}
			messageContent={messageContent}
			data-flx="threads.thread-created-message.system-message"
		/>
	);
});

export const ThreadStarterMessage = observer(({message}: {message: Message}) => {
	const {i18n} = useLingui();
	const reference = message.messageReference;
	const resolution = MessageReferences.getMessageReference(reference?.channel_id ?? '', reference?.message_id ?? '');
	const parent = reference?.channel_id ? Channels.getChannel(reference.channel_id) : undefined;
	const starter =
		resolution.state === MessageReferenceState.LOADED
			? resolution.message
			: resolution.state === MessageReferenceState.DELETED
				? null
				: message.referencedMessage;
	if (starter && parent) {
		return (
			<div className={styles.starter} data-flx="threads.thread-starter-message.starter">
				<MessageComponent
					message={starter}
					channel={parent}
					previewContext={MessagePreviewContext.LIST_POPOUT}
					data-flx="threads.thread-starter-message.message-component"
				/>
				<Divider spacing={8} data-flx="threads.thread-starter-message.divider" />
			</div>
		);
	}
	return (
		<div className={styles.starterDeleted} data-flx="threads.thread-starter-message.starter-deleted">
			<TrashIcon size={16} data-flx="threads.thread-starter-message.trash-icon" />
			<span data-flx="threads.thread-starter-message.text">{i18n._(D.STARTER_MESSAGE_DELETED_DESCRIPTOR)}</span>
		</div>
	);
});
