// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelChatLayout.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {createContext, useContext} from 'react';

const MESSAGES_DESCRIPTOR = msg({
	message: 'Messages',
	comment: 'Short label in the channel chat layout. Keep it concise.',
});
const MESSAGE_COMPOSER_DESCRIPTOR = msg({
	message: 'Message composer',
	comment: 'Short label in the channel chat layout. Keep it concise.',
});

interface ChannelChatLayoutProps {
	messages: React.ReactNode;
	textarea: React.ReactNode;
	hideBottomBar?: boolean;
}

const ChannelComposerAmbientStatusVisibilityContext = createContext(true);

export function useChannelComposerAmbientStatusVisibility(): boolean {
	return useContext(ChannelComposerAmbientStatusVisibilityContext);
}

export const ChannelChatLayout = observer(({messages, textarea, hideBottomBar = false}: ChannelChatLayoutProps) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.container} data-flx="channel.channel-chat-layout.container">
			<section
				className={styles.messagesArea}
				aria-label={i18n._(MESSAGES_DESCRIPTOR)}
				data-flx="channel.channel-chat-layout.messages-area"
			>
				{messages}
			</section>
			<section
				className={styles.textareaArea}
				aria-label={i18n._(MESSAGE_COMPOSER_DESCRIPTOR)}
				data-flx="channel.channel-chat-layout.textarea-area"
			>
				<ChannelComposerAmbientStatusVisibilityContext.Provider value={!hideBottomBar}>
					{textarea}
				</ChannelComposerAmbientStatusVisibilityContext.Provider>
			</section>
		</div>
	);
});
