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

import {Trans} from '@lingui/react/macro';
import {PushPinIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {SystemMessage} from '~/components/channel/SystemMessage';
import {SystemMessageUsername} from '~/components/channel/SystemMessageUsername';
import {useSystemMessageData} from '~/hooks/useSystemMessageData';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {MessageRecord} from '~/records/MessageRecord';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import styles from '~/styles/Message.module.css';
import {goToMessage} from '~/utils/MessageNavigator';

export const PinSystemMessage = observer(({message}: {message: MessageRecord}) => {
	const {author, channel, guild} = useSystemMessageData(message);
	const mobileLayout = MobileLayoutStore;

	const jumpToMessage = React.useCallback(() => {
		if (message.messageReference?.message_id) {
			goToMessage(message.channelId, message.messageReference.message_id);
		}
	}, [message.channelId, message.messageReference?.message_id]);

	const openPins = React.useCallback(() => {
		if (mobileLayout.enabled) {
			ComponentDispatch.dispatch('CHANNEL_DETAILS_OPEN', {
				initialTab: 'pins',
			});
		} else {
			ComponentDispatch.dispatch('CHANNEL_PINS_OPEN');
		}
	}, [mobileLayout.enabled]);

	if (!channel) {
		return null;
	}

	const messageContent = (
		<Trans>
			<SystemMessageUsername key={author.id} author={author} guild={guild} message={message} /> pinned{' '}
			<button key={`pin-${message.id}`} type="button" className={styles.systemMessageLink} onClick={jumpToMessage}>
				a message
			</button>{' '}
			to this channel. See{' '}
			<button key={`pin-all-${message.id}`} type="button" className={styles.systemMessageLink} onClick={openPins}>
				all pinned messages
			</button>
			.
		</Trans>
	);

	return <SystemMessage icon={PushPinIcon} iconWeight="fill" message={message} messageContent={messageContent} />;
});
