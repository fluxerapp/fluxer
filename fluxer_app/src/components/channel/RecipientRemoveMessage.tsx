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
import {UserMinusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {SystemMessage} from '~/components/channel/SystemMessage';
import {SystemMessageUsername} from '~/components/channel/SystemMessageUsername';
import {useSystemMessageData} from '~/hooks/useSystemMessageData';
import type {MessageRecord} from '~/records/MessageRecord';
import UserStore from '~/stores/UserStore';
import styles from './RecipientRemoveMessage.module.css';

export const RecipientRemoveMessage = observer(({message}: {message: MessageRecord}) => {
	const {author, channel, guild} = useSystemMessageData(message);

	const removedUserId = message.mentions.length > 0 ? message.mentions[0].id : null;
	const removedUser = UserStore.getUser(removedUserId ?? '');

	if (!channel) {
		return null;
	}

	const isSelfRemove = removedUserId === author.id;

	const messageContent = isSelfRemove ? (
		<Trans>
			<SystemMessageUsername key={author.id} author={author} guild={guild} message={message} /> has left the group.
		</Trans>
	) : removedUser ? (
		<Trans>
			<SystemMessageUsername key={author.id} author={author} guild={guild} message={message} /> removed{' '}
			<SystemMessageUsername key={removedUser.id} author={removedUser} guild={guild} message={message} /> from the
			group.
		</Trans>
	) : (
		<Trans>
			<SystemMessageUsername key={author.id} author={author} guild={guild} message={message} /> removed someone from the
			group.
		</Trans>
	);

	return (
		<SystemMessage
			icon={UserMinusIcon}
			iconWeight="bold"
			iconClassname={styles.icon}
			message={message}
			messageContent={messageContent}
		/>
	);
});
