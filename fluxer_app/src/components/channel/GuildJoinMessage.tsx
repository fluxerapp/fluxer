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

import {useLingui} from '@lingui/react/macro';
import {ArrowRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {SystemMessage} from '~/components/channel/SystemMessage';
import {SystemMessageUsername} from '~/components/channel/SystemMessageUsername';
import {useSystemMessageData} from '~/hooks/useSystemMessageData';
import type {MessageRecord} from '~/records/MessageRecord';
import {SystemMessageUtils} from '~/utils/SystemMessageUtils';
import styles from './GuildJoinMessage.module.css';

export const GuildJoinMessage = observer(({message}: {message: MessageRecord}) => {
	const {i18n} = useLingui();
	const {author, channel, guild} = useSystemMessageData(message);

	if (!channel) {
		return null;
	}

	const messageContent = SystemMessageUtils.getGuildJoinMessage(
		message.id,
		<SystemMessageUsername author={author} guild={guild} message={message} key={author.id} />,
		i18n,
	);
	return (
		<SystemMessage
			icon={ArrowRightIcon}
			iconWeight="bold"
			iconClassname={styles.icon}
			message={message}
			messageContent={messageContent}
		/>
	);
});
