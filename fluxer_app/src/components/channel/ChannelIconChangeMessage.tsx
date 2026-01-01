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
import {ImageSquareIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {SystemMessage} from '~/components/channel/SystemMessage';
import {SystemMessageUsername} from '~/components/channel/SystemMessageUsername';
import {useSystemMessageData} from '~/hooks/useSystemMessageData';
import type {MessageRecord} from '~/records/MessageRecord';

export const ChannelIconChangeMessage = observer(({message}: {message: MessageRecord}) => {
	const {author, channel, guild} = useSystemMessageData(message);

	if (!channel) {
		return null;
	}

	const messageContent = (
		<Trans>
			<SystemMessageUsername key={author.id} author={author} guild={guild} message={message} /> changed the channel
			icon.
		</Trans>
	);

	return <SystemMessage icon={ImageSquareIcon} iconWeight="bold" message={message} messageContent={messageContent} />;
});
