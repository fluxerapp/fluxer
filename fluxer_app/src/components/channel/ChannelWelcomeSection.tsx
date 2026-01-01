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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {ChannelTypes} from '~/Constants';
import styles from '~/components/channel/ChannelWelcomeSection.module.css';
import {DMWelcomeSection} from '~/components/channel/dm/DMWelcomeSection';
import {GroupDMWelcomeSection} from '~/components/channel/dm/GroupDMWelcomeSection';
import {PersonalNotesWelcomeSection} from '~/components/channel/dm/PersonalNotesWelcomeSection';
import type {ChannelRecord} from '~/records/ChannelRecord';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';

export const ChannelWelcomeSection = observer(({channel}: {channel: ChannelRecord}) => {
	const recipient = UserStore.getUser(channel.recipientIds[0]);

	if (channel.type === ChannelTypes.DM && recipient) {
		return <DMWelcomeSection userId={recipient.id} />;
	}

	if (channel.type === ChannelTypes.DM_PERSONAL_NOTES && recipient) {
		return <PersonalNotesWelcomeSection userId={recipient.id} />;
	}

	if (channel.type === ChannelTypes.GROUP_DM) {
		return <GroupDMWelcomeSection channel={channel} />;
	}

	return (
		<div className={styles.container}>
			<div className={clsx('pointer-events-none', styles.channelIcon)}>
				{ChannelUtils.getIcon(channel, {className: styles.iconSize})}
			</div>
			<h1 className={styles.heading}>
				<Trans>Welcome to #{channel.name ?? ''}</Trans>
			</h1>
			<p className={styles.description}>
				<Trans>In the beginning, there was nothing. Then, there was #{channel.name ?? ''}. And it was good.</Trans>
			</p>
		</div>
	);
});
