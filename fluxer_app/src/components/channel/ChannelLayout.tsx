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
import {SmileySadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useParams} from '~/lib/router';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import styles from './ChannelLayout.module.css';

export const ChannelLayout = observer(({children}: {children: React.ReactNode}) => {
	const {guildId: routeGuildId, channelId} = useParams() as {guildId?: string; channelId: string};
	const channel = ChannelStore.getChannel(channelId);
	const guildId = routeGuildId || channel?.guildId;
	const guild = guildId ? GuildStore.getGuild(guildId) : null;

	if (guild && !channel) {
		return (
			<div className={styles.channelNotFoundContainer}>
				<div className={styles.channelNotFoundContent}>
					<SmileySadIcon className={styles.channelNotFoundIcon} />
					<h1 className={styles.channelNotFoundTitle}>
						<Trans>This is not the channel you're looking for.</Trans>
					</h1>
					<p className={styles.channelNotFoundDescription}>
						<Trans>The channel you're looking for may have been deleted or you may not have access to it.</Trans>
					</p>
				</div>
			</div>
		);
	}

	return <div className={styles.channelLayoutContainer}>{children}</div>;
});
