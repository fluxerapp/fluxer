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

import * as NavigationActionCreators from '@app/actions/NavigationActionCreators';
import {DMChannelView} from '@app/components/channel/channel_view/DMChannelView';
import {GuildChannelView} from '@app/components/channel/channel_view/GuildChannelView';
import {useLocation, useParams} from '@app/lib/router/React';
import ChannelStore from '@app/stores/ChannelStore';
import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

export const ChannelIndexPage = observer(() => {
	const location = useLocation();
	const {
		guildId: routeGuildId,
		channelId,
		messageId,
	} = useParams() as {
		guildId?: string;
		channelId?: string;
		messageId?: string;
	};

	const channel = channelId ? ChannelStore.getChannel(channelId) : undefined;
	const isInFavorites = location.pathname.startsWith('/channels/@favorites');
	const derivedGuildId = isInFavorites ? channel?.guildId : routeGuildId || channel?.guildId;

	useEffect(() => {
		if (!channelId) {
			return;
		}

		if (!channel) {
			return;
		}

		if (channel.type !== ChannelTypes.GUILD_CATEGORY && channel.type !== ChannelTypes.GUILD_LINK) {
			return;
		}

		const fallbackGuildId = routeGuildId ?? (isInFavorites ? FAVORITES_GUILD_ID : undefined);
		if (!fallbackGuildId) {
			return;
		}

		NavigationActionCreators.selectChannel(fallbackGuildId, undefined, undefined, 'replace');
	}, [channelId, channel, routeGuildId, isInFavorites]);

	if (!channelId) {
		return null;
	}

	if (channel && (channel.type === ChannelTypes.GUILD_CATEGORY || channel.type === ChannelTypes.GUILD_LINK)) {
		return null;
	}

	if (channel?.isPrivate()) {
		return <DMChannelView channelId={channelId} />;
	}

	return <GuildChannelView channelId={channelId} guildId={derivedGuildId} messageId={messageId} />;
});
