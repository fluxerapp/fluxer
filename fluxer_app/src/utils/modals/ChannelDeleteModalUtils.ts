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

import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ChannelTypes} from '~/Constants';
import ChannelStore from '~/stores/ChannelStore';

export interface ChannelDeleteModalProps {
	channelId: string;
}

export const deleteChannel = async (channelId: string): Promise<void> => {
	const channel = ChannelStore.getChannel(channelId);
	if (!channel) return;

	await ChannelActionCreators.remove(channelId);
	ModalActionCreators.popAll();

	ToastActionCreators.createToast({
		type: 'success',
		children: channel.type === ChannelTypes.GUILD_CATEGORY ? 'Category deleted' : 'Channel deleted',
	});
};

export const getChannelDeleteInfo = (channelId: string) => {
	const channel = ChannelStore.getChannel(channelId);
	if (!channel) return null;

	const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
	const title = isCategory ? 'Delete category' : 'Delete channel';
	const confirmText = isCategory ? 'Delete category' : 'Delete channel';
	const successMessage = isCategory ? 'Category deleted' : 'Channel deleted';

	return {
		channel,
		isCategory,
		title,
		confirmText,
		successMessage,
	};
};
