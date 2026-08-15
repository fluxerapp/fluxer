// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import Permission from '@app/features/permissions/state/Permission';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';

export function canAttachFilesInChannel(channel: Channel): boolean {
	return (
		!DeveloperOptions.forceNoAttachFiles && (channel.isPrivate() || Permission.can(Permissions.ATTACH_FILES, channel))
	);
}
