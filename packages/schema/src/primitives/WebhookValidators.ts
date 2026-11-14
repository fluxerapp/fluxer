// SPDX-License-Identifier: AGPL-3.0-or-later

import {createInt32EnumType} from '@fluxer/schema/src/primitives/SchemaPrimitives';

export const WebhookTypeSchema = createInt32EnumType(
	[
		[1, 'INCOMING', 'Incoming webhook'],
		[2, 'CHANNEL_FOLLOWER', 'Channel follower webhook'],
	],
	'The type of webhook',
	'WebhookType',
);
