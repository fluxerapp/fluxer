// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageFlags, SENDABLE_MESSAGE_FLAGS} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

describe('announcement channel constants', () => {
	it('keeps the crosspost flags out of the sendable flags', () => {
		const crosspostFlags = MessageFlags.CROSSPOSTED | MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED;
		expect(crosspostFlags & SENDABLE_MESSAGE_FLAGS).toBe(0);
	});
});
