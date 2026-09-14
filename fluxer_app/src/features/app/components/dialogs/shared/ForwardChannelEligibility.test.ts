// SPDX-License-Identifier: AGPL-3.0-or-later

import {isForwardableChannelType} from '@app/features/app/components/dialogs/shared/ForwardChannelEligibility';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

describe('forward channel eligibility', () => {
	it('allows every text-based message destination, including text-in-voice channels', () => {
		expect(isForwardableChannelType(ChannelTypes.GUILD_TEXT)).toBe(true);
		expect(isForwardableChannelType(ChannelTypes.GUILD_VOICE)).toBe(true);
		expect(isForwardableChannelType(ChannelTypes.DM)).toBe(true);
		expect(isForwardableChannelType(ChannelTypes.GROUP_DM)).toBe(true);
		expect(isForwardableChannelType(ChannelTypes.DM_PERSONAL_NOTES)).toBe(true);
	});
	it('rejects non-message destinations', () => {
		expect(isForwardableChannelType(ChannelTypes.GUILD_CATEGORY)).toBe(false);
		expect(isForwardableChannelType(ChannelTypes.GUILD_LINK)).toBe(false);
		expect(isForwardableChannelType(-1)).toBe(false);
	});
});
