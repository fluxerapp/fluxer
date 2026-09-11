// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageReferenceTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {MessageReferenceTypeSchema, MessageTypeSchema} from '@fluxer/schema/src/primitives/MessageValidators';
import {describe, expect, it} from 'vitest';

describe('MessageTypeSchema', () => {
	it.each(
		Object.values(MessageTypes).filter((value) => value !== MessageTypes.CLIENT_SYSTEM),
	)('accepts wire message type %i', (value) => {
		expect(MessageTypeSchema.parse(value)).toBe(value);
	});

	it.each([MessageTypes.CLIENT_SYSTEM, 8, -1, 0.5, '0', null])('rejects non-wire message type %j', (value) => {
		expect(MessageTypeSchema.safeParse(value).success).toBe(false);
	});
});

describe('MessageReferenceTypeSchema', () => {
	it.each(Object.values(MessageReferenceTypes))('accepts reference type %i', (value) => {
		expect(MessageReferenceTypeSchema.parse(value)).toBe(value);
	});

	it.each([2, -1, 0.5, '0', null])('rejects invalid reference type %j', (value) => {
		expect(MessageReferenceTypeSchema.safeParse(value).success).toBe(false);
	});
});
