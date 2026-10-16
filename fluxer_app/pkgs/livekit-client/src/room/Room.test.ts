// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {describe, expect, it, vi} from 'vitest';
import type {InternalRoomOptions} from '../options.ts';
import Room from './Room.ts';
import RTCEngine from './RTCEngine.ts';

describe('Room createEngine option', () => {
	it('builds the engine through the factory and hands it the merged room options', () => {
		const createEngine = vi.fn((options: InternalRoomOptions) => new RTCEngine(options));
		const room = new Room({createEngine, dynacast: false});
		expect(createEngine).toHaveBeenCalledTimes(1);
		expect(room.engine).toBe(createEngine.mock.results[0]?.value);
		expect(createEngine.mock.calls[0]?.[0]).toBe(room.options);
	});

	it('builds a plain RTCEngine when no factory is set', () => {
		const room = new Room({dynacast: false});
		expect(room.engine).toBeInstanceOf(RTCEngine);
	});
});
