// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUuid} from '@app/features/platform/utils/RandomUuid';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@app/features/guild/state/Guilds', () => ({default: {}}));
vi.mock('@app/features/member/state/GuildMembers', () => ({default: {}}));
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {}}));
vi.mock('@app/features/user/state/Users', () => ({default: {}}));

const UUID_V4 = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;

describe('without crypto.randomUUID', () => {
	beforeEach(() => {
		Object.defineProperty(globalThis.crypto, 'randomUUID', {value: undefined, configurable: true});
	});

	afterEach(() => {
		Reflect.deleteProperty(globalThis.crypto, 'randomUUID');
	});

	test('randomUuid returns distinct v4 UUIDs', () => {
		const ids = new Set(Array.from({length: 64}, () => randomUuid()));
		expect(ids.size).toBe(64);
		for (const id of ids) {
			expect(id).toMatch(UUID_V4);
		}
	});

	test('member search context is created on composer mount', async () => {
		const {SearchContext} = await import('@app/features/member/state/MemberSearch');
		const first = new SearchContext(() => {});
		const second = new SearchContext(() => {});
		expect(first).toBeInstanceOf(SearchContext);
		expect(second).not.toBe(first);
	});

	test('toasts and custom keybinds get ids', async () => {
		const [{default: Toasts}, {generateCustomKeybindId}] = await Promise.all([
			import('@app/features/ui/state/Toast'),
			import('@app/features/input/state/input_keybind/KeybindSyncCodec'),
		]);
		const toastId = Toasts.success('saved');
		expect(toastId).toMatch(UUID_V4);
		expect(Toasts.hasToast(toastId)).toBe(true);
		expect(generateCustomKeybindId()).toMatch(UUID_V4);
	});
});

test('randomUuid uses crypto.randomUUID when present', () => {
	const fixed = '00000000-0000-4000-8000-000000000000';
	Object.defineProperty(globalThis.crypto, 'randomUUID', {value: () => fixed, configurable: true});
	try {
		expect(randomUuid()).toBe(fixed);
	} finally {
		Reflect.deleteProperty(globalThis.crypto, 'randomUUID');
	}
});
