// SPDX-License-Identifier: AGPL-3.0-or-later

import {usernameBaseFromDisplayName} from '@app/api/utils/UsernameSuggestionUtils';
import {UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {describe, expect, it} from 'vitest';

const notBlocked = () => false;

describe('usernameBaseFromDisplayName', () => {
	it.each([
		{name: 'José Ñúñez', expected: 'Jose_Nunez'},
		{name: 'Åsa Öberg-Ärlig', expected: 'Asa_Oberg_Arlig'},
		{name: 'Дмитрий', expected: 'Dmitriy'},
		{name: 'Γιώργος', expected: 'Giorgos'},
		{name: 'さくら', expected: 'sakura'},
		{name: '李小龍', expected: 'Li_Xiao_Long'},
		{name: '김민준', expected: 'gimminjun'},
		{name: 'محمد', expected: 'mHmd'},
		{name: 'דוד', expected: 'dvd'},
		{name: 'Zoë 🦊', expected: 'Zoe'},
		{name: '𝓕𝓪𝓷𝓬𝔂', expected: 'Fancy'},
		{name: '  __Kai__  ', expected: 'Kai'},
	])('derives $expected from $name', ({name, expected}) => {
		expect(usernameBaseFromDisplayName(name, notBlocked)).toBe(expected);
	});

	it('romanises Japanese kanji into a valid name', () => {
		const derived = usernameBaseFromDisplayName('山田太郎', notBlocked);
		expect(derived).toMatch(/^[A-Za-z_]+$/);
		expect(UsernameType.safeParse(derived).success).toBe(true);
	});

	it.each(['🦊✨', '!!!', '・・・', '❤️‍🔥'])('falls back to a generated name for %s', (name) => {
		const derived = usernameBaseFromDisplayName(name, notBlocked);
		expect(derived).toMatch(/^[A-Z][a-z]+[A-Z][a-z]+$/);
		expect(UsernameType.safeParse(derived).success).toBe(true);
	});

	it('keeps long names within the username limit', () => {
		const derived = usernameBaseFromDisplayName('長い名前のテストユーザーです長い名前のテストユーザーです', notBlocked);
		expect(derived.length).toBeLessThanOrEqual(32);
		expect(derived.endsWith('_')).toBe(false);
		expect(UsernameType.safeParse(derived).success).toBe(true);
	});

	it('drops reserved terms and keeps the rest of the name', () => {
		expect(usernameBaseFromDisplayName('Fluxer Fan', notBlocked)).toBe('Fan');
		expect(usernameBaseFromDisplayName('my system message', notBlocked)).toBe('my');
	});

	it('adds digits to a name that is reserved on its own', () => {
		expect(usernameBaseFromDisplayName('everyone 🎉', notBlocked)).toMatch(/^everyone\d{1,4}$/);
		expect(usernameBaseFromDisplayName('Here', notBlocked)).toMatch(/^Here\d{1,4}$/);
	});

	it('picks a generated name when the derived one is blocked', () => {
		const derived = usernameBaseFromDisplayName('Rude Word', (username) => username.toLowerCase().includes('rude'));
		expect(derived.toLowerCase()).not.toContain('rude');
		expect(UsernameType.safeParse(derived).success).toBe(true);
	});
});
