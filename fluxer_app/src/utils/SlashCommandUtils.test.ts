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

import {describe, expect, test} from 'vitest';
import type {Command} from '~/components/channel/Autocomplete';
import {
	detectAutocompleteTrigger,
	filterCommandsByQuery,
	getCommandInsertionText,
	isCommandRequiringUserMention,
} from './SlashCommandUtils';

describe('SlashCommandUtils', () => {
	describe('detectAutocompleteTrigger', () => {
		test('should detect mention trigger', () => {
			const result = detectAutocompleteTrigger('hello @use');
			expect(result).toEqual({
				type: 'mention',
				match: expect.any(Array),
				matchedText: 'use',
			});
		});

		test('should detect channel trigger', () => {
			const result = detectAutocompleteTrigger('hello #cha');
			expect(result).toEqual({
				type: 'channel',
				match: expect.any(Array),
				matchedText: 'cha',
			});
		});

		test('should detect emoji trigger', () => {
			const result = detectAutocompleteTrigger('hello :sm');
			expect(result).toEqual({
				type: 'emoji',
				match: expect.any(Array),
				matchedText: 'sm',
			});
		});

		test('should detect command trigger with slash only', () => {
			const result = detectAutocompleteTrigger('/');
			expect(result).toEqual({
				type: 'command',
				match: expect.any(Array),
				matchedText: '',
			});
		});

		test('should detect command trigger with partial command', () => {
			const result = detectAutocompleteTrigger('/ba');
			expect(result).toEqual({
				type: 'command',
				match: expect.any(Array),
				matchedText: 'ba',
			});
		});

		test('should detect meme search trigger', () => {
			const result = detectAutocompleteTrigger('/saved cat');
			expect(result).toEqual({
				type: 'meme',
				match: expect.any(Array),
				matchedText: 'cat',
			});
		});

		test('should detect meme search trigger with empty query', () => {
			const result = detectAutocompleteTrigger('/saved ');
			expect(result).toEqual({
				type: 'meme',
				match: expect.any(Array),
				matchedText: '',
			});
		});

		test('should detect sticker search trigger with empty query', () => {
			const result = detectAutocompleteTrigger('/sticker ');
			expect(result).toEqual({
				type: 'sticker',
				match: expect.any(Array),
				matchedText: '',
			});
		});

		test('should detect command argument trigger', () => {
			const result = detectAutocompleteTrigger('/kick user');
			expect(result).toEqual({
				type: 'commandArg',
				match: expect.any(Array),
				matchedText: 'user',
			});
		});

		test('should detect command argument trigger with empty query', () => {
			const result = detectAutocompleteTrigger('/kick ');
			expect(result).toEqual({
				type: 'commandArg',
				match: expect.any(Array),
				matchedText: '',
			});
		});

		test('should detect command argument mention trigger', () => {
			const result = detectAutocompleteTrigger('/kick @user');
			expect(result).toEqual({
				type: 'commandArgMention',
				match: expect.any(Array),
				matchedText: 'user',
			});
		});

		test('should detect gif search trigger', () => {
			const result = detectAutocompleteTrigger('/gif funny');
			expect(result).toEqual({
				type: 'gif',
				match: expect.any(Array),
				matchedText: 'funny',
			});
		});

		test('should detect command argument mention trigger', () => {
			const result = detectAutocompleteTrigger('/ban @use');
			expect(result).toEqual({
				type: 'commandArgMention',
				match: expect.any(Array),
				matchedText: 'use',
			});
		});

		test('should return null for no trigger', () => {
			const result = detectAutocompleteTrigger('hello world');
			expect(result).toBeNull();
		});

		test('should not trigger when / is not at start or after whitespace', () => {
			const result = detectAutocompleteTrigger('hello/test');
			expect(result).toBeNull();
		});

		test('should trigger when / is at start of text', () => {
			const result = detectAutocompleteTrigger('/');
			expect(result?.type).toBe('command');
		});

		test('should NOT trigger when / is in the middle of text', () => {
			const result = detectAutocompleteTrigger('test /');
			expect(result).toBeNull();
		});

		test('should trigger when / is after leading whitespace', () => {
			const result = detectAutocompleteTrigger('   /');
			expect(result?.type).toBe('command');
		});
	});

	describe('filterCommandsByQuery', () => {
		const mockCommands: Array<Command> = [
			{type: 'simple', name: '/shrug', content: '¯\\_(ツ)_/¯'},
			{type: 'action', name: '/ban', permission: 4n, requiresGuild: true},
			{type: 'action', name: '/kick', permission: 2n, requiresGuild: true},
			{type: 'action', name: '/msg'},
			{type: 'action', name: '/saved'},
		];

		test('should return all commands when query is empty', () => {
			const result = filterCommandsByQuery(mockCommands, '');
			expect(result).toHaveLength(5);
		});

		test('should filter commands by name', () => {
			const result = filterCommandsByQuery(mockCommands, 'ba');
			expect(result).toHaveLength(1);
			expect(result[0].name).toBe('/ban');
		});

		test('should be case insensitive', () => {
			const result = filterCommandsByQuery(mockCommands, 'BAN');
			expect(result).toHaveLength(1);
			expect(result[0].name).toBe('/ban');
		});

		test('should return empty array when no matches', () => {
			const result = filterCommandsByQuery(mockCommands, 'xyz');
			expect(result).toHaveLength(0);
		});

		test('should handle partial matches', () => {
			const result = filterCommandsByQuery(mockCommands, 'm');
			expect(result).toHaveLength(1);
			expect(result.map((c) => c.name)).toEqual(expect.arrayContaining(['/msg']));
		});
	});

	describe('isCommandRequiringUserMention', () => {
		test('should return true for commands requiring user mentions', () => {
			expect(isCommandRequiringUserMention('/kick')).toBe(true);
			expect(isCommandRequiringUserMention('/ban')).toBe(true);
			expect(isCommandRequiringUserMention('/msg')).toBe(true);
			expect(isCommandRequiringUserMention('/saved')).toBe(true);
		});

		test('should return false for other commands', () => {
			expect(isCommandRequiringUserMention('/shrug')).toBe(false);
			expect(isCommandRequiringUserMention('/nick')).toBe(false);
			expect(isCommandRequiringUserMention('/me')).toBe(false);
		});
	});

	describe('getCommandInsertionText', () => {
		test('should return content for simple commands', () => {
			const command: Command = {type: 'simple', name: '/shrug', content: '¯\\_(ツ)_/¯'};
			expect(getCommandInsertionText(command)).toBe('¯\\_(ツ)_/¯');
		});

		test('should return command name with space for action commands', () => {
			const command: Command = {type: 'action', name: '/ban', permission: 4n, requiresGuild: true};
			expect(getCommandInsertionText(command)).toBe('/ban ');
		});

		test('should return command name with space for action commands without permission', () => {
			const command: Command = {type: 'action', name: '/msg'};
			expect(getCommandInsertionText(command)).toBe('/msg ');
		});
	});
});
