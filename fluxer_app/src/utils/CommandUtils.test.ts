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
import {isCommand, parseCommand, transformWrappingCommands} from './CommandUtils';

describe('CommandUtils', () => {
	describe('parseCommand', () => {
		test('should parse /nick command', () => {
			const result = parseCommand('/nick NewNickname');
			expect(result).toEqual({
				type: 'nick',
				nickname: 'NewNickname',
			});
		});

		test('should parse /kick command with user mention', () => {
			const result = parseCommand('/kick <@123456789> Spamming');
			expect(result).toEqual({
				type: 'kick',
				userId: '123456789',
				reason: 'Spamming',
			});
		});

		test('should parse /kick command with user mention and no reason', () => {
			const result = parseCommand('/kick <@123456789>');
			expect(result).toEqual({
				type: 'kick',
				userId: '123456789',
				reason: undefined,
			});
		});

		test('should parse /ban command with user mention', () => {
			const result = parseCommand('/ban <@123456789> Harassment');
			expect(result).toEqual({
				type: 'ban',
				userId: '123456789',
				deleteMessageDays: 1,
				duration: 0,
				reason: 'Harassment',
			});
		});

		test('should parse /msg command with user mention', () => {
			const result = parseCommand('/msg <@123456789> Hello there!');
			expect(result).toEqual({
				type: 'msg',
				userId: '123456789',
				message: 'Hello there!',
			});
		});

		test('should return unknown for invalid /msg command', () => {
			const result = parseCommand('/msg <@123456789>');
			expect(result).toEqual({type: 'unknown'});
		});

		test('should return unknown for command without user mention', () => {
			const result = parseCommand('/kick username');
			expect(result).toEqual({type: 'unknown'});
		});

		test('should return unknown for unknown command', () => {
			const result = parseCommand('/unknown command');
			expect(result).toEqual({type: 'unknown'});
		});

		test('should handle whitespace in commands', () => {
			const result = parseCommand('  /nick  NewNickname  ');
			expect(result).toEqual({
				type: 'nick',
				nickname: 'NewNickname',
			});
		});

		test('should parse /me command', () => {
			const result = parseCommand('/me does something');
			expect(result).toEqual({type: 'me', content: 'does something'});
		});

		test('should parse /me command with whitespace', () => {
			const result = parseCommand('  /me  does something  ');
			expect(result).toEqual({type: 'me', content: 'does something'});
		});

		test('should return unknown for empty /me command', () => {
			const result = parseCommand('/me ');
			expect(result).toEqual({type: 'unknown'});
		});

		test('should parse /spoiler command', () => {
			const result = parseCommand('/spoiler secret message');
			expect(result).toEqual({type: 'spoiler', content: 'secret message'});
		});

		test('should parse /spoiler command with whitespace', () => {
			const result = parseCommand('  /spoiler  secret message  ');
			expect(result).toEqual({type: 'spoiler', content: 'secret message'});
		});

		test('should return unknown for empty /spoiler command', () => {
			const result = parseCommand('/spoiler ');
			expect(result).toEqual({type: 'unknown'});
		});
	});

	describe('isCommand', () => {
		test('should return true for valid commands', () => {
			expect(isCommand('/nick test')).toBe(true);
			expect(isCommand('/kick <@123456789>')).toBe(true);
			expect(isCommand('/ban <@123456789>')).toBe(true);
			expect(isCommand('/msg <@123456789> hello')).toBe(true);
			expect(isCommand('/me does something')).toBe(true);
			expect(isCommand('/spoiler secret message')).toBe(true);
			expect(isCommand('_action_')).toBe(true);
		});

		test('should return false for non-commands', () => {
			expect(isCommand('hello world')).toBe(false);
			expect(isCommand('/unknown')).toBe(false);
			expect(isCommand('')).toBe(false);
		});

		test('should return false for incomplete commands', () => {
			expect(isCommand('/nick')).toBe(false);
			expect(isCommand('/kick')).toBe(false);
			expect(isCommand('/ban')).toBe(false);
			expect(isCommand('/msg')).toBe(false);
		});

		test('should handle whitespace', () => {
			expect(isCommand('  /nick test  ')).toBe(true);
			expect(isCommand('  _action_  ')).toBe(true);
		});
	});

	describe('transformWrappingCommands', () => {
		test('should transform /me command', () => {
			const result = transformWrappingCommands('/me does something');
			expect(result).toBe('_does something_');
		});

		test('should transform /me command with whitespace', () => {
			const result = transformWrappingCommands('  /me  does something  ');
			expect(result).toBe('_does something_');
		});

		test('should not transform empty /me command', () => {
			const result = transformWrappingCommands('/me ');
			expect(result).toBe('/me ');
		});

		test('should transform /spoiler command', () => {
			const result = transformWrappingCommands('/spoiler secret message');
			expect(result).toBe('||secret message||');
		});

		test('should transform /spoiler command with whitespace', () => {
			const result = transformWrappingCommands('  /spoiler  secret message  ');
			expect(result).toBe('||secret message||');
		});

		test('should not transform empty /spoiler command', () => {
			const result = transformWrappingCommands('/spoiler ');
			expect(result).toBe('/spoiler ');
		});

		test('should return original content for non-wrapping commands', () => {
			const result = transformWrappingCommands('hello world');
			expect(result).toBe('hello world');
		});
	});
});
