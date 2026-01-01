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

import {beforeEach, describe, expect, it} from 'vitest';
import {TextareaStateManager} from './TextareaStateManager';

describe('TextareaStateManager', () => {
	let manager: TextareaStateManager;

	beforeEach(() => {
		manager = new TextareaStateManager();
	});

	describe('basic state management', () => {
		it('should start with empty text', () => {
			expect(manager.getText()).toBe('');
		});

		it('should update text', () => {
			manager.setText('Hello world');
			expect(manager.getText()).toBe('Hello world');
		});

		it('should track cursor position', () => {
			manager.setCursorPosition(5);
			expect(manager.getCursorPosition()).toBe(5);
		});

		it('should clear all state', () => {
			manager.setText('Hello');
			manager.setCursorPosition(3);
			manager.clear();
			expect(manager.getText()).toBe('');
			expect(manager.getCursorPosition()).toBe(0);
		});
	});

	describe('text up to cursor', () => {
		it('should return text up to cursor position', () => {
			manager.setText('Hello world');
			manager.setCursorPosition(5);
			expect(manager.getTextUpToCursor()).toBe('Hello');
		});

		it('should return empty string when cursor is at start', () => {
			manager.setText('Hello world');
			manager.setCursorPosition(0);
			expect(manager.getTextUpToCursor()).toBe('');
		});

		it('should return full text when cursor is at end', () => {
			manager.setText('Hello world');
			manager.setCursorPosition(11);
			expect(manager.getTextUpToCursor()).toBe('Hello world');
		});
	});

	describe('autocomplete detection', () => {
		describe('mention detection', () => {
			it('should detect mention at start', () => {
				manager.setText('@user');
				manager.setCursorPosition(5);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'mention',
					query: 'user',
					matchStart: 0,
					matchEnd: 5,
				});
			});

			it('should detect mention after space', () => {
				manager.setText('Hello @user');
				manager.setCursorPosition(11);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'mention',
					query: 'user',
					matchStart: 5,
					matchEnd: 11,
				});
			});

			it('should detect partial mention', () => {
				manager.setText('Hello @u');
				manager.setCursorPosition(8);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'mention',
					query: 'u',
					matchStart: 5,
					matchEnd: 8,
				});
			});

			it('should not detect mention in middle of word', () => {
				manager.setText('email@user');
				manager.setCursorPosition(10);
				const match = manager.detectAutocompleteMatch();
				expect(match).toBeNull();
			});
		});

		describe('channel detection', () => {
			it('should detect channel mention', () => {
				manager.setText('#general');
				manager.setCursorPosition(8);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'channel',
					query: 'general',
					matchStart: 0,
					matchEnd: 8,
				});
			});

			it('should detect partial channel', () => {
				manager.setText('Join #gen');
				manager.setCursorPosition(9);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'channel',
					query: 'gen',
					matchStart: 4,
					matchEnd: 9,
				});
			});
		});

		describe('emoji detection', () => {
			it('should detect emoji', () => {
				manager.setText(':smile');
				manager.setCursorPosition(6);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'emoji',
					query: 'smile',
					matchStart: 0,
					matchEnd: 6,
				});
			});

			it('should detect emoji with minimum 2 chars', () => {
				manager.setText(':sm');
				manager.setCursorPosition(3);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'emoji',
					query: 'sm',
					matchStart: 0,
					matchEnd: 3,
				});
			});

			it('should not detect emoji with 1 char', () => {
				manager.setText(':s');
				manager.setCursorPosition(2);
				const match = manager.detectAutocompleteMatch();
				expect(match).toBeNull();
			});
		});

		describe('command detection', () => {
			it('should detect command at start', () => {
				manager.setText('/');
				manager.setCursorPosition(1);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'command',
					query: '',
					matchStart: 0,
					matchEnd: 1,
				});
			});

			it('should detect command after space', () => {
				manager.setText('Hello /');
				manager.setCursorPosition(7);
				const match = manager.detectAutocompleteMatch();
				expect(match).toEqual({
					type: 'command',
					query: '',
					matchStart: 5,
					matchEnd: 7,
				});
			});
		});

		describe('priority', () => {
			it('should prioritize mention over others', () => {
				manager.setText('@user #channel :emoji');
				manager.setCursorPosition(5);
				const match = manager.detectAutocompleteMatch();
				expect(match?.type).toBe('mention');
			});
		});
	});

	describe('insertAutocompleteSegment', () => {
		it('should insert mention segment', () => {
			manager.setText('Hello @u');
			manager.setCursorPosition(8);
			const match = manager.detectAutocompleteMatch()!;

			const result = manager.insertAutocompleteSegment(match, '@User#0001', '<@123>', 'user', '123');

			expect(result.newText).toBe('Hello @User#0001 ');
			expect(manager.getText()).toBe('Hello @User#0001 ');
			expect(manager.getSegmentManager().getSegments()).toHaveLength(1);
		});

		it('should add space before mention if needed', () => {
			manager.setText('Hello @u');
			manager.setCursorPosition(8);
			const match = manager.detectAutocompleteMatch()!;

			const result = manager.insertAutocompleteSegment(match, '@User#0001', '<@123>', 'user', '123');

			expect(result.newText).toBe('Hello @User#0001 ');
		});

		it('should preserve text after match', () => {
			manager.setText('Hello @u world');
			manager.setCursorPosition(8);
			const match = manager.detectAutocompleteMatch()!;

			const result = manager.insertAutocompleteSegment(match, '@User#0001', '<@123>', 'user', '123');

			expect(result.newText).toBe('Hello @User#0001  world');
		});

		it('should update cursor position correctly', () => {
			manager.setText('Hello @u');
			manager.setCursorPosition(8);
			const match = manager.detectAutocompleteMatch()!;

			const result = manager.insertAutocompleteSegment(match, '@User#0001', '<@123>', 'user', '123');

			expect(result.newCursorPosition).toBe(17);
		});
	});

	describe('insertPlainText', () => {
		it('should insert command without segment', () => {
			manager.setText('/');
			manager.setCursorPosition(1);
			const match = manager.detectAutocompleteMatch()!;

			const result = manager.insertPlainText(match, '¯\\_(ツ)_/¯');

			expect(result.newText).toBe('¯\\_(ツ)_/¯ ');
			expect(manager.getSegmentManager().getSegments()).toHaveLength(0);
		});

		it('should add space before text if needed', () => {
			manager.setText('Hello /');
			manager.setCursorPosition(7);
			const match = manager.detectAutocompleteMatch()!;

			const result = manager.insertPlainText(match, 'shrug');

			expect(result.newText).toBe('Hello shrug ');
		});
	});

	describe('insertSegmentAtCursor', () => {
		it('should insert segment at end of text', () => {
			manager.setText('Hello ');
			const result = manager.insertSegmentAtCursor('@User#0001', '<@123>', 'user', '123');

			expect(result).toBe('Hello @User#0001');
			expect(manager.getSegmentManager().getSegments()).toHaveLength(1);
		});

		it('should add space if text does not end with space', () => {
			manager.setText('Hello');
			const result = manager.insertSegmentAtCursor('@User#0001', '<@123>', 'user', '123');

			expect(result).toBe('Hello @User#0001');
		});

		it('should work on empty text', () => {
			const result = manager.insertSegmentAtCursor('@User#0001', '<@123>', 'user', '123');

			expect(result).toBe('@User#0001');
		});
	});

	describe('getActualContent', () => {
		it('should convert display text to actual content', () => {
			manager.setText('Hello ');
			manager.insertSegmentAtCursor('@User#0001', '<@123>', 'user', '123');

			const actual = manager.getActualContent();
			expect(actual).toBe('Hello <@123>');
		});

		it('should handle multiple segments', () => {
			manager.setText('');
			manager.insertSegmentAtCursor('@User#0001', '<@123>', 'user', '123');

			const newText = `${manager.getText()} and `;
			manager.handleTextChange(newText);
			manager.insertSegmentAtCursor('@Other#0002', '<@456>', 'user', '456');

			const actual = manager.getActualContent();
			expect(actual).toBe('<@123> and <@456>');
		});
	});

	describe('hasOpenCodeBlock', () => {
		it('should detect open code block', () => {
			manager.setText('```\ncode');
			manager.setCursorPosition(8);
			expect(manager.hasOpenCodeBlock()).toBe(true);
		});

		it('should detect closed code block', () => {
			manager.setText('```\ncode\n```');
			manager.setCursorPosition(13);
			expect(manager.hasOpenCodeBlock()).toBe(false);
		});

		it('should handle no code blocks', () => {
			manager.setText('normal text');
			manager.setCursorPosition(11);
			expect(manager.hasOpenCodeBlock()).toBe(false);
		});

		it('should handle multiple open/close blocks', () => {
			manager.setText('```\ncode\n``` ```\nmore');
			manager.setCursorPosition(19);
			expect(manager.hasOpenCodeBlock()).toBe(true);
		});
	});

	describe('handleTextChange', () => {
		it('should update text and adjust segments', () => {
			manager.setText('Hello ');
			manager.insertSegmentAtCursor('@User#0001', '<@123>', 'user', '123');

			manager.handleTextChange('Hello @User#0001 world');

			expect(manager.getText()).toBe('Hello @User#0001 world');
			const segments = manager.getSegmentManager().getSegments();
			expect(segments).toHaveLength(1);
			expect(segments[0].start).toBe(6);
			expect(segments[0].end).toBe(16);
		});

		it('should remove segment when edited', () => {
			manager.setText('Hello ');
			manager.insertSegmentAtCursor('@User#0001', '<@123>', 'user', '123');

			manager.handleTextChange('Hello @User');

			const segments = manager.getSegmentManager().getSegments();
			expect(segments).toHaveLength(0);
		});
	});

	describe('integration scenarios', () => {
		it('should handle complete autocomplete flow', () => {
			manager.setText('@u');
			manager.setCursorPosition(2);

			const match = manager.detectAutocompleteMatch();
			expect(match?.type).toBe('mention');

			manager.insertAutocompleteSegment(match!, '@User#0001', '<@123>', 'user', '123');

			expect(manager.getText()).toBe('@User#0001 ');
			expect(manager.getActualContent()).toBe('<@123> ');
		});

		it('should handle typing after autocomplete', () => {
			manager.setText('');
			manager.insertSegmentAtCursor('@User#0001', '<@123>', 'user', '123');

			manager.handleTextChange('@User#0001 how are you?');

			expect(manager.getText()).toBe('@User#0001 how are you?');
			expect(manager.getActualContent()).toBe('<@123> how are you?');
		});

		it('should handle multiple autocompletes', () => {
			manager.setText('Hey @u');
			manager.setCursorPosition(6);
			let match = manager.detectAutocompleteMatch()!;
			manager.insertAutocompleteSegment(match, '@User#0001', '<@123>', 'user', '123');

			const newText = 'Hey @User#0001 check :smi';
			manager.handleTextChange(newText);
			manager.setCursorPosition(newText.length);

			match = manager.detectAutocompleteMatch()!;
			expect(match.type).toBe('emoji');
			manager.insertAutocompleteSegment(match, ':smile:', '<:smile:emoji123>', 'emoji', 'emoji123');

			expect(manager.getActualContent()).toBe('Hey <@123> check <:smile:emoji123> ');
		});
	});
});
