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
import {type MentionSegment, TextareaSegmentManager} from './TextareaSegmentManager';

describe('TextareaSegmentManager', () => {
	let manager: TextareaSegmentManager;

	beforeEach(() => {
		manager = new TextareaSegmentManager();
	});

	describe('basic operations', () => {
		it('should start with empty segments', () => {
			expect(manager.getSegments()).toEqual([]);
		});

		it('should set segments', () => {
			const segments: Array<MentionSegment> = [
				{
					type: 'user',
					id: '123',
					displayText: '@User#0001',
					actualText: '<@123>',
					start: 0,
					end: 10,
				},
			];
			manager.setSegments(segments);
			expect(manager.getSegments()).toEqual(segments);
		});

		it('should clear segments', () => {
			const segments: Array<MentionSegment> = [
				{
					type: 'user',
					id: '123',
					displayText: '@User#0001',
					actualText: '<@123>',
					start: 0,
					end: 10,
				},
			];
			manager.setSegments(segments);
			manager.clear();
			expect(manager.getSegments()).toEqual([]);
		});
	});

	describe('displayToActual', () => {
		it('should convert display text to actual text with single mention', () => {
			manager.setSegments([
				{
					type: 'user',
					id: '123',
					displayText: '@User#0001',
					actualText: '<@123>',
					start: 5,
					end: 15,
				},
			]);
			const result = manager.displayToActual('Hey, @User#0001 how are you?');
			expect(result).toBe('Hey, <@123> how are you?');
		});

		it('should convert display text with multiple mentions', () => {
			manager.setSegments([
				{
					type: 'user',
					id: '123',
					displayText: '@User#0001',
					actualText: '<@123>',
					start: 0,
					end: 10,
				},
				{
					type: 'user',
					id: '456',
					displayText: '@Other#0002',
					actualText: '<@456>',
					start: 15,
					end: 26,
				},
			]);
			const result = manager.displayToActual('@User#0001 and @Other#0002');
			expect(result).toBe('<@123> and <@456>');
		});

		it('should handle emoji segments', () => {
			manager.setSegments([
				{
					type: 'emoji',
					id: 'emoji123',
					displayText: ':smile:',
					actualText: '<:smile:emoji123>',
					start: 0,
					end: 7,
				},
			]);
			const result = manager.displayToActual(':smile: hello');
			expect(result).toBe('<:smile:emoji123> hello');
		});

		it('should handle channel mentions', () => {
			manager.setSegments([
				{
					type: 'channel',
					id: 'chan123',
					displayText: '#general',
					actualText: '<#chan123>',
					start: 8,
					end: 16,
				},
			]);
			const result = manager.displayToActual('Join us #general');
			expect(result).toBe('Join us <#chan123>');
		});

		it('should handle role mentions', () => {
			manager.setSegments([
				{
					type: 'role',
					id: 'role123',
					displayText: '@Admin',
					actualText: '<@&role123>',
					start: 0,
					end: 6,
				},
			]);
			const result = manager.displayToActual('@Admin please help');
			expect(result).toBe('<@&role123> please help');
		});

		it('should return unchanged text when no segments', () => {
			const result = manager.displayToActual('Hello world');
			expect(result).toBe('Hello world');
		});
	});

	describe('insertSegment', () => {
		it('should insert segment at beginning', () => {
			const {newText, newSegments} = manager.insertSegment('world', 0, '@User#0001 ', '<@123> ', 'user', '123');
			expect(newText).toBe('@User#0001 world');
			expect(newSegments).toHaveLength(1);
			expect(newSegments[0]).toMatchObject({
				start: 0,
				end: 11,
				displayText: '@User#0001 ',
				actualText: '<@123> ',
			});
		});

		it('should insert segment at end', () => {
			const {newText, newSegments} = manager.insertSegment('Hello ', 6, '@User#0001', '<@123>', 'user', '123');
			expect(newText).toBe('Hello @User#0001');
			expect(newSegments).toHaveLength(1);
			expect(newSegments[0]).toMatchObject({
				start: 6,
				end: 16,
			});
		});

		it('should insert segment in middle', () => {
			const {newText, newSegments} = manager.insertSegment('Hello world', 6, '@User#0001 ', '<@123> ', 'user', '123');
			expect(newText).toBe('Hello @User#0001 world');
			expect(newSegments).toHaveLength(1);
			expect(newSegments[0]).toMatchObject({
				start: 6,
				end: 17,
			});
		});

		it('should shift existing segments when inserting before them', () => {
			manager.insertSegment('world', 0, '@User#0001 ', '<@123> ', 'user', '123');

			const {newText, newSegments} = manager.insertSegment(
				'@User#0001 world',
				0,
				'@Other#0002 ',
				'<@456> ',
				'user',
				'456',
			);

			expect(newText).toBe('@Other#0002 @User#0001 world');
			expect(newSegments).toHaveLength(2);
			expect(newSegments[0]).toMatchObject({
				start: 12,
				end: 23,
				id: '123',
			});
			expect(newSegments[1]).toMatchObject({
				start: 0,
				end: 12,
				id: '456',
			});
		});

		it('should not shift segments that are before insertion point', () => {
			manager.insertSegment('world', 0, '@User#0001 ', '<@123> ', 'user', '123');

			const {newText, newSegments} = manager.insertSegment(
				'@User#0001 world',
				17,
				'@Other#0002',
				'<@456>',
				'user',
				'456',
			);

			expect(newText).toBe('@User#0001 world@Other#0002');
			expect(newSegments).toHaveLength(2);
			expect(newSegments[0]).toMatchObject({
				start: 0,
				end: 11,
				id: '123',
			});
			expect(newSegments[1]).toMatchObject({
				start: 17,
				end: 28,
				id: '456',
			});
		});
	});

	describe('updateSegmentsForTextChange', () => {
		beforeEach(() => {
			manager.setSegments([
				{
					type: 'user',
					id: '123',
					displayText: '@User#0001',
					actualText: '<@123>',
					start: 6,
					end: 16,
				},
				{
					type: 'user',
					id: '456',
					displayText: '@Other#0002',
					actualText: '<@456>',
					start: 21,
					end: 32,
				},
			]);
		});

		it('should keep segments before the change', () => {
			const segments = manager.updateSegmentsForTextChange(32, 32, 6);
			expect(segments).toHaveLength(2);
			expect(segments[0]).toMatchObject({start: 6, end: 16});
			expect(segments[1]).toMatchObject({start: 21, end: 32});
		});

		it('should shift segments after the change', () => {
			const segments = manager.updateSegmentsForTextChange(0, 0, 4);
			expect(segments).toHaveLength(2);
			expect(segments[0]).toMatchObject({start: 10, end: 20});
			expect(segments[1]).toMatchObject({start: 25, end: 36});
		});

		it('should remove segment that overlaps with change', () => {
			const segments = manager.updateSegmentsForTextChange(6, 10, 0);
			expect(segments).toHaveLength(1);
			expect(segments[0]).toMatchObject({
				id: '456',
				start: 17,
				end: 28,
			});
		});

		it('should handle deletion in middle of text', () => {
			const segments = manager.updateSegmentsForTextChange(16, 21, 0);
			expect(segments).toHaveLength(2);
			expect(segments[0]).toMatchObject({start: 6, end: 16});
			expect(segments[1]).toMatchObject({start: 16, end: 27});
		});

		it('should handle replacement', () => {
			const segments = manager.updateSegmentsForTextChange(0, 6, 3);
			expect(segments).toHaveLength(2);
			expect(segments[0]).toMatchObject({start: 3, end: 13});
			expect(segments[1]).toMatchObject({start: 18, end: 29});
		});

		it('should handle complete deletion of segment', () => {
			const segments = manager.updateSegmentsForTextChange(5, 17, 0);
			expect(segments).toHaveLength(1);
			expect(segments[0]).toMatchObject({
				id: '456',
				start: 9,
				end: 20,
			});
		});

		it('should handle multiple segments being removed', () => {
			const segments = manager.updateSegmentsForTextChange(0, 32, 0);
			expect(segments).toHaveLength(0);
		});
	});

	describe('detectChange', () => {
		it('should detect insertion at beginning', () => {
			const change = TextareaSegmentManager.detectChange('world', 'Hello world');
			expect(change).toEqual({
				changeStart: 0,
				changeEnd: 0,
				replacementLength: 6,
			});
		});

		it('should detect insertion at end', () => {
			const change = TextareaSegmentManager.detectChange('Hello', 'Hello world');
			expect(change).toEqual({
				changeStart: 5,
				changeEnd: 5,
				replacementLength: 6,
			});
		});

		it('should detect insertion in middle', () => {
			const change = TextareaSegmentManager.detectChange('Helloworld', 'Hello world');
			expect(change).toEqual({
				changeStart: 5,
				changeEnd: 5,
				replacementLength: 1,
			});
		});

		it('should detect deletion at beginning', () => {
			const change = TextareaSegmentManager.detectChange('Hello world', 'world');
			expect(change).toEqual({
				changeStart: 0,
				changeEnd: 6,
				replacementLength: 0,
			});
		});

		it('should detect deletion at end', () => {
			const change = TextareaSegmentManager.detectChange('Hello world', 'Hello');
			expect(change).toEqual({
				changeStart: 5,
				changeEnd: 11,
				replacementLength: 0,
			});
		});

		it('should detect deletion in middle', () => {
			const change = TextareaSegmentManager.detectChange('Hello world', 'Helloworld');
			expect(change).toEqual({
				changeStart: 5,
				changeEnd: 6,
				replacementLength: 0,
			});
		});

		it('should detect replacement', () => {
			const change = TextareaSegmentManager.detectChange('Hello world', 'Hi world');
			expect(change).toEqual({
				changeStart: 1,
				changeEnd: 5,
				replacementLength: 1,
			});
		});

		it('should detect no change', () => {
			const change = TextareaSegmentManager.detectChange('Hello world', 'Hello world');
			expect(change).toEqual({
				changeStart: 11,
				changeEnd: 11,
				replacementLength: 0,
			});
		});

		it('should detect complex replacement in middle', () => {
			const change = TextareaSegmentManager.detectChange('The quick brown fox', 'The slow brown fox');
			expect(change).toEqual({
				changeStart: 4,
				changeEnd: 9,
				replacementLength: 4,
			});
		});
	});

	describe('integration scenarios', () => {
		it('should handle user typing and then autocompleting a mention', () => {
			const text = 'Hello @u';

			const change = TextareaSegmentManager.detectChange(text, 'Hello ');
			manager.updateSegmentsForTextChange(change.changeStart, change.changeEnd, change.replacementLength);

			const {newText} = manager.insertSegment('Hello ', 6, '@User#0001 ', '<@123> ', 'user', '123');

			expect(newText).toBe('Hello @User#0001 ');
			expect(manager.getSegments()).toHaveLength(1);
			expect(manager.displayToActual(newText)).toBe('Hello <@123> ');
		});

		it('should handle user deleting part of a mention', () => {
			manager.insertSegment('Hello ', 6, '@User#0001 ', '<@123> ', 'user', '123');

			const oldText = 'Hello @User#0001 world';
			const newText = 'Hello @0001 world';
			const change = TextareaSegmentManager.detectChange(oldText, newText);

			const segments = manager.updateSegmentsForTextChange(
				change.changeStart,
				change.changeEnd,
				change.replacementLength,
			);

			expect(segments).toHaveLength(0);
		});

		it('should handle multiple mentions and editing between them', () => {
			manager.insertSegment('', 0, '@User#0001 ', '<@123> ', 'user', '123');
			manager.insertSegment('@User#0001 and ', 15, '@Other#0002', '<@456>', 'user', '456');

			const oldText = '@User#0001 and @Other#0002';
			const newText = '@User#0001 hello and @Other#0002';
			const change = TextareaSegmentManager.detectChange(oldText, newText);

			manager.updateSegmentsForTextChange(change.changeStart, change.changeEnd, change.replacementLength);

			const segments = manager.getSegments();
			expect(segments).toHaveLength(2);
			expect(segments[0]).toMatchObject({start: 0, end: 11});
			expect(segments[1]).toMatchObject({start: 21, end: 32});
		});

		it('should handle complex scenario with emoji and mentions', () => {
			manager.insertSegment('Hey ', 4, '@User#0001 ', '<@123> ', 'user', '123');
			manager.insertSegment('Hey @User#0001 ', 15, ':smile: ', '<:smile:emoji123> ', 'emoji', 'emoji123');

			const text = "Hey @User#0001 :smile: what's up?";
			const actualText = manager.displayToActual(text);

			expect(actualText).toBe("Hey <@123> <:smile:emoji123> what's up?");
			expect(manager.getSegments()).toHaveLength(2);
		});

		it('should handle realistic message submission flow - insert mention then type more text', () => {
			let displayText = 'Hey ';
			const {newText: afterMention} = manager.insertSegment(displayText, 4, '@User#0001 ', '<@123> ', 'user', '123');
			displayText = afterMention;

			expect(displayText).toBe('Hey @User#0001 ');
			expect(manager.getSegments()).toHaveLength(1);

			const typedMore = 'Hey @User#0001 how are you?';
			const change = TextareaSegmentManager.detectChange(displayText, typedMore);
			manager.updateSegmentsForTextChange(change.changeStart, change.changeEnd, change.replacementLength);
			displayText = typedMore;

			const actualContent = manager.displayToActual(displayText);
			expect(actualContent).toBe('Hey <@123> how are you?');
		});

		it('should handle realistic multiple mentions submission flow', () => {
			let displayText = '';
			const {newText: after1} = manager.insertSegment(displayText, 0, '@User#0001 ', '<@123> ', 'user', '123');
			displayText = after1;

			expect(displayText).toBe('@User#0001 ');

			const withAnd = '@User#0001 and ';
			const change1 = TextareaSegmentManager.detectChange(displayText, withAnd);
			manager.updateSegmentsForTextChange(change1.changeStart, change1.changeEnd, change1.replacementLength);
			displayText = withAnd;

			const {newText: after2} = manager.insertSegment(
				displayText,
				displayText.length,
				'@Other#0002 ',
				'<@456> ',
				'user',
				'456',
			);
			displayText = after2;

			expect(displayText).toBe('@User#0001 and @Other#0002 ');

			const final = '@User#0001 and @Other#0002 are cool';
			const change2 = TextareaSegmentManager.detectChange(displayText, final);
			manager.updateSegmentsForTextChange(change2.changeStart, change2.changeEnd, change2.replacementLength);
			displayText = final;

			const actualContent = manager.displayToActual(displayText);
			expect(actualContent).toBe('<@123> and <@456> are cool');
		});

		it('should handle mention at start of message after trimStart', () => {
			const {newText} = manager.insertSegment(' ', 1, '@User#0001 ', '<@123> ', 'user', '123');
			expect(newText).toBe(' @User#0001 ');

			const trimmed = newText.trimStart();
			const trimmedChars = newText.length - trimmed.length;
			const segments = manager.getSegments();
			const adjusted = segments.map((seg) => ({
				...seg,
				start: seg.start - trimmedChars,
				end: seg.end - trimmedChars,
			}));
			manager.setSegments(adjusted);

			const actualContent = manager.displayToActual(trimmed);
			expect(actualContent).toBe('<@123> ');
		});

		it('should handle editing text before a mention', () => {
			manager.insertSegment('Hello ', 6, '@User#0001 ', '<@123> ', 'user', '123');

			const oldText = 'Hello @User#0001 ';
			const newText = 'Hello there @User#0001 ';
			const change = TextareaSegmentManager.detectChange(oldText, newText);
			manager.updateSegmentsForTextChange(change.changeStart, change.changeEnd, change.replacementLength);

			const segments = manager.getSegments();
			expect(segments).toHaveLength(1);
			expect(segments[0].start).toBe(12);
			expect(segments[0].end).toBe(23);

			const actualContent = manager.displayToActual(newText);
			expect(actualContent).toBe('Hello there <@123> ');
		});

		it('should handle editing text after a mention', () => {
			manager.insertSegment('', 0, '@User#0001 ', '<@123> ', 'user', '123');

			const oldText = '@User#0001 ';
			const newText = '@User#0001 hello';
			const change = TextareaSegmentManager.detectChange(oldText, newText);
			manager.updateSegmentsForTextChange(change.changeStart, change.changeEnd, change.replacementLength);

			const segments = manager.getSegments();
			expect(segments).toHaveLength(1);
			expect(segments[0].start).toBe(0);
			expect(segments[0].end).toBe(11);

			const actualContent = manager.displayToActual(newText);
			expect(actualContent).toBe('<@123> hello');
		});
	});

	describe('displayToActualSubstring', () => {
		beforeEach(() => {
			manager.insertSegment('Hey ', 4, '@User#0001 ', '<@123> ', 'user', '123');
			manager.insertSegment('Hey @User#0001 check ', 21, '#general ', '<#chan1> ', 'channel', 'chan1');
		});

		it('should convert substring with one segment', () => {
			const text = 'Hey @User#0001 check #general and more';

			const result = manager.displayToActualSubstring(text, 4, 15);
			expect(result).toBe('<@123> ');
		});

		it('should convert substring with multiple segments', () => {
			const text = 'Hey @User#0001 check #general and more';

			const result = manager.displayToActualSubstring(text, 4, 30);
			expect(result).toBe('<@123> check <#chan1> ');
		});

		it('should handle substring with no segments', () => {
			const text = 'Hey @User#0001 check #general and more';

			const result = manager.displayToActualSubstring(text, 0, 4);
			expect(result).toBe('Hey ');
		});

		it('should handle substring that partially overlaps segments', () => {
			const text = 'Hey @User#0001 check #general and more';

			const result = manager.displayToActualSubstring(text, 8, 20);
			expect(result).toBe('r#0001 check');
		});

		it('should handle full text selection', () => {
			const text = 'Hey @User#0001 check #general and more';

			const result = manager.displayToActualSubstring(text, 0, text.length);
			expect(result).toBe('Hey <@123> check <#chan1> and more');
		});

		it('should work with consecutive mentions selection', () => {
			manager.clear();
			manager.insertSegment('', 0, '@User1#0001 ', '<@123> ', 'user', '123');
			manager.insertSegment('@User1#0001 ', 12, '@User2#0002 ', '<@456> ', 'user', '456');

			const text = '@User1#0001 @User2#0002 text';

			const result = manager.displayToActualSubstring(text, 0, 24);
			expect(result).toBe('<@123> <@456> ');
		});
	});

	describe('integration scenarios', () => {
		it('should handle consecutive mentions insertion via autocomplete', () => {
			let displayText = '';
			const {newText: after1} = manager.insertSegment(displayText, 0, '@User#0001 ', '<@123> ', 'user', '123');
			displayText = after1;

			expect(displayText).toBe('@User#0001 ');
			expect(manager.getSegments()).toHaveLength(1);

			const {newText: after2} = manager.insertSegment(
				displayText,
				displayText.length,
				'@Other#0002 ',
				'<@456> ',
				'user',
				'456',
			);
			displayText = after2;

			expect(displayText).toBe('@User#0001 @Other#0002 ');
			const segments = manager.getSegments();
			expect(segments).toHaveLength(2);
			expect(segments[0]).toMatchObject({
				id: '123',
				start: 0,
				end: 11,
			});
			expect(segments[1]).toMatchObject({
				id: '456',
				start: 11,
				end: 23,
			});

			const actualContent = manager.displayToActual(displayText);
			expect(actualContent).toBe('<@123> <@456> ');
		});

		it('should handle three consecutive mentions', () => {
			let displayText = '';

			const {newText: after1} = manager.insertSegment(displayText, 0, '@User1#0001 ', '<@123> ', 'user', '123');
			displayText = after1;

			const {newText: after2} = manager.insertSegment(
				displayText,
				displayText.length,
				'@User2#0002 ',
				'<@456> ',
				'user',
				'456',
			);
			displayText = after2;

			const {newText: after3} = manager.insertSegment(
				displayText,
				displayText.length,
				'@User3#0003 ',
				'<@789> ',
				'user',
				'789',
			);
			displayText = after3;

			expect(displayText).toBe('@User1#0001 @User2#0002 @User3#0003 ');
			const segments = manager.getSegments();
			expect(segments).toHaveLength(3);

			const actualContent = manager.displayToActual(displayText);
			expect(actualContent).toBe('<@123> <@456> <@789> ');
		});
	});
});
