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
import {TextareaSegmentManager} from './TextareaSegmentManager';

describe('Textarea INSERT_MENTION Flow Integration', () => {
	let manager: TextareaSegmentManager;

	beforeEach(() => {
		manager = new TextareaSegmentManager();
	});

	function simulateInsertMention(currentValue: string, userTag: string, userId: string): string {
		const actualText = `<@${userId}>`;
		const displayText = `@${userTag}`;
		const needsSpace = currentValue.length > 0 && !currentValue.endsWith(' ');
		const prefix = currentValue.length === 0 ? '' : needsSpace ? ' ' : '';
		const insertPosition = currentValue.length + prefix.length;

		const {newText} = manager.insertSegment(
			currentValue + prefix,
			insertPosition,
			displayText,
			actualText,
			'user',
			userId,
		);

		return newText;
	}

	it('should handle first INSERT_MENTION', () => {
		const result = simulateInsertMention('', 'Hampus#0001', '123');

		expect(result).toBe('@Hampus#0001');
		expect(manager.getSegments()).toHaveLength(1);
		expect(manager.displayToActual(result)).toBe('<@123>');
	});

	it('should handle two consecutive INSERT_MENTION calls', () => {
		let value = '';

		value = simulateInsertMention(value, 'Hampus#0001', '123');
		expect(value).toBe('@Hampus#0001');

		value += ' ';

		value = simulateInsertMention(value, 'Hampus#0001', '123');
		expect(value).toBe('@Hampus#0001 @Hampus#0001');

		const segments = manager.getSegments();
		expect(segments).toHaveLength(2);
		expect(segments[0]).toMatchObject({id: '123', start: 0, end: 12});
		expect(segments[1]).toMatchObject({id: '123', start: 13, end: 25});
		expect(manager.displayToActual(value)).toBe('<@123> <@123>');
	});

	it('should handle INSERT_MENTION without manually adding space between', () => {
		let value = '';

		value = simulateInsertMention(value, 'Hampus#0001', '123');

		value = simulateInsertMention(value, 'Hampus#0001', '123');

		expect(value).toBe('@Hampus#0001 @Hampus#0001');
		const segments = manager.getSegments();
		expect(segments).toHaveLength(2);
		expect(manager.displayToActual(value)).toBe('<@123> <@123>');
	});

	it('should handle three consecutive INSERT_MENTION calls', () => {
		let value = '';

		value = simulateInsertMention(value, 'Hampus#0001', '123');
		value = simulateInsertMention(value, 'Hampus#0001', '123');
		value = simulateInsertMention(value, 'Hampus#0001', '123');

		expect(value).toBe('@Hampus#0001 @Hampus#0001 @Hampus#0001');
		const segments = manager.getSegments();
		expect(segments).toHaveLength(3);
		expect(manager.displayToActual(value)).toBe('<@123> <@123> <@123>');
	});

	it('should handle INSERT_MENTION with text changes in between via handleTextChange', () => {
		let value = '';

		value = simulateInsertMention(value, 'Hampus#0001', '123');

		const oldValue = value;
		value = `${value} hello`;
		const {changeStart, changeEnd, replacementLength} = TextareaSegmentManager.detectChange(oldValue, value);
		manager.updateSegmentsForTextChange(changeStart, changeEnd, replacementLength);

		value = simulateInsertMention(value, 'Other#0002', '456');

		expect(value).toBe('@Hampus#0001 hello @Other#0002');
		const segments = manager.getSegments();
		expect(segments).toHaveLength(2);
		expect(manager.displayToActual(value)).toBe('<@123> hello <@456>');
	});
});
