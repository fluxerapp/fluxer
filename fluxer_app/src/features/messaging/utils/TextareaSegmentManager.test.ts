// SPDX-License-Identifier: AGPL-3.0-or-later

import {type MentionSegment, TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {describe, expect, it} from 'vitest';

function userSegment(overrides: Partial<MentionSegment> = {}): MentionSegment {
	return {
		type: 'user',
		id: '1000',
		displayText: '@Zacy',
		actualText: '<@1000>',
		start: 0,
		end: 5,
		...overrides,
	};
}

describe('TextareaSegmentManager', () => {
	it('uses the pre-edit selection to keep mention segments when inserting matching text before them', () => {
		const manager = new TextareaSegmentManager();
		manager.setSegments([userSegment()]);

		const oldValue = '@Zacy ';
		const newValue = '@@Zacy ';
		const change = TextareaSegmentManager.detectChange(oldValue, newValue, {
			selectionStart: 0,
			selectionEnd: 0,
			inputType: 'insertText',
		});

		manager.updateSegmentsForTextChange(change.changeStart, change.changeEnd, change.replacementLength);

		expect(change).toEqual({changeStart: 0, changeEnd: 0, replacementLength: 1});
		expect(manager.getSegments()).toEqual([userSegment({start: 1, end: 6})]);
		expect(manager.displayToActual(newValue)).toBe('@<@1000> ');
	});

	it('falls back to text diffing when no selection hint is available', () => {
		const change = TextareaSegmentManager.detectChange('@Zacy ', '@@Zacy ');

		expect(change).toEqual({changeStart: 1, changeEnd: 1, replacementLength: 1});
	});
});
