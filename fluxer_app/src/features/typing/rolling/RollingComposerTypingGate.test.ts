// SPDX-License-Identifier: AGPL-3.0-or-later

import {decideComposerTyping} from '@app/features/typing/rolling/RollingComposerTypingGate';
import {describe, expect, it} from 'vitest';

type ComposerTypingChange = Parameters<typeof decideComposerTyping>[0];

function decide(change: Partial<ComposerTypingChange>): ReturnType<typeof decideComposerTyping> {
	return decideComposerTyping({
		previousValue: 'hell',
		value: 'hello',
		isRestoringDraft: false,
		isEditingMessageInComposer: false,
		enabled: true,
		...change,
	});
}

describe('decideComposerTyping', () => {
	it('starts typing when the value changes to non-empty text', () => {
		expect(decide({})).toBe('start');
		expect(decide({previousValue: '', value: 'h'})).toBe('start');
		expect(decide({previousValue: 'hello', value: 'hell'})).toBe('start');
	});

	it('stops typing for a whitespace-only draft', () => {
		expect(decide({previousValue: '', value: '   '})).toBe('stop');
		expect(decide({previousValue: 'hi', value: ' \n\t'})).toBe('stop');
	});

	it('stops typing when the value becomes exactly empty', () => {
		expect(decide({previousValue: 'h', value: ''})).toBe('stop');
	});

	it('does nothing when the value is unchanged', () => {
		expect(decide({previousValue: 'hello', value: 'hello'})).toBe('none');
		expect(decide({previousValue: '', value: ''})).toBe('none');
	});

	it('does nothing on the first run after mount', () => {
		expect(decide({previousValue: null, value: 'saved draft'})).toBe('none');
	});

	it('does nothing when the channel changed since the previous value', () => {
		expect(decide({previousValue: null, value: ''})).toBe('none');
		expect(decide({previousValue: null, value: 'draft in the next channel'})).toBe('none');
	});

	it('does nothing while a draft is being restored', () => {
		expect(decide({isRestoringDraft: true})).toBe('none');
		expect(decide({isRestoringDraft: true, value: ''})).toBe('none');
	});

	it('does nothing while editing a message in the composer', () => {
		expect(decide({isEditingMessageInComposer: true})).toBe('none');
		expect(decide({isEditingMessageInComposer: true, value: ''})).toBe('none');
	});

	it('does nothing while the composer is disabled', () => {
		expect(decide({enabled: false})).toBe('none');
		expect(decide({enabled: false, value: ''})).toBe('none');
	});

	it('suppresses a leading slash', () => {
		expect(decide({previousValue: '', value: '/'})).toBe('none');
		expect(decide({previousValue: '/gif', value: '/gif cats'})).toBe('none');
	});

	it('suppresses a bare plus', () => {
		expect(decide({previousValue: '', value: '+'})).toBe('none');
		expect(decide({previousValue: '+', value: '+1'})).toBe('start');
	});

	it('suppresses the plus colon emoji shorthand', () => {
		expect(decide({previousValue: '+', value: '+:'})).toBe('none');
		expect(decide({previousValue: '+:thumbsup', value: '+:thumbsup:'})).toBe('none');
		expect(decide({previousValue: '+:thumbs', value: '+:thumbs-up'})).toBe('none');
	});

	it('suppresses the reaction shorthand with leading whitespace', () => {
		expect(decide({previousValue: '  +:', value: '  +:smile'})).toBe('none');
	});

	it('suppresses a replace command', () => {
		expect(decide({previousValue: 's/teh/th', value: 's/teh/the/'})).toBe('none');
		expect(decide({previousValue: ' s/teh/th', value: ' s/teh/the '})).toBe('none');
	});

	it('starts typing while autocomplete is open', () => {
		expect(decide({previousValue: '@al', value: '@ali'})).toBe('start');
		expect(decide({previousValue: 'nice :smi', value: 'nice :smil'})).toBe('start');
		expect(decide({previousValue: '#gen', value: '#gene'})).toBe('start');
	});
});
