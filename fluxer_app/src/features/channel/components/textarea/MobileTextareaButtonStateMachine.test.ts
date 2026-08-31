// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type MobileTextareaButtonSignals,
	transitionMobileTextareaButtonState,
} from '@app/features/channel/components/textarea/MobileTextareaButtonStateMachine';
import {describe, expect, it} from 'vitest';

function signals(overrides: Partial<MobileTextareaButtonSignals> = {}): MobileTextareaButtonSignals {
	return {
		disabled: false,
		canRecordVoice: true,
		value: '',
		isSlowmodeActive: false,
		isOverCharacterLimit: false,
		isEditingMessage: false,
		hasContent: false,
		hasAttachments: false,
		hasPendingSticker: false,
		...overrides,
	};
}

function state(overrides: Partial<MobileTextareaButtonSignals> = {}) {
	return transitionMobileTextareaButtonState(signals(overrides));
}

describe('MobileTextareaButtonStateMachine', () => {
	it('shows the voice button for an empty normal composer', () => {
		expect(state()).toMatchObject({
			mode: 'voice',
			visibleButton: 'voice',
			sendButton: {disabled: true},
			voiceButton: {disabled: false},
			hasSubmissionContent: false,
		});
	});

	it('shows an enabled send button as soon as raw visible textarea text exists', () => {
		expect(state({value: 'hello', hasContent: false})).toMatchObject({
			mode: 'sendReady',
			visibleButton: 'send',
			sendButton: {disabled: false},
			hasTypedText: true,
			hasTextContent: true,
			hasSubmissionContent: true,
		});
	});

	it('keeps the voice button for whitespace and invisible-only textarea content', () => {
		expect(state({value: ' \n\t\u200b'})).toMatchObject({
			mode: 'voice',
			visibleButton: 'voice',
			hasTypedText: false,
			hasSubmissionContent: false,
		});
	});

	it('shows send for attachment and sticker payloads without text', () => {
		expect(state({hasAttachments: true})).toMatchObject({
			mode: 'sendReady',
			visibleButton: 'send',
			sendButton: {disabled: false},
			hasSubmissionContent: true,
		});
		expect(state({hasPendingSticker: true})).toMatchObject({
			mode: 'sendReady',
			visibleButton: 'send',
			sendButton: {disabled: false},
			hasSubmissionContent: true,
		});
	});

	it('shows send while blocked by slowmode or character limits', () => {
		expect(state({value: 'hello', isSlowmodeActive: true})).toMatchObject({
			mode: 'sendBlocked',
			visibleButton: 'send',
			sendButton: {disabled: true},
		});
		expect(state({value: 'hello', isOverCharacterLimit: true})).toMatchObject({
			mode: 'sendBlocked',
			visibleButton: 'send',
			sendButton: {disabled: true},
		});
	});

	it('keeps edit mode on send even when the edited content is empty', () => {
		expect(state({isEditingMessage: true})).toMatchObject({
			mode: 'sendReady',
			visibleButton: 'send',
			sendButton: {disabled: false},
		});
	});

	it('falls back to a disabled send button when voice recording is unavailable and the composer is empty', () => {
		expect(state({canRecordVoice: false})).toMatchObject({
			mode: 'sendBlocked',
			visibleButton: 'send',
			sendButton: {disabled: true},
		});
	});
});
