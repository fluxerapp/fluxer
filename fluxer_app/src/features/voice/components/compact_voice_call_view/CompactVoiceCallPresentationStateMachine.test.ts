// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CompactVoiceCallPresentationInput,
	type CompactVoiceCallPresentationMachineSnapshot,
	createCompactVoiceCallPresentationSnapshot,
	getCompactVoiceCallPresentationStateValue,
	resolveCompactVoiceCallPresentationModel,
	selectCompactVoiceCallPresentationModel,
	transitionCompactVoiceCallPresentationSnapshot,
} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallPresentationStateMachine';
import {describe, expect, it} from 'vitest';

function update(
	snapshot: CompactVoiceCallPresentationMachineSnapshot,
	input: CompactVoiceCallPresentationInput,
): CompactVoiceCallPresentationMachineSnapshot {
	return transitionCompactVoiceCallPresentationSnapshot(snapshot, {
		type: 'presentation.update',
		input,
	});
}

describe('CompactVoiceCallPresentationStateMachine', () => {
	it('renders compact call layout when live media is available', () => {
		const model = resolveCompactVoiceCallPresentationModel({
			hasRenderableCallMedia: true,
			showAvatarFallback: true,
		});
		expect(model).toEqual({
			stateValue: 'compactCallLayout',
			shouldRenderCallLayout: true,
			useFullHeightCallLayout: false,
			shouldShowAvatarFallback: false,
			shouldForceFloatingHudVisible: false,
		});
	});

	it('renders full-height call layout before media arrives when fillHeight owns the available space', () => {
		const snapshot = createCompactVoiceCallPresentationSnapshot({
			fillHeight: true,
			hasRenderableCallMedia: false,
			showAvatarFallback: true,
		});
		expect(getCompactVoiceCallPresentationStateValue(snapshot)).toBe('fullHeightCallLayout');
		expect(selectCompactVoiceCallPresentationModel(snapshot)).toMatchObject({
			shouldRenderCallLayout: true,
			useFullHeightCallLayout: true,
			shouldShowAvatarFallback: false,
		});
	});

	it('keeps audio-only calls on avatar fallback even when tracks or fillHeight are present', () => {
		const model = resolveCompactVoiceCallPresentationModel({
			audioOnly: true,
			fillHeight: true,
			hasRenderableCallMedia: true,
			showAvatarFallback: true,
		});
		expect(model.stateValue).toBe('avatarFallback');
		expect(model.shouldRenderCallLayout).toBe(false);
		expect(model.shouldShowAvatarFallback).toBe(true);
		expect(model.shouldForceFloatingHudVisible).toBe(true);
	});

	it('renders no media content when neither call layout nor avatar fallback is allowed', () => {
		const model = resolveCompactVoiceCallPresentationModel({
			hasRenderableCallMedia: false,
			showAvatarFallback: false,
		});
		expect(model).toEqual({
			stateValue: 'empty',
			shouldRenderCallLayout: false,
			useFullHeightCallLayout: false,
			shouldShowAvatarFallback: false,
			shouldForceFloatingHudVisible: false,
		});
	});

	it('reroutes when presentation inputs change', () => {
		let snapshot = createCompactVoiceCallPresentationSnapshot({
			showAvatarFallback: true,
			hasRenderableCallMedia: false,
		});
		expect(getCompactVoiceCallPresentationStateValue(snapshot)).toBe('avatarFallback');
		snapshot = update(snapshot, {
			showAvatarFallback: true,
			hasRenderableCallMedia: true,
		});
		expect(getCompactVoiceCallPresentationStateValue(snapshot)).toBe('compactCallLayout');
		snapshot = update(snapshot, {
			audioOnly: true,
			showAvatarFallback: false,
			hasRenderableCallMedia: true,
		});
		expect(selectCompactVoiceCallPresentationModel(snapshot)).toMatchObject({
			stateValue: 'empty',
			shouldRenderCallLayout: false,
			shouldShowAvatarFallback: false,
		});
	});
});
