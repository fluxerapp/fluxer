// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceCallLayoutPresentationSnapshot,
	getVoiceCallLayoutPresentationStateValue,
	resolveVoiceCallLayoutPresentationModel,
	selectVoiceCallLayoutPresentationModel,
	transitionVoiceCallLayoutPresentationSnapshot,
	type VoiceCallLayoutPresentationInput,
	type VoiceCallLayoutPresentationMachineSnapshot,
} from '@app/features/voice/components/VoiceCallLayoutPresentationStateMachine';
import {describe, expect, it} from 'vitest';

function input(overrides: Partial<VoiceCallLayoutPresentationInput> = {}): VoiceCallLayoutPresentationInput {
	return {
		layoutMode: 'focus',
		hasFocusMainTrack: true,
		secondaryFocusTrackCount: 1,
		compact: true,
		isVoiceCallAppFullscreen: true,
		membersRowVisible: false,
		hasScreenShare: false,
		...overrides,
	};
}

function toggle(snapshot: VoiceCallLayoutPresentationMachineSnapshot): VoiceCallLayoutPresentationMachineSnapshot {
	return transitionVoiceCallLayoutPresentationSnapshot(snapshot, {type: 'participants.toggle'});
}

function update(
	snapshot: VoiceCallLayoutPresentationMachineSnapshot,
	nextInput: VoiceCallLayoutPresentationInput,
): VoiceCallLayoutPresentationMachineSnapshot {
	return transitionVoiceCallLayoutPresentationSnapshot(snapshot, {type: 'presentation.update', input: nextInput});
}

describe('VoiceCallLayoutPresentationStateMachine', () => {
	it('models the compact fullscreen participant grid as collapsed until toggled open', () => {
		let snapshot = createVoiceCallLayoutPresentationSnapshot(input());
		expect(getVoiceCallLayoutPresentationStateValue(snapshot)).toBe('focusLayout');
		expect(selectVoiceCallLayoutPresentationModel(snapshot)).toMatchObject({
			canShowParticipantsGridPanel: true,
			participantsGridExpanded: false,
			isParticipantsExpanded: false,
			focusScrollerOverflow: 'hidden',
		});

		snapshot = toggle(snapshot);
		expect(getVoiceCallLayoutPresentationStateValue(snapshot)).toBe('focusParticipantsExpanded');
		expect(selectVoiceCallLayoutPresentationModel(snapshot)).toMatchObject({
			canShowParticipantsGridPanel: true,
			participantsGridExpanded: true,
			isParticipantsExpanded: true,
			showMembersRow: false,
			showMembersToggle: false,
			focusScrollerOverflow: 'auto',
		});
	});

	it('collapses expanded participants when the panel loses capability', () => {
		let snapshot = toggle(createVoiceCallLayoutPresentationSnapshot(input()));
		expect(selectVoiceCallLayoutPresentationModel(snapshot).isParticipantsExpanded).toBe(true);

		snapshot = update(snapshot, input({secondaryFocusTrackCount: 0}));
		expect(selectVoiceCallLayoutPresentationModel(snapshot)).toMatchObject({
			stateValue: 'focusLayout',
			canShowParticipantsGridPanel: false,
			participantsGridExpanded: false,
			isParticipantsExpanded: false,
		});

		snapshot = update(snapshot, input());
		expect(selectVoiceCallLayoutPresentationModel(snapshot)).toMatchObject({
			canShowParticipantsGridPanel: true,
			participantsGridExpanded: false,
			isParticipantsExpanded: false,
		});
	});

	it('preserves participant expansion while grid layout is selected without losing panel capability', () => {
		let snapshot = toggle(createVoiceCallLayoutPresentationSnapshot(input()));
		snapshot = update(snapshot, input({layoutMode: 'grid'}));
		expect(selectVoiceCallLayoutPresentationModel(snapshot)).toMatchObject({
			stateValue: 'gridLayout',
			mainLayout: 'grid',
			canShowParticipantsGridPanel: true,
			participantsGridExpanded: true,
			isParticipantsExpanded: false,
		});

		snapshot = update(snapshot, input({layoutMode: 'focus'}));
		expect(selectVoiceCallLayoutPresentationModel(snapshot)).toMatchObject({
			stateValue: 'focusParticipantsExpanded',
			mainLayout: 'focus',
			participantsGridExpanded: true,
			isParticipantsExpanded: true,
		});
	});

	it('selects the desktop fullscreen members row and toggle from secondary-track state', () => {
		expect(
			resolveVoiceCallLayoutPresentationModel(
				input({
					compact: false,
					membersRowVisible: true,
				}),
			),
		).toMatchObject({
			isFullscreenFocusLayout: true,
			hasSecondaryFocusTracks: true,
			canShowParticipantsGridPanel: false,
			showMembersRow: true,
			showMembersToggle: true,
		});

		expect(
			resolveVoiceCallLayoutPresentationModel(
				input({
					compact: false,
					membersRowVisible: false,
				}),
			),
		).toMatchObject({
			showMembersRow: false,
			showMembersToggle: true,
		});
	});

	it('falls back to the grid when focus has no main track and secondary tracks cannot render', () => {
		expect(
			resolveVoiceCallLayoutPresentationModel(
				input({
					hasFocusMainTrack: false,
					secondaryFocusTrackCount: 2,
					compact: false,
					isVoiceCallAppFullscreen: false,
				}),
			),
		).toMatchObject({
			stateValue: 'focusFallbackGrid',
			shouldRenderFocusFallbackGrid: true,
		});

		expect(
			resolveVoiceCallLayoutPresentationModel(
				input({
					hasFocusMainTrack: false,
					secondaryFocusTrackCount: 2,
					compact: true,
					isVoiceCallAppFullscreen: false,
				}),
			),
		).toMatchObject({
			stateValue: 'focusFallbackGrid',
			shouldRenderFocusFallbackGrid: true,
		});

		expect(
			resolveVoiceCallLayoutPresentationModel(
				input({
					hasFocusMainTrack: false,
					secondaryFocusTrackCount: 2,
					compact: false,
					isVoiceCallAppFullscreen: true,
				}),
			),
		).toMatchObject({
			stateValue: 'focusLayout',
			shouldRenderFocusFallbackGrid: false,
		});

		expect(
			resolveVoiceCallLayoutPresentationModel(
				input({
					hasFocusMainTrack: false,
					secondaryFocusTrackCount: 2,
					compact: true,
					isVoiceCallAppFullscreen: true,
				}),
			),
		).toMatchObject({
			stateValue: 'focusLayout',
			shouldRenderFocusFallbackGrid: false,
		});
	});

	it('selects fallback grid and screen-share wrapping conditions', () => {
		expect(
			resolveVoiceCallLayoutPresentationModel(
				input({
					hasFocusMainTrack: false,
					secondaryFocusTrackCount: 0,
				}),
			),
		).toMatchObject({
			stateValue: 'focusFallbackGrid',
			mainLayout: 'focus',
			shouldRenderFocusFallbackGrid: true,
			shouldRenderFocusLayout: false,
		});

		expect(
			resolveVoiceCallLayoutPresentationModel(
				input({
					layoutMode: 'grid',
					hasScreenShare: true,
				}),
			),
		).toMatchObject({
			stateValue: 'gridLayout',
			mainLayout: 'grid',
			shouldWrapScreenShareGrid: true,
		});
	});
});
