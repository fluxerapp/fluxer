// SPDX-License-Identifier: AGPL-3.0-or-later

import {hasVisibleMessageContent} from '@app/features/messaging/utils/VisibleMessageContent';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export type MobileTextareaVisibleButton = 'send' | 'voice';
export type MobileTextareaButtonMode = 'voice' | 'sendReady' | 'sendBlocked';

export interface MobileTextareaButtonSignals {
	disabled: boolean;
	canRecordVoice: boolean;
	value: string;
	isSlowmodeActive: boolean;
	isOverCharacterLimit: boolean;
	isEditingMessage: boolean;
	hasContent: boolean;
	hasAttachments: boolean;
	hasPendingSticker: boolean;
}

export interface MobileTextareaButtonModel {
	mode: MobileTextareaButtonMode;
	visibleButton: MobileTextareaVisibleButton;
	sendButton: {
		disabled: boolean;
	};
	voiceButton: {
		disabled: boolean;
	};
	hasTypedText: boolean;
	hasTextContent: boolean;
	hasSubmissionContent: boolean;
}

export type MobileTextareaButtonEvent = {
	type: 'mobileTextareaButtons.evaluate';
	signals: MobileTextareaButtonSignals;
};

interface MobileTextareaButtonContext {
	signals: MobileTextareaButtonSignals;
	model: MobileTextareaButtonModel;
}

function hasTypedText(value: string): boolean {
	return hasVisibleMessageContent(value.trim());
}

export function selectMobileTextareaButtonModel(signals: MobileTextareaButtonSignals): MobileTextareaButtonModel {
	const typedText = hasTypedText(signals.value);
	const hasTextContent = signals.hasContent || typedText;
	const hasSubmissionContent = hasTextContent || signals.hasAttachments || signals.hasPendingSticker;
	const shouldShowVoice = signals.canRecordVoice && !signals.isEditingMessage && !hasSubmissionContent;
	const sendDisabled =
		signals.disabled ||
		signals.isOverCharacterLimit ||
		(signals.isSlowmodeActive && !signals.isEditingMessage) ||
		(!hasSubmissionContent && !signals.isEditingMessage);
	const voiceDisabled = signals.disabled || signals.isSlowmodeActive || signals.isOverCharacterLimit;
	const visibleButton = shouldShowVoice ? 'voice' : 'send';
	const mode = visibleButton === 'voice' ? 'voice' : sendDisabled ? 'sendBlocked' : 'sendReady';
	return {
		mode,
		visibleButton,
		sendButton: {disabled: sendDisabled},
		voiceButton: {disabled: voiceDisabled},
		hasTypedText: typedText,
		hasTextContent,
		hasSubmissionContent,
	};
}

const DEFAULT_SIGNALS: MobileTextareaButtonSignals = {
	disabled: false,
	canRecordVoice: true,
	value: '',
	isSlowmodeActive: false,
	isOverCharacterLimit: false,
	isEditingMessage: false,
	hasContent: false,
	hasAttachments: false,
	hasPendingSticker: false,
};
const DEFAULT_MODEL = selectMobileTextareaButtonModel(DEFAULT_SIGNALS);

const evaluateTransitions = [
	{
		guard: 'isVoiceMode',
		target: 'voice',
		actions: 'assignModel',
	},
	{
		guard: 'isSendReadyMode',
		target: 'sendReady',
		actions: 'assignModel',
	},
	{
		target: 'sendBlocked',
		actions: 'assignModel',
	},
] as const;

export const mobileTextareaButtonStateMachine = setup({
	types: {} as {
		context: MobileTextareaButtonContext;
		events: MobileTextareaButtonEvent;
	},
	guards: {
		isVoiceMode: ({event}) => selectMobileTextareaButtonModel(event.signals).mode === 'voice',
		isSendReadyMode: ({event}) => selectMobileTextareaButtonModel(event.signals).mode === 'sendReady',
	},
	actions: {
		assignModel: assign(({event}) => ({
			signals: event.signals,
			model: selectMobileTextareaButtonModel(event.signals),
		})),
	},
}).createMachine({
	id: 'mobileTextareaButtons',
	context: () => ({
		signals: DEFAULT_SIGNALS,
		model: DEFAULT_MODEL,
	}),
	initial: DEFAULT_MODEL.mode,
	states: {
		voice: {
			on: {
				'mobileTextareaButtons.evaluate': evaluateTransitions,
			},
		},
		sendReady: {
			on: {
				'mobileTextareaButtons.evaluate': evaluateTransitions,
			},
		},
		sendBlocked: {
			on: {
				'mobileTextareaButtons.evaluate': evaluateTransitions,
			},
		},
	},
});

export type MobileTextareaButtonSnapshot = SnapshotFrom<typeof mobileTextareaButtonStateMachine>;

export function createMobileTextareaButtonSnapshot(): MobileTextareaButtonSnapshot {
	return getInitialSnapshot(mobileTextareaButtonStateMachine);
}

export function transitionMobileTextareaButtonSnapshot(
	snapshot: MobileTextareaButtonSnapshot,
	event: MobileTextareaButtonEvent,
): MobileTextareaButtonSnapshot {
	return transition(mobileTextareaButtonStateMachine, snapshot, event)[0] as MobileTextareaButtonSnapshot;
}

export function transitionMobileTextareaButtonState(signals: MobileTextareaButtonSignals): MobileTextareaButtonModel {
	const snapshot = transitionMobileTextareaButtonSnapshot(createMobileTextareaButtonSnapshot(), {
		type: 'mobileTextareaButtons.evaluate',
		signals,
	});
	return snapshot.context.model;
}
