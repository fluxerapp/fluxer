// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	filterRoutableLinuxAudioSources,
	getLinuxAudioSourceDisplayName,
} from '@app/features/voice/utils/LinuxAudioSourceRules';
import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	resolveWindowShareAudioScope,
	type ScreenShareAudioSourceMode,
	type StreamSettingsShareContext,
	selectAppShareAudioRoute,
	supportsWindowShareAudioScope,
	type WindowShareAudioScope,
} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import type {VirtmicNode} from '@app/types/electron.d';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const NO_AUDIO_DESCRIPTOR = msg({
	message: 'No audio',
	comment: 'Screen-share audio summary shown when the share publishes no audio at all.',
});
export const CUSTOM_SOURCES_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Screen-share audio summary shown when one audio source is selected but it has no readable name.',
});
export const APP_COUNT_DESCRIPTOR = msg({
	message: '{length} apps',
	comment: 'Screen-share audio summary listing how many apps are captured. {length} is the integer app count.',
});
export const ENTIRE_SYSTEM_DESCRIPTOR = msg({
	message: 'Entire system',
	comment: 'Screen-share audio summary shown when the whole system audio mix is captured.',
});
export const SHARED_WINDOW_DESCRIPTOR = msg({
	message: 'Shared window',
	comment: 'Screen-share audio summary shown when a window share captures only the audio of the window it shares.',
});
export const MICROPHONE_DESCRIPTOR = msg({
	message: 'Microphone',
	comment: 'Screen-share audio summary on a video device share whose audio comes from an unnamed microphone.',
});
export const MICROPHONE_WITH_DEVICE_DESCRIPTOR = msg({
	message: 'Microphone ({deviceLabel})',
	comment:
		'Screen-share audio summary on a video device share. {deviceLabel} is the name of the selected audio input device.',
});

export interface ScreenShareAudioSummaryInput {
	sourceMode: ScreenShareAudioSourceMode;
	includeSources: ReadonlyArray<VirtmicNode>;
	shareContext: StreamSettingsShareContext;
	microphoneLabel?: string | null;
	displayShareEnvironment?: DisplayShareEnvironment;
	windowAudioScope?: WindowShareAudioScope;
}

export type ScreenShareAudioSummary =
	| {readonly kind: 'sourceName'; readonly name: string}
	| {
			readonly kind: 'message';
			readonly descriptor: MessageDescriptor;
			readonly values?: Record<string, string | number>;
	  };

function summariseSelectedSources(selected: ReadonlyArray<VirtmicNode>): ScreenShareAudioSummary {
	if (selected.length === 1) {
		const name = getLinuxAudioSourceDisplayName(selected[0]);
		return name == null ? {kind: 'message', descriptor: CUSTOM_SOURCES_DESCRIPTOR} : {kind: 'sourceName', name};
	}
	return {kind: 'message', descriptor: APP_COUNT_DESCRIPTOR, values: {length: selected.length}};
}

function summariseMicrophone(microphoneLabel?: string | null): ScreenShareAudioSummary {
	if (microphoneLabel == null || microphoneLabel === '') {
		return {kind: 'message', descriptor: MICROPHONE_DESCRIPTOR};
	}
	return {kind: 'message', descriptor: MICROPHONE_WITH_DEVICE_DESCRIPTOR, values: {deviceLabel: microphoneLabel}};
}

export function resolveScreenShareAudioSummary(input: ScreenShareAudioSummaryInput): ScreenShareAudioSummary {
	const selected = filterRoutableLinuxAudioSources(input.includeSources);
	const routesSelectedSources = input.sourceMode === 'specific' && selected.length > 0;
	if (input.shareContext === 'device') {
		return routesSelectedSources ? summariseSelectedSources(selected) : summariseMicrophone(input.microphoneLabel);
	}
	if (supportsWindowShareAudioScope(input)) {
		const route = selectAppShareAudioRoute({
			audioSourceMode: input.sourceMode,
			selectedSourceCount: selected.length,
			windowAudioScope: resolveWindowShareAudioScope(input),
		});
		if (route === 'window') return {kind: 'message', descriptor: SHARED_WINDOW_DESCRIPTOR};
		if (route === 'none') return {kind: 'message', descriptor: NO_AUDIO_DESCRIPTOR};
		if (route === 'apps') return summariseSelectedSources(selected);
		return {kind: 'message', descriptor: ENTIRE_SYSTEM_DESCRIPTOR};
	}
	if (input.sourceMode === 'none') return {kind: 'message', descriptor: NO_AUDIO_DESCRIPTOR};
	if (routesSelectedSources) return summariseSelectedSources(selected);
	return {kind: 'message', descriptor: ENTIRE_SYSTEM_DESCRIPTOR};
}

export function formatScreenShareAudioSummary(i18n: I18n, input: ScreenShareAudioSummaryInput): string {
	const summary = resolveScreenShareAudioSummary(input);
	if (summary.kind === 'sourceName') return summary.name;
	return i18n._(summary.descriptor, summary.values);
}
