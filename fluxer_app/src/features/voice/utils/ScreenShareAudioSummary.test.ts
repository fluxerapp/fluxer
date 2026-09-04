// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

const {
	APP_COUNT_DESCRIPTOR,
	CUSTOM_SOURCES_DESCRIPTOR,
	ENTIRE_SYSTEM_DESCRIPTOR,
	MICROPHONE_DESCRIPTOR,
	MICROPHONE_WITH_DEVICE_DESCRIPTOR,
	NO_AUDIO_DESCRIPTOR,
	resolveScreenShareAudioSummary,
	SHARED_WINDOW_DESCRIPTOR,
} = await import('./ScreenShareAudioSummary');
type ScreenShareAudioSummaryInput = Parameters<typeof resolveScreenShareAudioSummary>[0];

function summary(overrides: Partial<ScreenShareAudioSummaryInput> = {}) {
	return resolveScreenShareAudioSummary({
		sourceMode: 'system',
		includeSources: [],
		shareContext: 'display',
		displayShareEnvironment: 'desktop-custom',
		...overrides,
	});
}

describe('resolveScreenShareAudioSummary', () => {
	it('says no audio on a display share once the sources are switched off', () => {
		expect(summary({shareContext: 'display', sourceMode: 'none'})).toEqual({
			kind: 'message',
			descriptor: NO_AUDIO_DESCRIPTOR,
		});
		expect(summary({shareContext: 'app', windowAudioScope: 'system', sourceMode: 'none'})).toEqual({
			kind: 'message',
			descriptor: NO_AUDIO_DESCRIPTOR,
		});
	});

	it('never says no audio on a device share, whose microphone the source mode cannot switch off', () => {
		expect(summary({shareContext: 'device', sourceMode: 'none', microphoneLabel: 'Yeti'})).toEqual({
			kind: 'message',
			descriptor: MICROPHONE_WITH_DEVICE_DESCRIPTOR,
			values: {deviceLabel: 'Yeti'},
		});
	});

	it('never says no audio on a window share it still captures the window of', () => {
		expect(summary({shareContext: 'app', sourceMode: 'none', windowAudioScope: 'window'})).toEqual({
			kind: 'message',
			descriptor: SHARED_WINDOW_DESCRIPTOR,
		});
	});

	it('names the single picked application in every share context, capture cards included', () => {
		for (const shareContext of ['device', 'display'] as const) {
			expect(
				summary({
					shareContext,
					sourceMode: 'specific',
					includeSources: [{'application.name': 'mpv'}],
					microphoneLabel: 'Yeti',
				}),
			).toEqual({kind: 'sourceName', name: 'mpv'});
		}
		expect(
			summary({
				shareContext: 'app',
				windowAudioScope: 'system',
				sourceMode: 'specific',
				includeSources: [{'application.name': 'mpv'}],
			}),
		).toEqual({kind: 'sourceName', name: 'mpv'});
	});

	it('reads a selection the capture layer cannot express as the scope actually armed', () => {
		expect(summary({sourceMode: 'specific', includeSources: [{'fluxer.display.name': 'mpv'}]})).toEqual({
			kind: 'message',
			descriptor: ENTIRE_SYSTEM_DESCRIPTOR,
		});
		expect(
			summary({
				shareContext: 'app',
				windowAudioScope: 'system',
				sourceMode: 'specific',
				includeSources: [{'fluxer.display.name': 'mpv'}],
			}),
		).toEqual({kind: 'message', descriptor: ENTIRE_SYSTEM_DESCRIPTOR});
		expect(
			summary({
				shareContext: 'device',
				microphoneLabel: 'Yeti',
				sourceMode: 'specific',
				includeSources: [{'fluxer.display.name': 'mpv'}],
			}),
		).toEqual({kind: 'message', descriptor: MICROPHONE_WITH_DEVICE_DESCRIPTOR, values: {deviceLabel: 'Yeti'}});
	});

	it('counts the picked applications past the first one', () => {
		expect(
			summary({
				sourceMode: 'specific',
				includeSources: [{'application.name': 'mpv'}, {'application.name': 'Firefox'}],
			}),
		).toEqual({kind: 'message', descriptor: APP_COUNT_DESCRIPTOR, values: {length: 2}});
	});

	it('falls back to a custom label for a picked source with no readable name', () => {
		expect(summary({sourceMode: 'specific', includeSources: [{'object.serial': '41'}]})).toEqual({
			kind: 'message',
			descriptor: CUSTOM_SOURCES_DESCRIPTOR,
		});
	});

	it('reads an empty specific selection as the wide scope it actually routes', () => {
		expect(summary({sourceMode: 'specific', includeSources: []})).toEqual({
			kind: 'message',
			descriptor: ENTIRE_SYSTEM_DESCRIPTOR,
		});
		expect(summary({shareContext: 'app', sourceMode: 'specific', includeSources: []})).toEqual({
			kind: 'message',
			descriptor: SHARED_WINDOW_DESCRIPTOR,
		});
		expect(
			summary({shareContext: 'app', windowAudioScope: 'system', sourceMode: 'specific', includeSources: []}),
		).toEqual({kind: 'message', descriptor: ENTIRE_SYSTEM_DESCRIPTOR});
	});

	it('names the microphone on a device share and its device when one is resolved', () => {
		expect(summary({shareContext: 'device', microphoneLabel: 'Yeti'})).toEqual({
			kind: 'message',
			descriptor: MICROPHONE_WITH_DEVICE_DESCRIPTOR,
			values: {deviceLabel: 'Yeti'},
		});
		expect(summary({shareContext: 'device'})).toEqual({kind: 'message', descriptor: MICROPHONE_DESCRIPTOR});
		expect(summary({shareContext: 'device', microphoneLabel: ''})).toEqual({
			kind: 'message',
			descriptor: MICROPHONE_DESCRIPTOR,
		});
	});

	it('keeps a window share on the shared window whatever the stored display selection says', () => {
		for (const sourceMode of ['none', 'system', 'specific'] as const) {
			expect(summary({shareContext: 'app', sourceMode, includeSources: [{'application.name': 'mpv'}]})).toEqual({
				kind: 'message',
				descriptor: SHARED_WINDOW_DESCRIPTOR,
			});
		}
	});

	it('says the shared window on a window share and the entire system once it is widened', () => {
		expect(summary({shareContext: 'app'})).toEqual({kind: 'message', descriptor: SHARED_WINDOW_DESCRIPTOR});
		expect(summary({shareContext: 'app', windowAudioScope: 'window'})).toEqual({
			kind: 'message',
			descriptor: SHARED_WINDOW_DESCRIPTOR,
		});
		expect(summary({shareContext: 'app', windowAudioScope: 'system'})).toEqual({
			kind: 'message',
			descriptor: ENTIRE_SYSTEM_DESCRIPTOR,
		});
	});

	it('never promises a Wayland window share its own window audio', () => {
		expect(
			summary({shareContext: 'app', displayShareEnvironment: 'desktop-wayland', windowAudioScope: 'window'}),
		).toEqual({kind: 'message', descriptor: ENTIRE_SYSTEM_DESCRIPTOR});
	});

	it('says the entire system on a display share whatever the window scope holds', () => {
		for (const windowAudioScope of ['window', 'system'] as const) {
			expect(summary({shareContext: 'display', windowAudioScope})).toEqual({
				kind: 'message',
				descriptor: ENTIRE_SYSTEM_DESCRIPTOR,
			});
		}
	});
});
